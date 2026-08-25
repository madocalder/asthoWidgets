# Load required packages
library(colorspace)
library(dplyr)
library(purrr)
library(tidyr)

#' Generate Comprehensive Color Palette Analysis Dataframe with APCA Bronze Level Guidelines
#'
#' @param palette Character vector of hex color codes
#' @param palette_name Optional name for the palette
#' @return A list containing two dataframes:
#'   - color_properties: Detailed properties for each color
#'   - contrast_matrix: W3C and APCA contrast ratios with Bronze Level pass/fail booleans
#'
generate_palette_analysis <- function(palette, names = NULL, palette_name = "custom_palette") {

  # Add white and black for contrast testing
  extended_palette <- c("#FFFFFF", "#000000", palette)
  if(is.null(names)){
  extended_names <- c("white", "black", paste0("color_", seq_along(palette)))
  } else {
  extended_names <- c("white", "black", names)
  }

  # Initialize color properties dataframe
  color_properties <- data.frame(
    palette_name = palette_name,
    color_id = extended_names,
    hex_code = extended_palette,
    stringsAsFactors = FALSE
  )

  # Calculate HCL values
  hcl_vals <- coords(as(hex2RGB(extended_palette), "polarLUV"))
  color_properties$hue <- round(hcl_vals[, "H"], 2)
  color_properties$chroma <- round(hcl_vals[, "C"], 2)
  color_properties$luminance <- round(hcl_vals[, "L"], 2)

  # Calculate CIELAB values
  lab_vals <- coords(as(hex2RGB(extended_palette), "LAB"))
  color_properties$L_star <- round(lab_vals[, "L"], 2)
  color_properties$a_star <- round(lab_vals[, "A"], 2)
  color_properties$b_star <- round(lab_vals[, "B"], 2)

  # Generate CVD simulations
  color_properties$deuteranopia_hex <- deutan(extended_palette)
  color_properties$protanopia_hex <- protan(extended_palette)
  color_properties$tritanopia_hex <- tritan(extended_palette)
  color_properties$desat_50_hex <- desaturate(extended_palette, amount = 0.5)
  color_properties$desat_100_hex <- desaturate(extended_palette, amount = 1)

  # Calculate HCL values for CVD simulations
  for(cvd_type in c("deuteranopia", "protanopia", "tritanopia", "desat_50", "desat_100")) {
    hex_col <- paste0(cvd_type, "_hex")
    cvd_hcl <- coords(as(hex2RGB(color_properties[[hex_col]]), "polarLUV"))
    color_properties[[paste0(cvd_type, "_hue")]] <- round(cvd_hcl[, "H"], 2)
    color_properties[[paste0(cvd_type, "_chroma")]] <- round(cvd_hcl[, "C"], 2)
    color_properties[[paste0(cvd_type, "_luminance")]] <- round(cvd_hcl[, "L"], 2)
  }

  # Generate contrast matrix - STORE BOTH APCA VALUES
  contrast_matrix <- expand.grid(
    color1 = extended_names,
    color2 = extended_names,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      hex1 = extended_palette[match(color1, extended_names)],
      hex2 = extended_palette[match(color2, extended_names)]
    )

  # Calculate W3C contrasts
  contrast_matrix$w3c_contrast <- map2_dbl(
    contrast_matrix$hex1,
    contrast_matrix$hex2,
    ~contrast_ratio(.x, .y, algorithm = "WCAG")
  )

  # Calculate APCA contrasts - store both normal and reverse
  apca_results <- contrast_ratio(
    contrast_matrix$hex1,
    contrast_matrix$hex2,
    algorithm = "APCA"
  )

  # Store both APCA values
  contrast_matrix$apca_normal <- apca_results[, "normal"]
  contrast_matrix$apca_reverse <- apca_results[, "reverse"]

  # Calculate absolute APCA for pass/fail (use the worst case for conservative assessment)
  contrast_matrix$apca_worst_case <- pmin(abs(contrast_matrix$apca_normal),
                                          abs(contrast_matrix$apca_reverse))

  # Add W3C pass/fail booleans
  contrast_matrix <- contrast_matrix %>%
    mutate(
      w3c_pass_AA = w3c_contrast >= 4.5,
      w3c_pass_AAA = w3c_contrast >= 7,
      w3c_pass_large_AA = w3c_contrast >= 3,
      w3c_pass_large_AAA = w3c_contrast >= 4.5
    ) %>%
    # Add APCA Bronze Level pass/fail booleans
    mutate(
      # Bronze Level thresholds
      apca_bronze_Lc90 = apca_worst_case >= 90,  # Preferred fluent text
      apca_bronze_Lc75 = apca_worst_case >= 75,  # Minimum body text
      apca_bronze_Lc60 = apca_worst_case >= 60,  # Minimum content text
      apca_bronze_Lc45 = apca_worst_case >= 45,  # Minimum headlines/large text
      apca_bronze_Lc30 = apca_worst_case >= 30,  # Minimum non-content text
      apca_bronze_Lc15 = apca_worst_case >= 15,  # Minimum discernible non-text

      # AAA equivalents (Bronze Level + 15)
      apca_aaa_Lc90 = apca_worst_case >= 105,    # AAA fluent text
      apca_aaa_Lc75 = apca_worst_case >= 90,     # AAA body text
      apca_aaa_Lc60 = apca_worst_case >= 75,     # AAA content text
      apca_aaa_Lc45 = apca_worst_case >= 60,     # AAA headlines
      apca_aaa_Lc30 = apca_worst_case >= 45,     # AAA non-content text
      apca_aaa_Lc15 = apca_worst_case >= 30      # AAA discernible non-text
    ) %>%
    select(-hex1, -hex2)

  return(list(
    color_properties = color_properties,
    contrast_matrix = contrast_matrix
  ))
}

#' Generate specialized dataframes for specific analyses with APCA Bronze Levels
#'
#' @param analysis_result Output from generate_palette_analysis()
#' @return List of specialized dataframes
#'
generate_specialized_dataframes <- function(analysis_result) {
  color_props <- analysis_result$color_properties
  contrast_mat <- analysis_result$contrast_matrix

  # 1. White/Black contrast only
  white_black_contrast <- contrast_mat %>%
    filter((color1 == "white" & color2 == "black") |
             (color1 == "black" & color2 == "white") |
             (color1 %in% c("white", "black") & !color2 %in% c("white", "black")) |
             (!color1 %in% c("white", "black") & color2 %in% c("white", "black")))

  # 2. Color-to-color contrast only (excluding white/black)
  color_only_contrast <- contrast_mat %>%
    filter(!color1 %in% c("white", "black") & !color2 %in% c("white", "black"))

  # 3. CVD comparison dataframe
  cvd_comparison <- color_props %>%
    select(color_id, hex_code, ends_with("_hex")) %>%
    pivot_longer(
      cols = ends_with("_hex"),
      names_to = "simulation_type",
      values_to = "simulated_hex"
    ) %>%
    mutate(simulation_type = gsub("_hex", "", simulation_type))

  # 4. HCL properties summary
  hcl_summary <- color_props %>%
    select(color_id, hex_code, hue, chroma, luminance, L_star, a_star, b_star)

  # 5. Summary of contrast performance using APCA Bronze Levels
  contrast_summary <- contrast_mat %>%
    group_by(color1) %>%
    summarise(
      # W3C rates
      w3c_AA_pass_rate = mean(w3c_pass_AA, na.rm = TRUE),
      w3c_AAA_pass_rate = mean(w3c_pass_AAA, na.rm = TRUE),

      # APCA Bronze Level rates
      apca_Lc90_rate = mean(apca_bronze_Lc90, na.rm = TRUE),
      apca_Lc75_rate = mean(apca_bronze_Lc75, na.rm = TRUE),
      apca_Lc60_rate = mean(apca_bronze_Lc60, na.rm = TRUE),
      apca_Lc45_rate = mean(apca_bronze_Lc45, na.rm = TRUE),
      apca_Lc30_rate = mean(apca_bronze_Lc30, na.rm = TRUE),
      apca_Lc15_rate = mean(apca_bronze_Lc15, na.rm = TRUE),

      .groups = 'drop'
    )

  return(list(
    white_black_contrast = white_black_contrast,
    color_only_contrast = color_only_contrast,
    cvd_comparison = cvd_comparison,
    hcl_summary = hcl_summary,
    contrast_summary = contrast_summary,
    full_contrast_matrix = contrast_mat,
    full_color_properties = color_props
  ))
}

#' Quick contrast check against white/black background using APCA Bronze Levels
#'
#' @param palette Character vector of hex colors
#' @param background Background color ("white" or "black")
#' @param algorithm Contrast algorithm ("WCAG" or "APCA")
#' @return Dataframe with contrast ratios and appropriate pass/fail booleans
#'
quick_contrast_check <- function(palette, background = "white", algorithm = "WCAG") {
  # Use colorspace's vectorized contrast_ratio directly
  contrasts <- contrast_ratio(palette, background, algorithm = algorithm)

  # Handle APCA matrix output
  if (algorithm == "APCA" && is.matrix(contrasts)) {
    # For APCA, take the absolute value of the normal direction for conservative assessment
    contrasts <- abs(contrasts[, "normal"])
  }

  result <- data.frame(
    color = palette,
    background = background,
    algorithm = algorithm,
    contrast_ratio = contrasts,
    stringsAsFactors = FALSE
  )

  # Add pass/fail booleans based on algorithm
  if (algorithm == "WCAG") {
    result <- result %>%
      mutate(
        pass_AA = contrast_ratio >= 4.5,
        pass_AAA = contrast_ratio >= 7,
        pass_large_AA = contrast_ratio >= 3
      )
  } else { # APCA - use Bronze Level thresholds
    result <- result %>%
      mutate(
        bronze_Lc90 = contrast_ratio >= 90,  # Preferred fluent text
        bronze_Lc75 = contrast_ratio >= 75,  # Minimum body text
        bronze_Lc60 = contrast_ratio >= 60,  # Minimum content text
        bronze_Lc45 = contrast_ratio >= 45,  # Minimum headlines/large text
        bronze_Lc30 = contrast_ratio >= 30,  # Minimum non-content text
        bronze_Lc15 = contrast_ratio >= 15   # Minimum discernible non-text
      )
  }

  return(result)
}

#' Quick accessibility summary using APCA Bronze Levels
#'
#' @param analysis_result Output from generate_palette_analysis()
#' @return Dataframe with overall pass rates for both W3C and APCA Bronze Levels
#'
get_accessibility_summary <- function(analysis_result) {
  contrast_mat <- analysis_result$contrast_matrix

  # Filter to color-to-color pairs only (no white/black)
  color_pairs <- contrast_mat %>%
    filter(!color1 %in% c("white", "black") & !color2 %in% c("white", "black"))

  total_pairs <- nrow(color_pairs)

  summary_df <- data.frame(
    standard = c(
      # W3C standards
      "W3C AA (4.5:1)",
      "W3C AAA (7:1)",
      "W3C Large Text AA (3:1)",
      "W3C Large Text AAA (4.5:1)",

      # APCA Bronze Levels
      "APCA Lc90 (Fluent Text)",
      "APCA Lc75 (Body Text)",
      "APCA Lc60 (Content Text)",
      "APCA Lc45 (Headlines)",
      "APCA Lc30 (Non-content Text)",
      "APCA Lc15 (Discernible Non-text)",

      # APCA AAA equivalents
      "APCA AAA Fluent Text (105+)",
      "APCA AAA Body Text (90+)",
      "APCA AAA Content Text (75+)",
      "APCA AAA Headlines (60+)",
      "APCA AAA Non-content Text (45+)",
      "APCA AAA Discernible Non-text (30+)"
    ),
    passing_pairs = c(
      # W3C
      sum(color_pairs$w3c_pass_AA),
      sum(color_pairs$w3c_pass_AAA),
      sum(color_pairs$w3c_pass_large_AA),
      sum(color_pairs$w3c_pass_large_AAA),

      # APCA Bronze
      sum(color_pairs$apca_bronze_Lc90),
      sum(color_pairs$apca_bronze_Lc75),
      sum(color_pairs$apca_bronze_Lc60),
      sum(color_pairs$apca_bronze_Lc45),
      sum(color_pairs$apca_bronze_Lc30),
      sum(color_pairs$apca_bronze_Lc15),

      # APCA AAA
      sum(color_pairs$apca_aaa_Lc90),
      sum(color_pairs$apca_aaa_Lc75),
      sum(color_pairs$apca_aaa_Lc60),
      sum(color_pairs$apca_aaa_Lc45),
      sum(color_pairs$apca_aaa_Lc30),
      sum(color_pairs$apca_aaa_Lc15)
    ),
    total_pairs = total_pairs,
    pass_rate = c(
      # W3C
      mean(color_pairs$w3c_pass_AA),
      mean(color_pairs$w3c_pass_AAA),
      mean(color_pairs$w3c_pass_large_AA),
      mean(color_pairs$w3c_pass_large_AAA),

      # APCA Bronze
      mean(color_pairs$apca_bronze_Lc90),
      mean(color_pairs$apca_bronze_Lc75),
      mean(color_pairs$apca_bronze_Lc60),
      mean(color_pairs$apca_bronze_Lc45),
      mean(color_pairs$apca_bronze_Lc30),
      mean(color_pairs$apca_bronze_Lc15),

      # APCA AAA
      mean(color_pairs$apca_aaa_Lc90),
      mean(color_pairs$apca_aaa_Lc75),
      mean(color_pairs$apca_aaa_Lc60),
      mean(color_pairs$apca_aaa_Lc45),
      mean(color_pairs$apca_aaa_Lc30),
      mean(color_pairs$apca_aaa_Lc15)
    )
  ) %>%
    mutate(pass_rate = round(pass_rate * 100, 1))

  return(summary_df)
}


# Your palette
initial_palette <- c(
  "#005182", "#c65227", "#6ec4e9", "#8f5f9b", "#47ba83", "#eac049", "#90361f", "#cae6f2", "#8a7f6f", "#242c3d")


sequential_purples <- c( "#e3d5e6", "#c7acce", "#ac84b7", "#8f5f9b", "#6a4275", "#472650")

sequential_greens <- c(  "#d9f3dd", "#9ad1b6", "#5cad95", "#328877", "#176258", "#003f3a" )

sequential_orange_scale <- c("#f9eecd", "#e8ba73", "#d58236", "#b14f25", "#7f2916", "#4d0303" )

sequentual_blues <- c( "#e9f3f8","#cbe9f6","#85c6e5", "#2e7ca7", "#115784","#00355f" )


blues <- c("#cae6f2", "#6ec4e9", "#005182", "#002246")

yellows_oranges <- c( "#eac049","#c65227","#90361f")

others <- c("#8f5f9b", "#47ba83")

neutrals <- c( "#8a7f6f", "#242c3d")


# Color combos to be mindful by CVD
## Deuteranomaly - 2.7% & Deuteranopia - 0.56%

 cvd_deuter <- c("#8f5f9b", "#8a7f6f")

## Protanomaly - 0.66% & Protanopia - 0.59%
 cvd_protan <- c("#c65227", "#8a7f6f")










initial_palette_names <- c(
  "ASTHO Blue",
  "ASTHO Orange",
  "Sky Blue",
  "Plum",
  "Green",
  "Yellow",
  "Brick",
  "Mist",
  "Taupe Grey",
  "Slate"
)




# Generate comprehensive analysis with APCA Bronze Levels
analysis <- generate_palette_analysis(initial_palette, initial_palette_names, "8 Color Categorical Palette")

# Access the dataframes
color_properties <- analysis$color_properties
contrast_matrix <- analysis$contrast_matrix

# Quick contrast checks using the updated function
black_w3c <- quick_contrast_check(initial_palette, "black", "WCAG")
black_apca <- quick_contrast_check(initial_palette, "black", "APCA")

# Generate specialized views
specialized <- generate_specialized_dataframes(analysis)

# Get comprehensive accessibility summary
accessibility_summary <- get_accessibility_summary(analysis)

# # View results
# print("Color Properties:")
# print(head(color_properties))
#
# print("Contrast Matrix Sample (showing APCA columns):")
# print(contrast_matrix %>% select(color1, color2, w3c_contrast, apca_normal, apca_reverse, apca_bronze_Lc75) %>% head(10))
#
# print("APCA Bronze Level Summary:")
# print(accessibility_summary %>% filter(grepl("APCA", standard)))
#
# print("Quick White Background Check (APCA Bronze Levels):")
# print(white_apca)
#
# # Check specific APCA Bronze Level performance
# apca_performance <- contrast_matrix %>%
#   filter(!color1 %in% c("white", "black") & !color2 %in% c("white", "black")) %>%
#   summarise(
#     Lc75_body_text = mean(apca_bronze_Lc75) * 100,
#     Lc60_content_text = mean(apca_bronze_Lc60) * 100,
#     Lc45_headlines = mean(apca_bronze_Lc45) * 100
#   )
#
# print("APCA Bronze Level Performance for Color-to-Color Pairs:")
# print(apca_performance)
#
#
# swatch(initial_palette)
#
#
#
#
# original <- c("#67bff1",
#               "#28655a",
#               "#d0ea76",
#               "#29326a",
#               "#b267c6",
#               "#be3c00",
#               "#ddb71a",
#               "#176cdf",
#               "#df997c",
#               "#2b4d0a")
#
# primary <- c(
#
#
#   "#67bff1",
#   "#1c234c",
#   "#8e50a1",
#   "#28655a",
#   "#d0ea76"
#
# )
#
# cool <- c("#d0ea76",
#           "#51967c",
#           "#67bff1",
#           "#176cdf",
#           "#29326a")
#
swatchplot("initial_palette" = initial_palette, cvd = TRUE)
