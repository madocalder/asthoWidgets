// Extra marker symbols so a 6th+ line gets a distinct shape, not a
// repeat of the built-in five (circle/square/diamond/triangle/triangle-down).
(function () {
  if (typeof Highcharts === "undefined" || Highcharts._awSymbolsRegistered) {
    return;
  }
  var symbols = Highcharts.SVGRenderer.prototype.symbols;

  function polygon(x, y, w, h, sides, rotation) {
    var cx = x + w / 2, cy = y + h / 2, rx = w / 2, ry = h / 2, path = [];
    for (var i = 0; i < sides; i++) {
      var a = rotation + (2 * Math.PI * i) / sides;
      path.push(i === 0 ? "M" : "L", cx + rx * Math.cos(a), cy + ry * Math.sin(a));
    }
    path.push("Z");
    return path;
  }

  symbols.pentagon = function (x, y, w, h) {
    return polygon(x, y, w, h, 5, -Math.PI / 2);
  };
  symbols.hexagon = function (x, y, w, h) {
    return polygon(x, y, w, h, 6, -Math.PI / 2);
  };
  symbols.star = function (x, y, w, h) {
    var cx = x + w / 2, cy = y + h / 2, ro = w / 2, ri = w / 4, n = 5, path = [];
    for (var i = 0; i < 2 * n; i++) {
      var r = i % 2 === 0 ? ro : ri;
      var a = -Math.PI / 2 + (Math.PI * i) / n;
      path.push(i === 0 ? "M" : "L", cx + r * Math.cos(a), cy + r * Math.sin(a));
    }
    path.push("Z");
    return path;
  };

  Highcharts._awSymbolsRegistered = true;
})();
