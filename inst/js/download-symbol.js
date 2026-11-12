(function(H) {
    H.SVGRenderer.prototype.symbols.downloadIcon = function(x, y, w, h) {
        // Original path based on 640x640, scale and offset
        var scaleX = w / 640;
        var scaleY = h / 640;

        // You need to convert your original path coordinates point by point.
        // Below is an example framework; in practice, use R or a script to pre-convert all coordinates
        // Method: newX = x + originalX * scaleX, newY = y + originalY * scaleY

        // To keep it simple, here is an example using simplified path points (you need to replace with the complete converted path):
        return [
            'M', x + 344 * scaleX, y + 88 * scaleY,
            'L', x + 344 * scaleX, y + 334.1 * scaleY,
            // ... other path commands
        ];
    };
}(Highcharts));
