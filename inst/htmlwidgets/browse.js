HTMLWidgets.widget({

    name: "browse",
    type: "output",

    factory: function (el, width, height) {

        var shiny = utils.isShiny();
        var widget = new HistoryBrowser(el);

        return {
            renderValue: function (input) {
                widget.setData(input.data);
            },
            resize: function (width, height) {
                widget.setSize(width, height);
            },
            w: widget
        };
    }
});
