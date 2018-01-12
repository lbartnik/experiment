HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    // create our vis object and bind it to the element
    var visvis = new Widget(el);
    visvis.setSize($(el).width(), $(el).height());

    // return widget instance
    return {
      renderValue: function(input) {
        visvis.setOption('embedPlots', Boolean(input.options.embed_plots))
        visvis.setData(input.data);
      },

      resize: function(width, height) {
        visvis.setSize(width, height);
      },

      // Make the vis object available as a property on the widget
      // instance we're returning from factory().
      v: visvis
    };
  }
});
