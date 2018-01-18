HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    // create our vis object and bind it to the element
    var visvis = new Widget(el);
    visvis.setSize($(el).width(), $(el).height());
    if (typeof HTMLWidgets != 'undefined' && HTMLWidgets.shinyMode) {
      visvis.setOption('shiny', true);
    }

    var popup = PopUp(el, 'tekst');

    // return widget instance
    return {
      renderValue: function(input) {
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
