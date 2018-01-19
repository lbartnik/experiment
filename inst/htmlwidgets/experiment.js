HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    var shiny = (typeof HTMLWidgets != 'undefined' && HTMLWidgets.shinyMode);

    // create our vis object and bind it to the element
    var visvis = new Widget(el);
    var popup = PopUp(el);

    visvis.setSize($(el).width(), $(el).height());
    visvis.setOption('shiny', shiny);

    // return widget instance
    return {
      renderValue: function(input) {
        visvis.setData(input.data);
        if (shiny && 'options' in input && 'welcome' in input.options) {
          popup.show(input.options.welcome);
        }  
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
