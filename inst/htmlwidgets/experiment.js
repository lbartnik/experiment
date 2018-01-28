HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    var shiny = (typeof HTMLWidgets != 'undefined' && HTMLWidgets.shinyMode);

    // create our vis object and bind it to the element
    var controls = null;
    if (shiny) {
      controls = Controls(el, 1, 4);
    }

    var visvis = Widget(el);
    var popup = PopUp(el);

    visvis.setSize($(el).width(), $(el).height());
    visvis.setOption('shiny', shiny);
    if (controls) {
      controls.on('zoom', visvis.zoom);
    }

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
