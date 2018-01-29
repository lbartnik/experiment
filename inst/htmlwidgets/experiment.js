HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    var shiny = (typeof HTMLWidgets != 'undefined' && HTMLWidgets.shinyMode);

    el = $(el);
    var mainContainer = $("<div>", {class: "main-container"}).appendTo(el);
    var mainContainerEl = mainContainer.get(0);

    var controls = Controls(mainContainerEl, 1, 4);
    var visvis = Widget(mainContainerEl);
    var popup = PopUp(mainContainerEl);

    visvis.setOption('shiny', shiny);
    visvis.setSize(mainContainer.width(), mainContainer.height());
    controls.on('zoom', visvis.zoom);

    // return widget instance
    return {
      renderValue: function(input) {
        visvis.setData(input.data);
        if (shiny && 'options' in input && 'welcome' in input.options) {
  //        popup.show(input.options.welcome);
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
