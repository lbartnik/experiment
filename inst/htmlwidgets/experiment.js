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

    visvis.setOption('shiny', shiny);
    visvis.setSize(mainContainer.width(), mainContainer.height());
    controls.on('zoom',      visvis.zoom);
    controls.on('search',    visvis.search)
    controls.on('key:up',    visvis.selectParent);
    controls.on('key:down',  visvis.selectChild);
    controls.on('key:left',  visvis.selectSibling);
    controls.on('key:right', visvis.selectSibling);
    controls.on('key:enter', visvis.confirmSelection);

    // return widget instance
    return {
      renderValue: function(input) {
        if ('knitr' in input.options) {
          visvis.setOption('knitr', input.options.knitr)
        }
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
