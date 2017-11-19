HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    // dependencies: toolkit
    var tooltip = $('<div>slowa</div>').appendTo('body').attr('id', 'tooltip');

    // create our vis object and bind it to the element
    var visvis = new vis.Network(el);

    visvis.on('hoverNode', function (x) {
      tooltip
      .css({
        top: x.pointer.DOM.y,
        left: x.pointer.DOM.x,
        position: 'absolute'
      })
      .show();
    });

    visvis.on('blurNode', function (x) {
      tooltip.hide();
    });

    // return widget instance
    return {
      renderValue: function(x) {
        x.settings.layout = { 'randomSeed': 1 };
        visvis.setOptions(x.settings);
        visvis.setData(x.data);
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
