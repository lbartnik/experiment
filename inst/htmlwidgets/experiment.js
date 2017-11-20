function experiment () {
  function impl () {}

  impl.create = function (el) {
    this.tooltip = $('<div>slowa</div>').appendTo($(el).parent()).attr('id', 'tooltip');
    return this;
  };

  impl.show = function (x, y, text) {
      this.tooltip
      .css({
        top: y,
        left: x,
        position: 'absolute'
      })
      .show();
  };

  impl.hide = function () {
    this.tooltip.hide();
  };

  return impl;
}

HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    // dependencies: tooltip
    var tooltip = experiment().create(el);

    // create our vis object and bind it to the element
    var visvis = new vis.Network(el);

    visvis.on('hoverNode', function (e) {
      tooltip.show(e.pointer.DOM.x, e.pointer.DOM.y, 'slowa');
    });

    visvis.on('blurNode', function (x) {
      tooltip.hide();
    });

    visvis.on('hoverEdge', function (e) {
      console.log(e);
      tooltip.show(e.pointer.DOM.x, e.pointer.DOM.y, 'edge');
    });

    visvis.on('blurEdge', function (e) {
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
