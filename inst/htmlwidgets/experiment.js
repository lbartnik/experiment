HTMLWidgets.widget({

  name: "experiment",
  
  type: "output",
  
  factory: function(el, width, height) {
  
    // create our vis object and bind it to the element
    var visvis = new vis.Network(el);
    
    // return widget instance
    return {
      renderValue: function(x) {
        visvis.setOptions(x.settings);
        visvis.setData(x.data.nodes, x.data.edges);
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
