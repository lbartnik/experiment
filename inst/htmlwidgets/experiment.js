HTMLWidgets.widget({

  name: "experiment",

  type: "output",

  factory: function(el, width, height) {

    // create our vis object and bind it to the element
    var visvis = new Widget(el);

    // return widget instance
    return {
      renderValue: function(id) {
        $.ajax({url: $("#" + id + "-data-attachment").attr("href"),
                dataType: 'json'}).done(function (data) {
          console.log(data);
          visvis.setData(data);
        });
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
