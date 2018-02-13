HTMLWidgets.widget({

  name: "unittest",
  type: "output",

  factory: function(el, width, height) {

    var mochaDiv = $("<div>", {id: "mocha"}).appendTo($(el));
    console.log(mochaDiv);

    // return widget instance
    return {
      renderValue: function(input) {
        mocha.setup('tdd');
        registerTests(input.data);
        mocha.run();
      },

      resize: function(width, height) {
      },
    };
  }
});
