HTMLWidgets.widget({

  name: "unittest",
  type: "output",

  factory: function(el, width, height) {

    // return widget instance
    return {
      renderValue: function(input) {
        mocha.setup('tdd');
        registerTests();
        mocha.run();
      },

      resize: function(width, height) {
      },
    };
  }
});
