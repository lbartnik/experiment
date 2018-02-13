HTMLWidgets.widget({

  name: "unittest",
  type: "output",

  factory: function(el, width, height) {

    var shiny = (typeof HTMLWidgets != 'undefined' && HTMLWidgets.shinyMode);
    var mochaDiv = $("<div>", {id: "mocha"}).appendTo($(el));

    if (shiny) {
      $(document).on('shiny:value', function (e) {
        if (e.name == 'closeWindow') {
          window.close();
        }
      });
    }

    // return widget instance
    return {
      renderValue: function(input) {
        mocha.setup('tdd');
        registerTests(input.data);
        var runner = mocha.run();

        if (!shiny || ('autoClose' in input.options && !input.options.autoClose)) return;

        Shiny.onInputChange(runner.failures > 0 ? 'cancel' : 'done', 'cancelled');
      },

      resize: function(width, height) {
      },
    };
  }
});
