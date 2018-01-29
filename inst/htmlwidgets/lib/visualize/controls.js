"use strict";

// Generated by CoffeeScript 2.0.3
(function () {
  var Controls;

  Controls = function Controls(selection) {
    var min = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : .5;
    var max = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 2;
    var step = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1.1;

    var callback, controls, current, minus, options, outer, plus, zoom;
    outer = null;
    plus = null;
    minus = null;
    current = 1;
    callback = null;
    options = {
      knitr: false
    };
    controls = function controls() {};
    controls.initialize = function () {
      var zoomer;
      outer = $("<div>", {
        class: "controls"
      }).appendTo(selection);
      plus = $("<div>", {
        class: "button",
        id: "plus"
      }).appendTo(outer).text("+");
      minus = $("<div>", {
        class: "button",
        id: "minus"
      }).appendTo(outer).text("-");
      plus.on('click', function () {
        return zoom(current / step);
      });
      minus.on('click', function () {
        return zoom(current * step);
      });
      zoomer = d3.zoom().scaleExtent([min, max]).on("zoom", function () {
        return zoom(1 / d3.event.transform.k);
      });
      d3.select(selection).select("#plus").call(zoomer).on("dblclick.zoom", null);
      return d3.select(selection).select("#minus").call(zoomer).on("dblclick.zoom", null);
    };
    controls.on = function (event, fn) {
      if (event === 'zoom') {
        return callback = fn;
      }
    };
    zoom = function zoom(k) {
      k = Math.max(min, Math.min(max, k));
      if (k === current) {
        return;
      }
      current = k;
      if (callback) {
        return callback(k);
      }
    };
    controls.initialize();
    return controls;
  };

  window.Controls = Controls;
}).call(undefined);