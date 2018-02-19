"use strict";

// Generated by CoffeeScript 2.0.3
(function () {
  var Controls;

  Controls = function Controls(selection) {
    var min = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : .5;
    var max = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 2;
    var step = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1.1;

    var callbacks, controls, current, keyDown, keys, minus, outer, plus, search, translateKey, zoom;
    outer = null;
    plus = null;
    minus = null;
    search = null;
    current = 1;
    callbacks = {
      zoom: null,
      search: null
    };
    keys = {
      enter: null,
      up: null,
      down: null,
      left: null,
      right: null
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
      search = $("<input>", {
        id: "search",
        class: "search",
        type: "text"
      }).appendTo(outer);
      plus.on('click', function () {
        return zoom(current / step);
      });
      minus.on('click', function () {
        return zoom(current * step);
      });
      search.on('keyup', function (e) {
        console.log(this.value);
        return e.stopPropagation();
      });
      zoomer = d3.zoom().scaleExtent([min, max]).on("zoom", function () {
        return zoom(d3.event.transform.k);
      });
      d3.select(selection).select("#plus").call(zoomer).on("dblclick.zoom", null);
      d3.select(selection).select("#minus").call(zoomer).on("dblclick.zoom", null);
      $(window).on('keydown', keyDown);
      return $('iframe', parent.document).on('keydown', keyDown);
    };
    // --- configure events -----------------------------------------------
    controls.on = function (event, fn) {
      if (event === 'zoom' || event === 'search') {
        callbacks[event] = fn;
      }
      if (event.substring(0, 3) === 'key') {
        return keys[event.substring(4)] = fn;
      }
    };
    // --- zooming --------------------------------------------------------
    zoom = function zoom(k) {
      current = Math.max(min, Math.min(max, k));
      return typeof callbacks.zoom === "function" ? callbacks.zoom(current) : void 0;
    };
    // --- keyboard -------------------------------------------------------
    keyDown = function keyDown(e) {
      var key;
      key = translateKey(e);
      if (key in keys && keys[key]) {
        keys[key](key);
        return e.preventDefault();
      }
    };
    // --- translate key to its name ---
    translateKey = function translateKey(e) {
      var Codes, keyCode, ref;
      Codes = {
        13: "enter",
        37: "left",
        38: "up",
        39: "right",
        40: "down"
      };
      keyCode = (ref = e.originalEvent) != null ? ref.keyCode : void 0;
      if (keyCode in Codes) {
        return Codes[keyCode];
      }
      return null;
    };
    // --------------------------------------------------------------------
    controls.initialize();
    return controls;
  };

  window.Controls = Controls;
}).call(undefined);