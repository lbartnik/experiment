"use strict";

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// Generated by CoffeeScript 2.0.3
(function () {
  var Data;

  Data = function () {
    // The Data object owns the @raw data passed to it.

    function Data(raw) {
      _classCallCheck(this, Data);

      this.raw = raw;
      this.preprocess();
    }

    // Starting with the root, forms an ordered path through the graph.
    // If there is more than one child at any level, throws an exception.

    _createClass(Data, [{
      key: "sequence",
      value: function sequence() {
        var head, ref, ref1, seq;
        head = this.treed();
        seq = [];
        while (((ref = head.children) != null ? ref.length : void 0) === 1) {
          seq.push(head.data);
          head = head.children[0];
        }
        if (((ref1 = head.children) != null ? ref1.length : void 0) > 1) {
          throw "Node " + head.id + " has more than one child";
        }
        seq.push(head.data);
        return seq;
      }

      // Getters.

    }, {
      key: "steps",
      value: function steps() {
        return this.raw.steps;
      }
    }, {
      key: "links",
      value: function links() {
        return this.raw.links;
      }

      // Replace target/source identifiers in the @raw.links array with
      // actual objects.

    }, {
      key: "preprocess",
      value: function preprocess() {
        var stepsMap;
        stepsMap = d3.map();
        this.raw.steps.forEach(function (n) {
          return stepsMap.set(n.id, n);
        });
        this.raw.links.forEach(function (l) {
          l.source = stepsMap.get(l.source);
          return l.target = stepsMap.get(l.target);
        });
        return this.raw.steps.forEach(function (s) {
          var ref;
          if (((ref = s.expr) != null ? ref.constructor : void 0) === Array) {
            return s.expr = s.expr.join('\n');
          }
        });
      }

      // Turns the @raw data structure into a d3.stratified hierarchy.

    }, {
      key: "stratified",
      value: function stratified() {
        var parents, stratify;
        parents = d3.map();
        this.raw.links.forEach(function (l) {
          return parents.set(l.target.id, l.source.id);
        });
        stratify = d3.stratify().id(function (d) {
          return d.id;
        }).parentId(function (d) {
          return parents.get(d.id);
        });
        return stratify(this.raw.steps);
      }

      // Computes positions in d3.stratified hierarchy assuming nodes
      // form a tree.

    }, {
      key: "treed",
      value: function treed() {
        var spacing = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 25;

        var stfd, tree;
        tree = d3.tree().nodeSize([spacing, spacing]);
        stfd = this.stratified();
        stfd.sort();
        return tree(stfd);
      }
    }]);

    return Data;
  }();

  window.Data = Data;
}).call(undefined);