'use strict';

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

// Generated by CoffeeScript 2.0.3
(function () {
  var registerTests;

  registerTests = function registerTests(sampleData) {
    var assert, suite, test;
    assert = chai.assert;
    suite = Mocha.suite;
    test = Mocha.test;
    suite('Data', function () {
      var extractScale, unique;
      // helpers
      extractScale = function extractScale(steps) {
        var i, len, results, step;
        results = [];
        for (i = 0, len = steps.length; i < len; i++) {
          step = steps[i];
          if (step.scale !== void 0) {
            results.push(step.scale);
          }
        }
        return results;
      };
      unique = function unique(a) {
        var i, key, output, ref, ref1, results, value;
        output = {};
        for (key = i = 0, ref = a.length; 0 <= ref ? i < ref : i > ref; key = 0 <= ref ? ++i : --i) {
          output[(ref1 = a[key].id) != null ? ref1 : a[key]] = a[key];
        }
        results = [];
        for (key in output) {
          value = output[key];
          results.push(value);
        }
        return results;
      };
      // test cases
      return suite('Data construction', function () {
        // sample data
        setup(function () {
          return this.data = JSON.parse(JSON.stringify(sampleData));
        });
        test('data sanity', function () {
          var step;
          assert('links' in this.data);
          assert('steps' in this.data);
          assert.lengthOf(this.data.steps, 4);
          assert.lengthOf(extractScale(this.data.steps), 0);
          assert.sameMembers(unique(function () {
            var i, len, ref, results;
            ref = this.data.steps;
            results = [];
            for (i = 0, len = ref.length; i < len; i++) {
              step = ref[i];
              results.push(step.id !== void 0);
            }
            return results;
          }.call(this)), [true]);
          return assert.sameMembers(unique(function () {
            var i, len, ref, results;
            ref = this.data.steps;
            results = [];
            for (i = 0, len = ref.length; i < len; i++) {
              step = ref[i];
              results.push(_typeof(step.expr));
            }
            return results;
          }.call(this)), ['object', 'string']);
        });
        test('maintains keys', function () {
          var sd;
          sd = Data(this.data);
          assert('steps' in sd);
          return assert('links' in sd);
        });
        test('sets scale', function () {
          var sd;
          sd = Data(this.data);
          assert.lengthOf(sd.steps, this.data.steps.length);
          assert.lengthOf(extractScale(sd.steps), this.data.steps.length);
          return assert.sameMembers(unique(extractScale(sd.steps)), [1]);
        });
        test('concat expression', function () {
          var sd, step;
          sd = Data(this.data);
          return assert.sameMembers(unique(function () {
            var i, len, ref, results;
            ref = sd.steps;
            results = [];
            for (i = 0, len = ref.length; i < len; i++) {
              step = ref[i];
              results.push(_typeof(step.expr));
            }
            return results;
          }()), ['string']);
        });
        return test('replace id with object', function () {
          var sd;
          sd = Data(this.data);
          return sd.links.forEach(function (link) {
            assert.hasAllKeys(link, ['target', 'source']);
            return assert.includeDeepMembers(sd.steps, [link.target]);
          });
        });
      });
    });
    return null;
  };

  window.registerTests = registerTests;
}).call(undefined);