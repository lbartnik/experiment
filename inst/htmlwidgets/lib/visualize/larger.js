"use strict";

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

// Generated by CoffeeScript 2.0.3
(function () {
  var Data,
      Description,
      Position,
      UI,
      Widget,
      euclidean,
      mapNodes,
      plotHref,
      viewport,
      _extends = Object.assign || function (target) {
    for (var i = 1; i < arguments.length; i++) {
      var source = arguments[i];for (var key in source) {
        if (Object.prototype.hasOwnProperty.call(source, key)) {
          target[key] = source[key];
        }
      }
    }return target;
  };

  Array.prototype.unique = function () {
    var j, key, output, ref, ref1, results, value;
    output = {};
    for (key = j = 0, ref = this.length; 0 <= ref ? j < ref : j > ref; key = 0 <= ref ? ++j : --j) {
      output[(ref1 = this[key].id) != null ? ref1 : this[key]] = this[key];
    }
    results = [];
    for (key in output) {
      value = output[key];
      results.push(value);
    }
    return results;
  };

  // Helper function to map node id's to node objects.
  // Returns d3.map of ids -> nodes
  mapNodes = function mapNodes(nodes) {
    var nodesMap;
    nodesMap = d3.map();
    nodes.forEach(function (n) {
      return nodesMap.set(n.id, n);
    });
    return nodesMap;
  };

  euclidean = function euclidean(a, b) {
    return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2));
  };

  viewport = function viewport() {
    var h, w;
    w = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
    h = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    return {
      width: w,
      height: h
    };
  };

  plotHref = function plotHref(id) {
    return $("#plots-" + id + "-attachment").attr("href");
  };

  // add style to notifyjs, just once
  $.notify.addStyle('simplenotification', {
    html: "<div><span data-notify-text/></div>"
  });

  // --- Utils ------------------------------------------------------------
  UI = function UI(selection) {
    var nodeR = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 25;
    var innerR = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 25;

    var canvas, createGraphics, linksG, nodesG, outer, scaleText, ui;
    outer = null;
    canvas = null;
    linksG = null;
    nodesG = null;
    ui = function ui() {};
    ui.initialize = function () {
      outer = d3.select(selection).append("div").attr("class", "widget").style("overflow", "auto").style('overflow-y', 'auto');
      canvas = outer.append("svg");
      linksG = canvas.append("g").attr("id", "links");
      return nodesG = canvas.append("g").attr("id", "nodes");
    };
    ui.setSize = function (width, height) {
      // reduce the size to make sure scrolls don't show right away
      return canvas.attr("width", width - 10).attr("height", height - 10).attr("viewBox", "0 0 " + width + " " + height);
    };
    ui.setData = function (data) {
      return createGraphics(data);
    };
    // create all graphical elements on the canvas
    createGraphics = function createGraphics(data) {
      var enter, link, node;
      node = nodesG.selectAll("svg.variable").data(data.steps, function (d) {
        return d.id;
      });
      enter = node.enter().append("svg").attr("class", function (d) {
        return "variable " + d.type;
      }).attr("id", function (d) {
        return d.id;
      }).attr("viewBox", "0 0 " + 2 * innerR + " " + 2 * innerR).attr("width", 2 * nodeR).attr("height", 2 * nodeR);
      enter.each(function (d) {
        var element, text;
        element = d3.select(this);
        if (d.type === 'object') {
          element.append("rect").attr("width", 2 * innerR).attr("height", 2 * innerR).attr("rx", innerR / 2).attr("ry", innerR / 2);
          text = element.append("text").attr("class", "label").attr("text-anchor", "middle").attr("alignment-baseline", "middle").attr("y", '50%').attr("x", '50%').text(function (d) {
            return d.name;
          });
          text.style('font-size', scaleText(text));
          return element.append("rect").attr("class", "face").attr("width", 2 * innerR).attr("height", 2 * innerR);
        } else {
          if (d.contents) {
            return element.append("image").attr("width", 2 * innerR).attr("height", 2 * innerR).attr("xlink:href", plotHref(d.id));
          } else {
            return element.append("rect").attr('width', 2 * innerR).attr('height', 2 * innerR).style("fill", "grey");
          }
        }
      });
      node.exit().remove();
      link = linksG.selectAll("line.link").data(data.links, function (d) {
        return d.source.id + "_" + d.target.id;
      });
      link.enter().append("line").attr("class", "link").attr("stroke", "#ddd");
      return link.exit().remove();
    };
    // --- createGraphics

    // make sure text fits inside the node icon
    scaleText = function scaleText(text) {
      var fontSize, textWidth;
      textWidth = text.node().getBoundingClientRect().width;
      fontSize = parseFloat(text.style('font-size'));
      fontSize = fontSize * (textWidth / (innerR * 2.2));
      return fontSize + "px";
    };
    ui.updatePositions = function () {
      var link;
      nodesG.selectAll("svg.variable").attr("x", function (d) {
        return d.x - d.scale * nodeR;
      }).attr("y", function (d) {
        return d.y - d.scale * nodeR;
      }).attr("width", function (d) {
        return d.scale * 2 * nodeR;
      }).attr("height", function (d) {
        return d.scale * 2 * nodeR;
      });
      return link = linksG.selectAll("line.link").attr("x1", function (d) {
        return d.source.x;
      }).attr("y1", function (d) {
        return d.source.y;
      }).attr("x2", function (d) {
        return d.target.x;
      }).attr("y2", function (d) {
        return d.target.y;
      });
    };
    ui.mousePosition = function () {
      var x, y;

      var _d3$mouse = d3.mouse(canvas.node());

      var _d3$mouse2 = _slicedToArray(_d3$mouse, 2);

      x = _d3$mouse2[0];
      y = _d3$mouse2[1];

      return {
        x: x,
        y: y
      };
    };
    ui.nodesNear = function (point, distance) {
      var intList, n, parents, rc;
      rc = canvas.node().createSVGRect();
      rc.x = point.x - distance;
      rc.y = point.y - distance;
      rc.width = 2 * distance;
      rc.height = 2 * distance;
      // returns SVG node elements inside the parent svg
      intList = canvas.node().getIntersectionList(rc, nodesG.node());
      parents = function () {
        var j, len, results;
        results = [];
        for (j = 0, len = intList.length; j < len; j++) {
          n = intList[j];
          results.push(n.parentNode);
        }
        return results;
      }().unique();
      return parents.filter(function (n) {
        return euclidean(d3.select(n).datum(), point) <= distance;
      });
    };
    // --- events ---
    ui.on = function (event, callback) {
      // canvas-level events
      if (event === 'canvas:mousemove' || event === 'canvas:mouseout') {
        canvas.on(event.substring(7), callback);
      }
      // node-level events
      if (event === 'node:mouseover' || event === 'node:mouseout' || event === 'node:click') {
        return nodesG.selectAll(".face,image").on(event.substring(5), callback);
      }
    };
    ui.initialize();
    return ui;
  };

  // --- UI ---------------------------------------------------------------
  Data = function Data(data) {
    var resetScale, setupData;
    resetScale = function resetScale() {
      return data.steps.forEach(function (s) {
        return s.scale = 1;
      });
    };
    data = _extends({
      resetScale: resetScale
    }, data);
    // pre-process the input data
    setupData = function setupData() {
      var stepsMap;
      data.resetScale();
      // pre-process nodes
      data.steps.forEach(function (s) {
        if (s.expr.constructor === Array) {
          return s.expr = s.expr.join('\n');
        }
      });
      // replace target/source references in links with actual objects
      stepsMap = mapNodes(data.steps);
      return data.links.forEach(function (l) {
        l.source = stepsMap.get(l.source);
        return l.target = stepsMap.get(l.target);
      });
    };
    // initialize the object
    setupData();
    return data;
  };

  // --- Data -------------------------------------------------------------
  Position = function Position(width, height, margin) {
    var position, stratified, treed;
    width = width - margin * 2;
    height = height - margin * 2;
    position = function position() {};
    stratified = function stratified(data) {
      var parentsMap, stratify;
      parentsMap = d3.map();
      data.links.forEach(function (l) {
        return parentsMap.set(l.target.id, l.source.id);
      });
      stratify = d3.stratify().id(function (d) {
        return d.id;
      }).parentId(function (d) {
        return parentsMap.get(d.id);
      });
      return stratify(data.steps);
    };
    treed = function treed(data) {
      var tree;
      data.sort();
      tree = d3.tree().size([width, height]);
      return tree(data);
    };
    position.calculate = function (data) {
      var dx, dy, max_x, max_y, min_x, min_y, nodesMap, s, t, x, y;
      // use d3 to calculate positions for a tree
      s = stratified(data);
      t = treed(s);
      // centralize the tree
      x = t.descendants().map(function (n) {
        return n.x;
      });
      y = t.descendants().map(function (n) {
        return n.y;
      });
      min_x = x.reduce(function (a, b) {
        return Math.min(a, b);
      });
      max_x = x.reduce(function (a, b) {
        return Math.max(a, b);
      });
      min_y = y.reduce(function (a, b) {
        return Math.min(a, b);
      });
      max_y = y.reduce(function (a, b) {
        return Math.max(a, b);
      });
      dx = width - max_x - min_x + margin;
      dy = height - max_y - min_y + margin;
      // update original nodes' positions
      nodesMap = mapNodes(data.steps);
      return t.each(function (n) {
        s = nodesMap.get(n.id);
        s.x = n.x + dx;
        return s.y = n.y + dy;
      });
    };
    // --- calculate ---

    // return an instance of the Position object
    return position;
  };

  // --- Position ---------------------------------------------------------
  Description = function Description(element, step, outer) {
    var description, position;
    description = function description() {};
    description.show = function () {
      var height, ref, tooltip;
      tooltip = $("<div>").addClass("tooltip").attr("id", "tooltip_" + step.id);
      if (step.type === "object") {
        $("<span>").addClass("name").appendTo(tooltip).text(step.name);
        $("<span>").addClass("description").appendTo(tooltip).text(step.desc);
      } else {
        // 35 for the code
        height = Math.min(300, viewport().height - 65);
        // an image needs to be first loaded, before its dimensions and final
        // position can be calculated
        $("<img>", {
          src: plotHref(step.id),
          height: height
        }).appendTo(tooltip).on('load', function () {
          return position(element, tooltip);
        });
      }
      // add code describing this step
      $("<pre>").appendTo(tooltip).append($("<code>").addClass("R").text(step.expr));
      tooltip.find("pre code").each(function (i, block) {
        return hljs.highlightBlock(block);
      });
      // regular object can be created and positioned right away
      if (step.type === 'object') {
        position(element, tooltip);
      }

      // show
      if ((ref = element.tooltip) != null) {
        ref.remove();
      }
      return element.tooltip = tooltip.fadeTo('fast', 1);
    };
    description.hide = function () {
      var ref;
      return (ref = element.tooltip) != null ? ref.fadeTo('fast', 0, function () {
        var ref1;
        return (ref1 = element.tooltip) != null ? ref1.remove() : void 0;
      }) : void 0;
    };
    position = function position(element, tooltip) {
      var bcr, dx, dy, left, node, top;
      // append the <div> and collect its dimensions to see if it needs to
      // be moved up or to the right
      tooltip.css({
        left: 0,
        top: 0
      }).appendTo(outer);
      bcr = tooltip.get(0).getBoundingClientRect();
      node = element.getBoundingClientRect();
      left = node.left + node.width;
      top = node.top + node.height;
      dx = Math.max(left + bcr.width - viewport().width, 0);
      dy = Math.max(top + bcr.height - viewport().height, 0);
      // place where it can be seen, move if necessary by [dx, dy]
      return tooltip.css({
        visibility: "visible",
        left: left - dx,
        top: top - dy
      });
    };
    return description;
  };

  // --- Widget -----------------------------------------------------------
  Widget = function Widget(selection) {
    var clickNode, data, hideDialog, lenseR, moveLenses, nodeR, pos, resetScale, setEvents, showDialog, ui, updateCanvas, widget;
    nodeR = 15;
    lenseR = 50;
    ui = UI(selection, nodeR, 15);
    pos = Position(500, 500, nodeR);
    data = null;
    widget = function widget() {};
    widget.setData = function (input) {
      data = Data(input);
      ui.setData(data);
      updateCanvas();
      return setEvents();
    };
    widget.setSize = function (width, height) {
      ui.setSize(width, height);
      pos = Position(width, height, nodeR);
      return updateCanvas();
    };
    updateCanvas = function updateCanvas() {
      if (data) {
        pos.calculate(data);
        return ui.updatePositions();
      }
    };
    setEvents = function setEvents() {
      ui.on('canvas:mousemove', moveLenses);
      ui.on('canvas:mouseout', resetScale);
      ui.on('node:mouseover', showDialog);
      ui.on('node:mouseout', hideDialog);
      return ui.on('node:click', clickNode);
    };
    moveLenses = function moveLenses(d) {
      var mouse, nodes;
      data.resetScale();
      mouse = ui.mousePosition();
      nodes = ui.nodesNear(mouse, lenseR);
      nodes.forEach(function (n) {
        var datum, scale;
        datum = d3.select(n).datum();
        scale = euclidean(mouse, datum) / lenseR;
        return datum.scale = 1 + lenseR / nodeR * Math.pow(1 - scale, 2);
      });
      return ui.updatePositions();
    };
    resetScale = function resetScale(d) {
      data.resetScale();
      return ui.updatePositions();
    };
    showDialog = function showDialog(d) {
      this.description = Description(this, d, selection);
      return this.description.show();
    };
    hideDialog = function hideDialog(d) {
      return this.description.hide();
    };
    clickNode = function clickNode(d) {
      var input;
      input = $("<input>").appendTo(selection).val("restore('" + d.id + "')").select();
      document.execCommand("copy");
      input.remove();
      return $.notify("ID copied to clipboard", {
        autoHideDelay: 1000,
        className: 'info',
        style: 'simplenotification'
      });
    };
    return widget;
  };

  // export the Widget
  window.Widget = Widget;
}).call(undefined);