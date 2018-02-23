"use strict";

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

// Generated by CoffeeScript 2.0.3
(function () {
  var Data,
      Details,
      FloatingDescription,
      Log,
      Position,
      UI,
      Viewport,
      Widget,
      euclidean,
      log,
      mapNodes,
      plotHref,
      sign,
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

  Array.prototype.min = function () {
    return this.reduce(function (a, b) {
      return Math.min(a, b);
    });
  };

  Array.prototype.max = function () {
    return this.reduce(function (a, b) {
      return Math.max(a, b);
    });
  };

  Array.prototype.remove = function (e) {
    return this.filter(function (el) {
      return el !== e;
    });
  };

  if (Math.sign === void 0) {
    sign = function sign(x) {
      if (x < 0) {
        return -1;
      } else {
        return 1;
      }
    };
  } else {
    sign = Math.sign;
  }

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

  Viewport = function Viewport(selection) {
    var viewport;
    viewport = function viewport() {};
    viewport.size = function () {
      var h, w;
      // actual viewport; in R Studio, when running as AddIn, this is somehow
      // distorted and reports size larger than the actual viewport area
      w = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
      h = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
      // thus, we compare it with the size of the enclosing HTML element and
      // choose whatever is smaller
      w = Math.min(w, $(selection).width());
      h = Math.min(h, $(selection).height());
      return {
        width: w,
        height: h
      };
    };
    return viewport;
  };

  // returns:
  //   - the embedded image, if contents present
  //   - the image from link, if can be found
  //   - a grey 30x30 png, if nothing else works
  plotHref = function plotHref(step) {
    var from_id;
    if (step.contents) {
      return "data:image/png;base64," + step.contents;
    }
    from_id = $("#plots-" + step.id + "-attachment").attr("href");
    if (from_id) {
      return from_id;
    }
    return "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAIAAAC0Ujn1AAAACXBIWXMAAAsTAAALEwEAmpwY\nAAAAB3RJTUUH4gEMEg8VFQkJGwAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJ\nTVBkLmUHAAAAKUlEQVRIx+3MMREAAAgEILV/mI9oChcPAtBJ6sbUGbVarVar1Wr1/3oBRm8C\nTEfLR0EAAAAASUVORK5CYII=";
  };

  // --- simple logger ----------------------------------------------------
  Log = function Log() {
    var callerName, enabled, log, showMessage;
    enabled = false;
    callerName = function callerName() {
      var re, res, st;
      re = /([^(]+)@|at ([^(]+) \(/gm;
      st = new Error().stack;
      re.exec(st); // skip 1st line
      re.exec(st); // skip 2nd line
      re.exec(st); // skip 3rd line
      res = re.exec(st);
      if (!res) {
        return "unknown";
      }
      return res[1] || res[2];
    };
    showMessage = function showMessage(level, message) {
      var caller;
      if (!enabled) {
        return;
      }
      caller = callerName();
      return console.log(level + " " + caller + ": " + message);
    };
    log = function log() {};
    log.debug = function (message) {
      return showMessage("DEBUG", message);
    };
    log.info = function (message) {
      return showMessage("INFO ", message);
    };
    log.enable = function (onoff) {
      return enabled = onoff;
    };
    return log;
  };

  log = Log();

  // --- Utils ------------------------------------------------------------
  UI = function UI(selection) {
    var nodeR = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 25;
    var innerR = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 25;

    var canvas, createGraphics, data, hideNames, linksG, namesG, nodesG, outer, points, recalculateCanvas, resetCanvasSize, scaleText, showNames, sizes, switchView, ui, zoom;
    outer = null;
    canvas = null;
    linksG = null;
    nodesG = null;
    namesG = null;
    sizes = {
      ui: {
        width: 500,
        height: 500
      },
      canvas: {
        width: 500,
        height: 500
      }
    };
    data = null;
    zoom = {
      current: 1,
      switch: 1.5
    };
    ui = function ui() {};
    ui.initialize = function () {
      outer = d3.select(selection).append("div").attr("class", "widget");
      canvas = outer.append("svg");
      linksG = canvas.append("g").attr("id", "links");
      nodesG = canvas.append("g").attr("id", "nodes");
      return namesG = canvas.append("g").attr("id", "names");
    };
    ui.setSize = function (width, height) {
      var jOuter;
      sizes.ui.width = width;
      sizes.ui.height = height;
      jOuter = $(outer.node());
      return jOuter.css({
        width: width,
        height: height - parseInt(jOuter.css("top"), 10)
      });
    };
    ui.setData = function (Data) {
      data = Data;
      recalculateCanvas(data);
      resetCanvasSize();
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
        return "node_" + d.id;
      }).attr("viewBox", "0 0 " + 2 * innerR + " " + 2 * innerR).attr("width", 2 * nodeR).attr("height", 2 * nodeR);
      enter.each(function (d) {
        var element, text;
        element = d3.select(this);
        if (d.type === 'object') {
          element.append("circle").attr("cx", innerR).attr("cy", innerR).attr("r", innerR * .96);
          text = element.append("text").attr("class", "label").attr("text-anchor", "middle").attr("alignment-baseline", "middle").attr("y", '50%').attr("x", '50%').text(function (d) {
            return d.name;
          });
          text.style('font-size', scaleText(text));
          return element.append("rect").attr("class", "face").attr("width", 2 * innerR).attr("height", 2 * innerR); // type == plot
        } else {
          element.append("image").attr("xlink:href", plotHref(d));
          element.append("rect");
          element.append("rect").classed("face", true);
          return element.selectAll("image,rect").attr("width", 2 * innerR).attr("height", 2 * innerR);
        }
      });
      node.exit().remove();
      link = linksG.selectAll("line.link").data(data.links, function (d) {
        return "link_" + d.source.id + "_" + d.target.id;
      });
      link.enter().append("line").attr("class", "link").attr("stroke", "#ddd");
      return link.exit().remove();
    };
    // --- createGraphics

    // switch view between zoom-out and close-up
    switchView = function switchView(which) {
      if (which === "zoom-out") {
        nodesG.selectAll(".variable").interrupt("show-nodes").transition("hide-nodes").duration(500).style("opacity", "0").on("end", function (d) {
          return d3.select(this).style("visibility", "hidden");
        });
        linksG.selectAll("line.link").classed("thick", function (d) {
          return d.source.group === d.target.group;
        }).style("opacity", "0").transition().duration(500).style("opacity", "1");
        return linksG.selectAll("line.thick").on("mouseover", showNames).on("mouseout", hideNames); // close-up
      } else {
        nodesG.selectAll(".variable").interrupt("hide-nodes").style("visibility", "visible").transition("show-nodes").duration(500).style("opacity", "1");
        return linksG.selectAll("line.link").classed("thick", function (d) {
          return false;
        });
      }
    };
    // --- switchView

    // create a polygon that follows rectangle `rect` and has its
    // fifth vertex in point `point`
    points = function points(point, rect) {
      var bottom, left, right, top, x, y;
      x = point.x;
      y = point.y;
      left = rect.x;
      top = rect.y;
      right = rect.x + rect.width + 2 * zoom.current;
      bottom = rect.y + rect.height;
      return x + "," + y + " " + left + "," + top + " " + right + "," + top + " " + right + "," + bottom + " " + left + "," + bottom;
    };
    // show node names for a same-time group of nodes in the zoom-out mode
    showNames = function showNames(d) {
      var names, polys, shift, subSteps;
      shift = 10 * zoom.current;
      subSteps = data.steps.filter(function (step) {
        return step.group === d.source.group && step.type === "object";
      });
      names = namesG.selectAll("g").data(subSteps, function (d) {
        return "name_" + d.id;
      }).enter().append("g");
      polys = names.append("polygon").classed("bg", true);
      names.append("text").text(function (d) {
        if (d.type === "object") {
          return d.name;
        } else {
          return "plot";
        }
      }).attr("font-size", 12 * zoom.current).attr("class", function (d) {
        return d.type;
      }).attr("x", function (d) {
        return d.x + shift;
      }).attr("y", function (d) {
        return d.y;
      });
      namesG.selectAll("text").each(function (d, i) {
        return d.bb = this.getBBox();
      });
      return polys.attr("points", function (d) {
        return points({
          x: d.x + 10,
          y: d.y
        }, d.bb);
      });
    };
    hideNames = function hideNames(d) {
      return namesG.selectAll("g").remove();
    };
    // make sure text fits inside the node icon
    scaleText = function scaleText(text) {
      var fontSize, textWidth;
      textWidth = text.node().getBoundingClientRect().width;
      fontSize = parseFloat(text.style('font-size'));
      fontSize = Math.min(12, fontSize * (innerR * 1.6 / textWidth));
      return fontSize + "px";
    };
    ui.updateGraphicalElements = function () {
      var link;
      nodesG.selectAll("svg.variable").attr("x", function (d) {
        return d.x - d.scale * nodeR;
      }).attr("y", function (d) {
        return d.y - d.scale * nodeR;
      }).attr("width", function (d) {
        return d.scale * 2 * nodeR;
      }).attr("height", function (d) {
        return d.scale * 2 * nodeR;
      }).classed("selected", function (d) {
        return d.selected;
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
    // compute canvas size from data
    recalculateCanvas = function recalculateCanvas(data) {
      var step, x, xMax, xMin, y, yMax, yMin;
      x = function () {
        var j, len, ref, results;
        ref = data.steps;
        results = [];
        for (j = 0, len = ref.length; j < len; j++) {
          step = ref[j];
          results.push(step.x);
        }
        return results;
      }();
      y = function () {
        var j, len, ref, results;
        ref = data.steps;
        results = [];
        for (j = 0, len = ref.length; j < len; j++) {
          step = ref[j];
          results.push(step.y);
        }
        return results;
      }();
      // calculate marginal positions
      xMin = x.min() - nodeR;
      xMax = x.max() + nodeR;
      yMin = y.min() - nodeR;
      yMax = y.max() + nodeR;
      // if something went wrong, ignore altogether
      if (isNaN(xMin) || isNaN(xMax) || isNaN(yMin) || isNaN(yMax)) {
        return;
      }
      // new canvas dimensions
      sizes.canvas.width = Math.max(sizes.ui.width, xMax - xMin);
      sizes.canvas.height = Math.max(sizes.ui.height, yMax - yMin);
      // sizes.canvas.* are updated and we can update nodes' coordinates
      return data.center(sizes.canvas.width, sizes.canvas.height);
    };
    // canvas size is set independently, and canvas might need to be
    // scrolled within the outer div element
    resetCanvasSize = function resetCanvasSize() {
      // update graphical elements
      return canvas.attr("width", sizes.canvas.width / zoom.current).attr("height", sizes.canvas.height / zoom.current).attr("viewBox", "0 0 " + sizes.canvas.width + " " + sizes.canvas.height);
    };
    // returns mouse position relatively to the SVG canvas
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
        return nodesG.selectAll(".face").on(event.substring(5), callback);
      }
    };

    // --- zooming ---
    ui.zoom = function (k) {
      var ref, ref1;
      if (k < (ref = zoom.switch) && ref < zoom.current) {
        switchView("close-up");
      }
      if (k >= (ref1 = zoom.switch) && ref1 > zoom.current) {
        switchView("zoom-out");
      }
      zoom.current = k;
      return resetCanvasSize();
    };
    // --- scroll to selected node ---
    ui.scrollTo = function (id) {
      var queue = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : true;

      var node, scroll;
      node = nodesG.select("#node_" + id);
      scroll = {
        scrollTop: Math.max(0, node.attr("y") / zoom.current - sizes.ui.height / 2 + nodeR),
        scrollLeft: Math.max(0, node.attr("x") / zoom.current - sizes.ui.width / 2 + nodeR)
      };
      return $(outer.node()).animate(scroll, {
        queue: queue
      });
    };
    // --- trigger click event ---
    ui.clickOn = function (id) {
      log.debug(id);
      return nodesG.selectAll("#node_" + id + " .face").dispatch("click");
    };
    ui.initialize();
    return ui;
  };

  // --- UI ---------------------------------------------------------------
  Data = function Data(data) {
    var counter, dataObject, findRoot, head, setupData;
    dataObject = function dataObject() {};
    head = {
      id: 'head',
      expr: ""
    };
    // pre-process the input data
    setupData = function setupData() {
      var root, stepsMap;
      // add head to the main data set
      root = findRoot(data);
      data.steps.push(head);
      data.links.push({
        source: head.id,
        target: root.id
      });
      // copy over to dataObject
      dataObject.steps = data.steps;
      dataObject.links = data.links;
      // make sure each node has the scale attribute
      dataObject.resetScale();
      // pre-process nodes
      dataObject.steps.forEach(function (s) {
        var ref;
        if (((ref = s.expr) != null ? ref.constructor : void 0) === Array) {
          return s.expr = s.expr.join('\n');
        }
      });
      // replace target/source references in links with actual objects
      stepsMap = mapNodes(dataObject.steps);
      return dataObject.links.forEach(function (l) {
        l.source = stepsMap.get(l.source);
        return l.target = stepsMap.get(l.target);
      });
    };
    findRoot = function findRoot(data) {
      var linksSet;
      linksSet = d3.set();
      data.links.forEach(function (l) {
        return linksSet.add(l.target);
      });
      return data.steps.filter(function (s) {
        return !linksSet.has(s.id);
      })[0];
    };
    dataObject.resetScale = function () {
      return dataObject.steps.forEach(function (s) {
        return s.scale = 1;
      });
    };
    dataObject.stratified = function () {
      var parentsMap, stratify;
      parentsMap = d3.map();
      dataObject.links.forEach(function (l) {
        return parentsMap.set(l.target.id, l.source.id);
      });
      stratify = d3.stratify().id(function (d) {
        return d.id;
      }).parentId(function (d) {
        return parentsMap.get(d.id);
      });
      return stratify(dataObject.steps);
    };
    dataObject.center = function (width, height) {
      var dx, dy, step, x, y;
      x = function () {
        var j, len, ref, results;
        ref = dataObject.steps;
        results = [];
        for (j = 0, len = ref.length; j < len; j++) {
          step = ref[j];
          results.push(step.x);
        }
        return results;
      }();
      y = function () {
        var j, len, ref, results;
        ref = dataObject.steps;
        results = [];
        for (j = 0, len = ref.length; j < len; j++) {
          step = ref[j];
          results.push(step.y);
        }
        return results;
      }();
      dx = x.min() - Math.max(width - (x.max() - x.min()), 0) / 2;
      dy = y.min() - Math.max(height - (y.max() - y.min()), 0) / 2;
      return dataObject.steps.forEach(function (s) {
        s.x -= dx;
        return s.y -= dy;
      });
    };
    counter = function counter() {
      var start = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 1;

      return function () {
        return start++;
      };
    };
    // assign nodes to groups based on the time threshold
    dataObject.groupData = function (threshold) {
      var _assignGroup, groupNo, s, stepsMap;
      s = dataObject.stratified();
      groupNo = counter();
      s.group = groupNo();
      _assignGroup = function assignGroup(parent) {
        var ref;
        return (ref = parent.children) != null ? ref.forEach(function (child) {
          var dt;
          dt = child.data.time - parent.data.time;
          child.group = dt <= threshold ? parent.group : groupNo();
          return _assignGroup(child);
        }) : void 0;
      };
      _assignGroup(s);
      stepsMap = d3.map();
      dataObject.steps.forEach(function (s) {
        return stepsMap.set(s.id, s);
      });
      return s.descendants().forEach(function (d) {
        return stepsMap.get(d.id).group = d.group;
      });
    };
    // --- set node that is selected ---
    dataObject.selected = function (id) {
      return dataObject.steps.forEach(function (step) {
        return step.selected = step.id === id;
      });
    };
    dataObject.parentOf = function (id) {
      var parent;
      parent = dataObject.links.filter(function (link) {
        return link.target.id === id;
      });
      if (!parent.length) {
        return null;
      }
      return parent[0].source.id;
    };
    dataObject.childrenOf = function (id) {
      var children;
      children = dataObject.links.filter(function (link) {
        return link.source.id === id;
      });
      if (!children.length) {
        return [];
      }
      return children.sort(function (a, b) {
        return a.x < b.x;
      }).map(function (link) {
        return link.target.id;
      });
    };
    // --- filtering ---
    dataObject.filter = function (phrase) {
      var links, steps;
      // a copy of links
      links = data.links.map(function (l) {
        return _extends({}, l);
      });
      steps = data.steps.slice(0);
      dataObject.steps = data.steps.filter(function (step) {
        var childrenLinks, parentLink;
        if (step === head) {
          return true;
        }
        if (!step.name || step.name.match(phrase)) {
          return true;
        }
        parentLink = links.filter(function (l) {
          return l.target === step;
        })[0];
        childrenLinks = links.filter(function (l) {
          return l.source === step;
        });
        childrenLinks.forEach(function (l) {
          return l.source = parentLink.source;
        });
        links = links.remove(parentLink);
        return false;
      });
      return dataObject.links = links;
    };
    // initialize the object
    setupData();
    return dataObject;
  };

  // --- Data -------------------------------------------------------------
  Position = function Position(nodeR) {
    var position, treed;
    position = function position() {};
    treed = function treed(data) {
      var tree;
      data.sort();
      tree = d3.tree().nodeSize([4 * nodeR, 4 * nodeR]);
      return tree(data);
    };
    position.calculate = function (data) {
      var dx, dy, nodesMap, s, t;
      // use d3 to calculate positions for a tree
      s = data.stratified();
      t = treed(s);
      dx = t.descendants().map(function (n) {
        return n.x;
      }).min() - 2 * nodeR;
      dy = t.descendants().map(function (n) {
        return n.y;
      }).min() - 2 * nodeR;
      // update original nodes' positions
      nodesMap = mapNodes(data.steps);
      return t.each(function (n) {
        s = nodesMap.get(n.id);
        s.x = n.x - dx;
        return s.y = n.y - dy;
      });
    };
    // --- calculate ---

    // return an instance of the Position object
    return position;
  };

  // --- Position ---------------------------------------------------------
  FloatingDescription = function FloatingDescription(element, step, outer, viewport, nodeR) {
    var asArea, compare, description, position;
    description = function description() {};
    description.show = function () {
      var height, ref, tooltip;
      tooltip = $("<div>").addClass("tooltip").attr("id", "tooltip_" + step.id);
      if (step.type === "object") {
        $("<span>").addClass("name").appendTo(tooltip).text(step.name);
        $("<span>").addClass("description").appendTo(tooltip).text(step.desc);
      } else {
        // 35 for the code
        height = Math.min(300, viewport.size().height - 65);
        // an image needs to be first loaded, before its dimensions and final
        // position can be calculated
        $("<img>", {
          src: plotHref(step),
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
    // area: potential placement of the tooltip
    // box:  dimensions of the tooltip
    // center: where is the node we want to be close to
    // returns: {left, top, scale}
    compare = function compare(area, box, center) {
      var dx, dy, height, left, scale, scalex, scaley, top, width;
      scalex = (area.right - area.left) / box.width;
      scaley = (area.bottom - area.top) / box.height;
      scale = Math.min(scalex, scaley, 1);
      width = box.width * scale;
      height = box.height * scale;
      left = area.left + (area.right - area.left - width) / 2;
      top = area.top + (area.bottom - area.top - height) / 2;
      // direction towards the node
      dx = center.x - (left + width / 2);
      dy = center.y - (top + height / 2);
      dx = sign(dx) * Math.max(Math.abs(dx) - nodeR - width / 2, 0);
      dy = sign(dy) * Math.max(Math.abs(dy) - nodeR - height / 2, 0);
      return {
        left: left + dx,
        top: top + dy,
        scale: scale
      };
    };
    asArea = function asArea(numbers) {
      return {
        left: numbers[0],
        right: numbers[1],
        top: numbers[2],
        bottom: numbers[3]
      };
    };
    position = function position(element, tooltip) {
      var bottom, box, center, choice, left, node, right, top, view;
      // append the <div> and collect its dimensions to see if it needs to
      // be moved up or to the right
      tooltip.css({
        left: 0,
        top: 0
      }).appendTo(outer);
      box = tooltip.get(0).getBoundingClientRect();
      node = element.getBoundingClientRect();
      view = viewport.size();
      center = {
        x: node.left + node.width / 2,
        y: node.top + node.height / 2
      };
      left = compare(asArea([0, node.left, 0, view.height]), box, center);
      right = compare(asArea([node.right, view.width, 0, view.height]), box, center);
      top = compare(asArea([0, view.width, 0, node.top]), box, center);
      bottom = compare(asArea([0, view.width, node.bottom, view.height]), box, center);
      choice = [left, right, top, bottom].reduce(function (a, b) {
        if (a.scale > b.scale) {
          return a;
        } else {
          return b;
        }
      });
      left = choice.left;
      top = choice.top;
      tooltip.css({
        left: left,
        top: top,
        transform: "scale(" + choice.scale + ")"
      });
      // when running as R Studio AddIn, viewport gets messed up, so here
      // we perform one more adjustment: if the actual BCR is moved according
      // top the requested (left, top) we move it again by the difference, in
      // the hope that this will finally place it withing the visible viewport
      box = tooltip.get(0).getBoundingClientRect();
      left += left - box.left;
      top += top - box.top;
      return tooltip.css({
        visibility: "visible",
        left: left,
        top: top
      });
    };
    return description;
  };

  // --- FloatingDescription ---

  // --- Details ----------------------------------------------------------
  Details = function Details(selection, data, id, width, height, commentCallback) {
    var details, initialize, outer, step;
    step = data.steps.filter(function (step) {
      return step.id === id;
    })[0];
    outer = $("<div>", {
      id: "details-container"
    }).load($("#visualize-1-attachment").attr("href"), null, function () {
      return initialize();
    });
    initialize = function initialize() {
      var comment, initial;
      outer.width(width).appendTo(selection).height(height - parseInt(outer.css("top")));
      if (step.type === "object") {
        outer.find(".image").remove();
        outer.find(".name").text(step.name);
        outer.find(".description").text(step.desc);
      } else {
        outer.find(".object").remove();
        outer.find(".image img").attr("src", plotHref(step)).on('load', function () {
          return $(this).width(Math.min(width, this.width));
        });
      }
      // add code describing this step
      outer.find("code").text(step.expr).each(function (i, block) {
        return hljs.highlightBlock(block);
      });
      // handle comment
      comment = outer.find(".comment");
      comment.on('keydown', function (e) {
        return e.stopPropagation();
      }).on('keyup', function (e) {
        var cb;
        step.comment = this.value;
        clearTimeout(this.commentUpdate);
        cb = function cb() {
          return commentCallback(step.id, step.comment);
        };
        this.commentUpdate = setTimeout(cb, 3000);
        return e.stopPropagation();
      });
      if (step.comment) {
        return comment.text(step.comment).removeClass("empty");
      } else {
        initial = comment.attr("initial");
        return outer.find(".comment").text(initial).focus(function () {
          return $(this).text("").removeClass("empty");
        }).focusout(function () {
          if (!this.value) {
            return $(this).addClass("empty").text(initial);
          }
        });
      }
    };
    details = function details() {};
    details.setSize = function (width, height) {
      var queue = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : true;

      return outer.animate({
        width: width,
        height: height
      }, {
        duration: 'fast',
        queue: queue
      });
    };
    details.remove = function () {
      return outer.remove();
    };
    details.getId = function () {
      return id;
    };
    return details;
  };

  // --- Widget -----------------------------------------------------------
  Widget = function Widget(selection) {
    var changeComment, clickNode, data, details, hideDialog, lenseR, moveLenses, nodeR, options, pos, resetScale, setEvents, showDialog, size, ui, widget;
    options = {
      shiny: false,
      knitr: false
    };
    nodeR = 15;
    lenseR = 30;
    ui = UI(selection, nodeR, 15);
    pos = Position(nodeR);
    details = null;
    data = null;
    size = {
      width: 500,
      height: 500
    };
    widget = function widget() {};
    widget.setData = function (input) {
      data = Data(input);
      data.groupData(options.knitr ? 5 : 200);
      pos.calculate(data);
      ui.setData(data);
      ui.updateGraphicalElements();
      return setEvents();
    };
    widget.setSize = function (width, height) {
      size = {
        width: width,
        height: height
      };
      return ui.setSize(size.width, size.height);
    };
    widget.setOption = function (what, value) {
      if (what in options) {
        value = options[what].constructor(value);
        return options[what] = value;
      }
    };
    // delegate zooming  
    widget.zoom = ui.zoom;
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
        return datum.scale = 1 + lenseR / nodeR * Math.pow(1 - scale, 3);
      });
      return ui.updateGraphicalElements();
    };
    resetScale = function resetScale(d) {
      data.resetScale();
      return ui.updateGraphicalElements();
    };
    showDialog = function showDialog(d) {
      this.description = FloatingDescription(this, d, selection, Viewport(selection), nodeR);
      return this.description.show();
    };
    hideDialog = function hideDialog(d) {
      var ref;
      return (ref = this.description) != null ? ref.hide() : void 0;
    };
    clickNode = function clickNode(d) {
      var id, ref;
      log.debug("id: " + d.id + ", selected: " + d.selected);
      if ((ref = this.description) != null) {
        ref.hide();
      }
      id = !d.selected ? d.id : void 0;
      data.selected(id);
      data.resetScale();
      ui.updateGraphicalElements();
      if (options.shiny) {
        Shiny.onInputChange("object_selected", id);
      }
      if (details != null) {
        details.remove();
      }
      details = null;
      if (d.selected) {
        ui.setSize(size.width / 3, size.height, false);
        ui.scrollTo(id, false);
        return details = Details(selection, data, id, size.width * 2 / 3, size.height, changeComment);
      } else {
        return ui.setSize(size.width, size.height, false);
      }
    };
    changeComment = function changeComment(id, comment) {
      log.debug("id:" + id + " comment:" + comment);
      if (options.shiny) {
        Shiny.onInputChange("object_selected", id);
        return Shiny.onInputChange("comment", comment);
      }
    };
    // --- keyboard selection ---
    widget.selectParent = function () {
      if (!details) {
        return;
      }
      return ui.clickOn(data.parentOf(details.getId()));
    };
    widget.selectChild = function () {
      var children;
      if (!details) {
        return;
      }
      children = data.childrenOf(details.getId());
      if (children.length) {
        return ui.clickOn(children[0]);
      }
    };
    widget.selectSibling = function (key) {
      var me, siblings;
      if (!details) {
        return;
      }
      siblings = data.childrenOf(data.parentOf(details.getId()));
      me = siblings.indexOf(details.getId());
      if (key === "right" && me < siblings.length - 1) {
        ui.clickOn(siblings[me + 1]);
      }
      if (key === "left" && me > 0) {
        return ui.clickOn(siblings[me - 1]);
      }
    };
    widget.confirmSelection = function () {
      if (!details) {
        return;
      }
      if (options.shiny) {
        return Shiny.onInputChange('done', 'done');
      }
    };
    // --- search ---
    widget.search = function (phrase) {
      data.filter(phrase);
      pos.calculate(data);
      ui.setData(data);
      return ui.updateGraphicalElements();
    };
    return widget;
  };

  // export the Widget
  window.Widget = Widget;

  //log.enable(true)
  window.Data = Data;

  window.log = log;
}).call(undefined);