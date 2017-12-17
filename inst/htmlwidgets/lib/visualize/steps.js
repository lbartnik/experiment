"use strict";

// Generated by CoffeeScript 2.0.3
(function () {
  var DiagonalPosition, RoundPosition, Widget;

  RoundPosition = function RoundPosition(center, radius, n) {
    var increment;
    increment = n < 12 ? 30 : 360 / n;
    return function (i) {
      var angle, x, y;
      angle = i * increment;
      x = center.x + radius * Math.cos(angle * Math.PI / 180);
      y = center.y + radius * Math.sin(angle * Math.PI / 180);
      return {
        "x": x,
        "y": y
      };
    };
  };

  DiagonalPosition = function DiagonalPosition(width, height, n) {
    n = Math.max(n, width / 50);
    return function (i) {
      return {
        x: width * (i / n + .1),
        y: height * (i / n + .1)
      };
    };
  };

  Widget = function Widget(selection) {
    var addPlot, createVisuals, data, enableEvents, filterData, hidePlot, hideVariable, linksG, mapNodes, nodesG, placeVisuals, refreshVisuals, setupData, showPlot, showVariable, template, thumbnail, timeout, toClipboard, updatePositions, vis, widget, zoomFrame, zoomed;
    timeout = 150;
    thumbnail = 25;
    zoomed = 250;
    data = null;
    vis = d3.select(selection).attr("class", "widget").style("overflow", "auto").style('overflow-y', 'auto').append("svg");
    linksG = vis.append("g").attr("id", "links");
    nodesG = vis.append("g").attr("id", "nodes");
    template = "<div class=\"tooltip\">\n    <div class=\"inner\">\n        <span class=\"name\">{{name}}</span>\n        <span class=\"description\">{{description}}</span>\n        <pre><code class=\"R\">{{code}}</code></pre>\n    </div>\n</div>";
    widget = function widget() {};
    widget.setSize = function (width, height) {
      vis.attr("width", width).attr("height", height).attr("viewBox", "0 0 " + width + " " + height);
      if (data) {
        return refreshVisuals();
      }
    };
    widget.setData = function (Data) {
      data = setupData(Data);
      return refreshVisuals();
    };
    refreshVisuals = function refreshVisuals() {
      var filtered;
      filtered = filterData(data);
      createVisuals(filtered);
      placeVisuals(filtered);
      return enableEvents();
    };
    setupData = function setupData(data) {
      var stepsMap;
      // replace target/source references in links with actual objects
      stepsMap = mapNodes(data.steps);
      data.links.forEach(function (l) {
        l.source = stepsMap.get(l.source);
        return l.target = stepsMap.get(l.target);
      });
      return data;
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
    filterData = function filterData(data) {
      return data;
    };
    createVisuals = function createVisuals(data) {
      var enter, j, len, link, node, nodeSize, ref, step, steps;
      // this sizes each node
      nodeSize = function nodeSize(selection) {
        return selection.attr("width", 2 * thumbnail).attr("height", thumbnail).attr("rx", thumbnail / 3).attr("ry", thumbnail / 3);
      };
      // add regular steps
      steps = function () {
        var j, len, ref, results;
        ref = data.steps;
        results = [];
        for (j = 0, len = ref.length; j < len; j++) {
          step = ref[j];
          if (step.type === 'object') {
            results.push(step);
          }
        }
        return results;
      }();
      node = nodesG.selectAll("g.variable").data(steps, function (d) {
        return d.id;
      });
      enter = node.enter().append("svg").attr("class", "variable").attr("viewBox", "-1 -2 52 29").attr("width", "50").attr("height", "25");
      nodeSize(enter.append("rect"));
      enter.append("text").attr("class", "label").attr("text-anchor", "middle").attr("alignment-baseline", "middle").attr("y", '50%').attr("x", '50%').text(function (d) {
        return d.name;
      });
      nodeSize(enter.append("rect")).attr("class", "face");
      node.exit().remove();
      ref = data.steps;
      for (j = 0, len = ref.length; j < len; j++) {
        step = ref[j];
        if (step.type === 'plot') {

          // add plots
          addPlot(step);
        }
      }
      link = linksG.selectAll("line.link").data(data.links, function (d) {
        return d.source.id + "_" + d.target.id;
      });
      link.enter().append("line").attr("class", "link").attr("stroke", "#ddd");
      return link.exit().remove();
    };
    addPlot = function addPlot(step) {
      var bb, doc, fromBase64, parser, plot;
      fromBase64 = atob(step.contents.replace(/\s/g, ""));
      parser = new DOMParser();
      doc = parser.parseFromString(fromBase64, "application/xml");
      plot = vis.node().appendChild(doc.documentElement);
      // extract and remember the original size
      bb = plot.getBBox();
      step.width = bb.width - bb.x;
      step.height = bb.height - bb.y;
      // add the visual
      return d3.select(plot).data([step]).attr("id", "plot" + step.id).attr("class", "plot").attr("viewBox", bb.x + " " + bb.y + " " + bb.width + " " + bb.height).attr("width", thumbnail).attr("height", thumbnail).append("rect").attr("class", "face").attr('width', '100%').attr('height', '100%');
    };
    placeVisuals = function placeVisuals(data) {
      var bb, height, min_x, min_y, nodesMap, parentsMap, root, stratify, tree, width, x, y;
      parentsMap = d3.map();
      data.links.forEach(function (l) {
        return parentsMap.set(l.target.id, l.source.id);
      });
      stratify = d3.stratify().id(function (d) {
        return d.id;
      }).parentId(function (d) {
        return parentsMap.get(d.id);
      });
      root = stratify(data.steps);
      root.sort();
      tree = d3.tree().size([vis.attr("width") - thumbnail * 1.5, vis.attr("height") - thumbnail * 1.5]);
      root = tree(root);
      min_x = root.descendants().map(function (n) {
        return n.x;
      }).reduce(function (a, b) {
        return Math.min(a, b);
      });
      min_y = root.descendants().map(function (n) {
        return n.y;
      }).reduce(function (a, b) {
        return Math.min(a, b);
      });
      nodesMap = mapNodes(data.steps);
      root.each(function (n) {
        var s;
        s = nodesMap.get(n.id);
        s.x = n.x + thumbnail - min_x;
        return s.y = n.y + thumbnail / 1.75 - min_y;
      });
      updatePositions();
      bb = vis.node().getBBox();
      x = bb.x - thumbnail;
      y = bb.y - thumbnail;
      width = Math.max(bb.width + 2 * thumbnail, vis.attr("width"));
      height = Math.max(bb.height + 2 * thumbnail, vis.attr("height"));
      return vis.attr("width", width).attr("height", height).attr("viewBox", x + " " + y + " " + width + " " + height);
    };
    updatePositions = function updatePositions() {
      var link;
      nodesG.selectAll("svg.variable").attr("x", function (d) {
        return d.x - thumbnail;
      }).attr("y", function (d) {
        return d.y - thumbnail / 2;
      });
      vis.selectAll("svg.plot").attr("x", function (d) {
        return d.x - thumbnail / 2;
      }).attr("y", function (d) {
        return d.y - thumbnail / 2;
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
    enableEvents = function enableEvents() {
      vis.selectAll(".plot > rect.face").on("mouseover", showPlot).on("mouseout", hidePlot).on("click", toClipboard);
      return vis.selectAll(".variable > rect.face").on("mouseover", showVariable).on("mouseout", hideVariable).on("click", toClipboard);
    };
    // transition outside
    showPlot = function showPlot(step) {
      var ref, self;
      step.dx = Math.max(0, step.x + zoomed - vis.attr("width"));
      step.dy = Math.max(0, step.y + zoomed / step.width * step.height - vis.attr("height"));
      if ((ref = this.animation) != null) {
        ref.stop();
      }
      return this.animation = self = d3.timer(function (elapsed) {
        if (elapsed >= timeout) {
          self.stop();
        }
        return zoomFrame(step, Math.min(elapsed / timeout, 1));
      });
    };
    // transition inside
    hidePlot = function hidePlot(step) {
      var ref, self;
      if ((ref = this.animation) != null) {
        ref.stop();
      }
      return this.animation = self = d3.timer(function (elapsed) {
        if (elapsed >= timeout) {
          self.stop();
        }
        return zoomFrame(step, Math.max(1 - elapsed / timeout, 0));
      });
    };
    zoomFrame = function zoomFrame(step, alpha) {
      var zoom;
      zoom = Math.max(zoomed * alpha, thumbnail);
      return vis.select("#plot" + step.id).attr("width", zoom).attr("height", zoom / step.width * step.height).attr("x", step.x - thumbnail / 2 - step.dx * alpha).attr("y", step.y - thumbnail / 2 - step.dy * alpha);
    };
    showVariable = function showVariable(step) {
      var bcr, code, pos, ref, rendered, tooltip;
      Mustache.parse(template);
      code = step.expr.constructor === Array ? step.expr.join('\n') : step.expr;
      rendered = Mustache.render(template, {
        name: step.name,
        code: code,
        description: step.desc
      });
      tooltip = $(rendered);
      pos = $(selection).parent().position();
      bcr = this.getBoundingClientRect();
      console.log(pos);
      console.log(bcr);
      tooltip.attr("id", "tooltip_" + step.id).css({
        left: bcr.left + bcr.width,
        top: bcr.top + bcr.height
      }).find("pre code").each(function (i, block) {
        return hljs.highlightBlock(block);
      });
      tooltip.find(".inner").css({
        zoom: .1
      });
      if ((ref = this.tooltip) != null) {
        ref.remove();
      }
      this.tooltip = tooltip.appendTo(selection);
      return tooltip.css({
        visibility: 'visible'
      }).find(".inner").animate({
        zoom: 1
      }, 'fast');
    };
    hideVariable = function hideVariable(step) {
      var ref, thisNode;
      thisNode = this;
      return (ref = this.tooltip) != null ? ref.find('.inner').animate({
        zoom: 0
      }, 'fast', 'swing', function () {
        var ref1;
        return (ref1 = thisNode.tooltip) != null ? ref1.remove() : void 0;
      }) : void 0;
    };
    toClipboard = function toClipboard(step) {
      var input;
      input = $("<input>").appendTo(selection).val("restore('" + step.id + "')").select();
      document.execCommand("copy");
      input.remove();
      return $.notify("ID copied to clipboard", {
        autoHideDelay: 1000,
        className: 'info',
        style: 'simplenotification'
      });
    };
    widget.setSize($(selection).width(), $(selection).height());
    return widget;
  };

  // add style to notifyjs, just once
  $.notify.addStyle('simplenotification', {
    html: "<div><span data-notify-text/></div>"
  });

  // export the Widget
  window.Widget = Widget;
}).call(undefined);