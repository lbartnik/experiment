RoundPosition = (center, radius, n) ->
  increment = if n < 12 then 30 else 360 / n

  return (i) ->
    angle = i * increment
    x = (center.x + radius * Math.cos(angle * Math.PI / 180))
    y = (center.y + radius * Math.sin(angle * Math.PI / 180))
    {"x":x,"y":y}


DiagonalPosition = (width, height, n) ->
  n = Math.max(n, width/50)
  return (i) ->
    { x: width * (i/n + .1), y: height * (i/n + .1) }



Widget = (selection) ->

  timeout = 150
  thumbnail = 25
  zoomed = 250
  data  = null
  vis = d3.select(selection)
    .attr("class", "widget")
    .style("overflow", "auto")
    .style('overflow-y', 'auto')
    .append("svg")
  
  linksG = vis.append("g").attr("id", "links")
  nodesG = vis.append("g").attr("id", "nodes")

  template =
    """
    <div class="tooltip">
        <div class="inner">
            <span class="name">{{name}}</span>
            <span class="description">{{description}}</span>
            <pre><code class="R">{{code}}</code></pre>
        </div>
    </div>
    """
  
  widget = () ->

  widget.setSize = (width, height) ->
    vis.attr("width", width * .95)
       .attr("height", height * .95)
       .attr("viewBox", "0 0 #{width} #{height}")
    if data
      refreshVisuals()

  widget.setData = (Data) ->
    data = setupData(Data)
    refreshVisuals()
  
  refreshVisuals = () ->
    filtered = filterData(data)
    createVisuals(filtered)
    placeVisuals(filtered)
    enableEvents()

  setupData = (data) ->
    # replace target/source references in links with actual objects
    stepsMap = mapNodes(data.steps)
    data.links.forEach (l) ->
      l.source = stepsMap.get(l.source)
      l.target = stepsMap.get(l.target)

    return data

  # Helper function to map node id's to node objects.
  # Returns d3.map of ids -> nodes
  mapNodes = (nodes) ->
    nodesMap = d3.map()
    nodes.forEach (n) ->
      nodesMap.set(n.id, n)
    nodesMap

  filterData = (data) -> data

  createVisuals = (data) ->
    # this sizes each node
    nodeSize = (selection) ->
      selection
        .attr("width", 2*thumbnail)
        .attr("height", thumbnail)
        .attr("rx", thumbnail/3)
        .attr("ry", thumbnail/3)

    # add regular steps
    steps = (step for step in data.steps when step.type is 'object')
    node = nodesG.selectAll("g.variable")
      .data(steps, (d) -> d.id)
    enter = node.enter().append("svg")
      .attr("class", "variable")
      .attr("viewBox", "-1 -2 52 29")
      .attr("width", "50")
      .attr("height", "25")
    nodeSize enter.append("rect")
    enter.append("text")
      .attr("class", "label")
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle")
      .attr("y", '50%')
      .attr("x", '50%')
      .text((d) -> d.name)
    nodeSize enter.append("rect")
      .attr("class", "face")
    node.exit().remove()
    
    # add plots
    addPlot(step) for step in data.steps when step.type is 'plot'

    link = linksG.selectAll("line.link")
      .data(data.links, (d) -> "#{d.source.id}_#{d.target.id}")
    link.enter().append("line")
      .attr("class", "link")
      .attr("stroke", "#ddd")
    link.exit().remove()

  addPlot = (step) ->
    if step.contents
      fromBase64 = atob(step.contents.replace(/\s/g, ""))
      parser = new DOMParser()
      doc = parser.parseFromString(fromBase64, "application/xml")
      plot = vis.node()
        .appendChild(doc.documentElement)
    else
      plot = vis.append("svg")
      plot.append("rect")
        .attr('width', 150)
        .attr('height', 150)
        .style("fill", "grey")
      plot = plot.node()

    # extract and remember the original size
    bb = plot.getBBox()
    step.width = bb.width - bb.x
    step.height = bb.height - bb.y

    # add the visual
    d3.select(plot)
      .data([step])
      .attr("id", "plot#{step.id}")
      .attr("class", "plot")
      .attr("viewBox", "#{bb.x} #{bb.y} #{bb.width} #{bb.height}")
      .attr("width", thumbnail)
      .attr("height", thumbnail)
      .append("rect")
      .attr("class", "face")
      .attr('width', '100%')
      .attr('height', '100%')

  placeVisuals = (data) ->
    parentsMap = d3.map()
    data.links.forEach (l) ->
      parentsMap.set(l.target.id, l.source.id)

    stratify = d3.stratify()
      .id((d) -> d.id)
      .parentId((d) -> parentsMap.get(d.id))
    root = stratify(data.steps)

    width = vis.attr("width")
    height = vis.attr("height")

    # give the tree layout a somewhat smaller area (w/h-thumbnail*4)    
    root.sort()
    tree = d3.tree()
      .size([width - thumbnail * 4, height - thumbnail * 4])
    root = tree(root)

    min_x = root.descendants().map((n) -> n.x).reduce((a,b) -> Math.min(a,b))
    min_y = root.descendants().map((n) -> n.y).reduce((a,b) -> Math.min(a,b))

    # mode all nodes, take the marings into account (+2*thumbnail)
    nodesMap = mapNodes(data.steps)
    root.each (n) ->
      s = nodesMap.get(n.id)
      s.x = n.x + thumbnail - min_x + thumbnail*2
      s.y = n.y + thumbnail/1.75 - min_y + thumbnail*2
    
    updatePositions()

    #bb = vis.node().getBBox()
    #x      = bb.x - thumbnail
    #y      = bb.y - thumbnail
    #width  = Math.max(bb.width + 2*thumbnail, vis.attr("width"))
    #height = Math.max(bb.height  + 2*thumbnail, vis.attr("height"))

    vis
      .attr("width", width)
      .attr("height", height)
      .attr("viewBox", "0 0 #{width} #{height}")

  updatePositions = () ->
    nodesG.selectAll("svg.variable")
      .attr("x", (d) -> d.x - thumbnail)
      .attr("y", (d) -> d.y - thumbnail/2)
    vis.selectAll("svg.plot")
      .attr("x", (d) -> d.x - thumbnail/2)
      .attr("y", (d) -> d.y - thumbnail/2)
    link = linksG.selectAll("line.link")
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)

  enableEvents = () ->
    vis.selectAll(".plot > rect.face")
      .on("mouseover", showPlot)
      .on("mouseout", hidePlot)
      .on("click", toClipboard)
    vis.selectAll(".variable > rect.face")
      .on("mouseover", showVariable)
      .on("mouseout", hideVariable)
      .on("click", toClipboard)

  # transition outside
  showPlot = (step) ->
    step.dx = Math.max(0, step.x + zoomed - vis.attr("width"))
    step.dy = Math.max(0, step.y + zoomed/step.width*step.height - vis.attr("height"))
    
    this.animation?.stop()
    this.animation = self = d3.timer (elapsed) ->
      if elapsed >= timeout
        self.stop()
      zoomFrame(step, Math.min(elapsed/timeout, 1))

  # transition inside
  hidePlot = (step) ->
    this.animation?.stop()
    this.animation = self = d3.timer (elapsed) ->
      if elapsed >= timeout
        self.stop()
      zoomFrame(step, Math.max(1 - elapsed/timeout, 0))

  zoomFrame = (step, alpha) ->
    zoom = Math.max(zoomed * alpha, thumbnail)
    vis.select("#plot#{step.id}")
      .attr("width", zoom)
      .attr("height", zoom/step.width * step.height)
      .attr("x", step.x - thumbnail/2 - step.dx * alpha)
      .attr("y", step.y - thumbnail/2 - step.dy * alpha)

  showVariable = (step) ->
    Mustache.parse(template)

    code = if step.expr.constructor is Array then step.expr.join('\n') else step.expr
    rendered = Mustache.render(template, {
      name: step.name,
      code: code,
      description: step.desc
    })
    tooltip = $(rendered)

    pos = $(selection).parent().position()
    bcr = this.getBoundingClientRect()

    tooltip
      .attr("id", "tooltip_#{step.id}")
      .css({left: bcr.left + bcr.width, top: bcr.top + bcr.height})
      .find("pre code").each (i, block) -> hljs.highlightBlock(block)
    tooltip.find(".inner").css({zoom: .1})
    
    this.tooltip?.remove()
    this.tooltip = tooltip.appendTo(selection)

    tooltip.css({visibility: 'visible'})
      .find(".inner")
      .animate({zoom: 1}, 'fast')

  hideVariable = (step) ->
    thisNode = this
    this.tooltip?.find('.inner').animate({zoom: 0}, 'fast', 'swing', () -> thisNode.tooltip?.remove())

  toClipboard = (step) ->
    input = $("<input>")
      .appendTo(selection)
      .val("restore('#{step.id}')")
      .select()
    document.execCommand("copy")
    input.remove()
    $.notify("ID copied to clipboard", {autoHideDelay: 1000, className: 'info', style: 'simplenotification'})

  widget.setSize($(selection).width(), $(selection).height())
  widget

# add style to notifyjs, just once
$.notify.addStyle('simplenotification', {
  html: "<div><span data-notify-text/></div>"
})

# export the Widget
window.Widget = Widget
