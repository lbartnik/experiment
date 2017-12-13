RoundPosition = (center, radius, n) ->
  increment = if n < 12 then 30 else 360 / n

  return (i) ->
    angle = i * increment
    x = (center.x + radius * Math.cos(angle * Math.PI / 180))
    y = (center.y + radius * Math.sin(angle * Math.PI / 180))
    {"x":x,"y":y}


Widget = (selection) ->

  timeout = 150
  thumbnail = 25
  width  = 500
  height = 500
  data  = null
  vis  = d3.select(selection)
    .append("svg")
    .attr("viewBox", "0 0 #{width} #{height}")

  linksG = vis.append("g").attr("id", "links")
  nodesG = vis.append("g").attr("id", "nodes")
  
  widget = () ->

  widget.setData = (Data) ->
    data = setupData(Data)
    filtered = filterData(data)
    createVisuals(filtered)
    placeVisuals(filtered, enableEvents)

  setupData = (data) ->
    # initialize positioning of commits
    rp = RoundPosition({x: width/2, y: height/2}, 120, data.steps.length)
    i  = 0
    data.steps.forEach (n) ->
      p = rp(i++)
      n.x = p.x
      n.y = p.y
    
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
    # add regular steps
    steps = (step for step in data.steps when step.type is 'object')
    node = nodesG.selectAll("circle.variable")
      .data(steps, (d) -> d.id)
    node.enter().append("circle")
      .attr("class", "variable")
      .attr("r", 10)
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
    fromBase64 = atob(step.contents)
    parser = new DOMParser()
    doc = parser.parseFromString(fromBase64, "application/xml")
    plot = vis.node()
      .appendChild(doc.documentElement)
    bb = plot.getBBox()
    d3.select(plot)
      .data([step])
      .attr("id", "plot#{step.id}")
      .attr("class", "plot")
      .attr("viewBox", "#{bb.x} #{bb.y} #{bb.width} #{bb.height}")
    zoomFrame(step, 0)

  placeVisuals = (data, whenDone) ->
    force = d3.forceSimulation(data.steps)
      .force("charge", d3.forceManyBody())
      .force("link", d3.forceLink(data.links).distance(50))
      .alphaMin(.1)
      .on("tick", (e) -> updatePositions())
      .on("end", whenDone)

  updatePositions = () ->
    nodesG.selectAll("circle.variable")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
    vis.selectAll("svg.plot")
      .attr("x", (d) -> d.x - thumbnail/2)
      .attr("y", (d) -> d.y - thumbnail/2)
    link = linksG.selectAll("line.link")
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)

  enableEvents = () ->
    vis.selectAll("svg.plot")
      .on("mouseover", showPlot)
      .on("mouseout", hidePlot)
    vis.selectAll("circle.variable")
      .on("mouseover", showVariable)
      .on("mouseout", hideVariable)

  # transition outside
  showPlot = (step) ->
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
    zoom = Math.max(250 * alpha, thumbnail)
    vis.select("#plot#{step.id}")
      .attr("width", zoom)
      .attr("height", zoom)
      .attr("x", step.x - zoom /2)
      .attr("y", step.y - zoom /2)

  showVariable = (step) ->
    template = $('#tooltip-template').html()
    Mustache.parse(template)

    code = if step.expr.constructor is Array then step.expr.join('\n') else step.expr
    rendered = Mustache.render(template, {
      name: step.name,
      code: code
    })
    tooltip = $(rendered)

    tooltip
      .attr("id", "tooltip_#{step.id}")
      .css({
        left: step.x + width * .05,
        top: step.y + height * .05
      })
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

  widget.setSize = (Width, Height) ->
    width = Width
    height = Height
    vis.attr("width", width)
       .attr("height", height)

  widget

window.Widget = Widget
