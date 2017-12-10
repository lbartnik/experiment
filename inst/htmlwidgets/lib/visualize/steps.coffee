RoundPosition = (center, radius, n) ->
  increment = if n < 12 then 30 else 360 / n

  return (i) ->
    angle = i * increment
    x = (center.x + radius * Math.cos(angle * Math.PI / 180))
    y = (center.y + radius * Math.sin(angle * Math.PI / 180))
    {"x":x,"y":y}


Widget = (selection) ->

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
    setupVisuals(filtered)

    force = d3.forceSimulation(data.steps)
      .force("charge", d3.forceManyBody())
      .force("link", d3.forceLink(data.links).distance(50))
      .alphaMin(.1)
      .on("tick", (e) -> updatePosition())    

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

  setupVisuals = (data) ->
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


  updatePosition = () ->
    nodesG.selectAll("circle")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
    vis.selectAll("svg.plot")
      .attr("x", (d) -> d.x - 12.5)
      .attr("y", (d) -> d.y - 12.5)
    link = linksG.selectAll("line.link")
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)

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
      .attr("width", 25)
      .attr("height", 25)
      .on("mouseover", (e) -> show(step))
      .on("mouseout", (e) -> hide(step))

  # transition outside
  show = (step, timeout = 150) ->
    animation?.stop()
    animation = self = d3.timer (elapsed) ->
      zoomFrame(self, step, Math.min(elapsed/timeout, 1))

  # transition inside
  hide = (step, timeout = 150) ->
    animation?.stop()
    animation = self = d3.timer (elapsed) ->
      zoomFrame(self, step, Math.max(1 - elapsed/timeout, 0))

  zoomFrame = (timer, step, alpha) ->
    plot = d3.select("#plot#{step.id}")
    if alpha == 1 or alpha == 0
      timer.stop()
    zoom = Math.max(250 * alpha, 25)
    plot
      .attr("width", zoom)
      .attr("height", zoom)
      .attr("x", step.x - zoom /2)
      .attr("y", step.y - zoom /2)


  widget.setSize = (Width, Height) ->
    width = Width
    height = Height
    vis.attr("width", width)
       .attr("height", height)

  widget

window.Widget = Widget
