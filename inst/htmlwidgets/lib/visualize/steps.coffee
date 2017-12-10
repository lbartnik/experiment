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
    data.steps.forEach (n) ->
      n.x = Math.floor(Math.random()*width)
      n.y = Math.floor(Math.random()*height)
    
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
      .attr("x", (d) -> d.x)
      .attr("y", (d) -> d.y)
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
  
  widget.setSize = (Width, Height) ->
    width = Width
    height = Height
    vis.attr("width", width)
       .attr("height", height)

  widget

window.Widget = Widget