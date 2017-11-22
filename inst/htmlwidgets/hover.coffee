# compute positioning of variables
VariablesNetwork = (radius, vis) ->

  center = {}
  vars  = []
  links = []
  # links group goes before nodes to paint them below
  linksG = vis.append("g").attr("id", "varlinks")
  varsG  = vis.append("g").attr("id", "variables")

  variablesNetwork = () ->
  
  variablesNetwork.update = (newCenter, newVars, newLinks) ->
    center = newCenter
    vars   = newVars
    links  = newLinks

    # initialize all variables inside the commit node
    vars.forEach (v) ->
      v.x = center.x
      v.y = center.y

    vG = varsG.selectAll("circle.node")
      .data(vars, (d) -> d.id)
    vG.enter().append("circle")
      .attr("class", "node variable")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
      .attr("r", 5)
    vG.exit().remove()

    lG = linksG.selectAll("line.link")
      .data(links, (d) -> "#{d.source.id}_#{d.target.id}")
    lG.enter().append("line")
      .attr("class", "link")
      .attr("stroke", "#ddd")
      .attr("stroke-opacity", 0.8)
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)
    lG.exit().remove()

  # transition outside
  variablesNetwork.show = () ->
    varsG.selectAll("circle.node")
      .raise()
      .transition()
      .duration(150)
      .attr('cx', (v, i) -> computePosition(i * 360 / vars.length).x)
      .attr('cy', (v, i) -> computePosition(i * 360 / vars.length).y)
    linksG.selectAll("line.link")
      .lower()
      .transition()
      .duration(150)
      .attr("x2", (v, i) -> computePosition(i * 360 / vars.length).x)
      .attr("y2", (v, i) -> computePosition(i * 360 / vars.length).y)

  # transition inside
  variablesNetwork.hide = () ->
    varsG.selectAll("circle.node")
      .transition()
      .duration(750)
      .attr('cx', (v, i) -> center.x)
      .attr('cy', (v, i) -> center.y)
      .remove()
    linksG.selectAll("line.link")
      .transition()
      .duration(750)
      .attr("x2", (v, i) -> center.x)
      .attr("y2", (v, i) -> center.y)

  computePosition = (angle) ->
    x = (center.x + radius * Math.cos(angle * Math.PI / 180))
    y = (center.y + radius * Math.sin(angle * Math.PI / 180))
    {"x":x,"y":y}
  
  return variablesNetwork


# compute positioning of commits
CommitsNetwork = (commits, links, width, height, vis, showVariables, hideVariables) ->
  linksG = vis.append("g").attr("id", "links")
  nodesG = vis.append("g").attr("id", "nodes")
  force = null

  network = () ->

  # constructor
  network.init = () ->
    updateNodes()
    updateLinks()
    force = d3.forceSimulation(commits)
      .force("charge", d3.forceManyBody())
      .force("link", d3.forceLink(links).distance(50))
      .force("center", d3.forceCenter(width/2, height/2))
      .alphaMin(.1)
      .on("tick", forceTick)

  forceTick = (e) ->
    nodesG.selectAll("circle.node")
      .attr("cx", (d) -> d.x )
      .attr("cy", (d) -> d.y )
    linksG.selectAll("line.link")
      .attr("x1", (d) -> d.source.x )
      .attr("y1", (d) -> d.source.y )
      .attr("x2", (d) -> d.target.x )
      .attr("y2", (d) -> d.target.y )

  updateNodes = () ->
    node = nodesG.selectAll("circle.node")
      .data(commits, (d) -> d.id)
      .raise()
    node.enter().append("circle")
      .attr("class", "node commit")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
      .attr("r", 10)
      .raise()
      .on("mouseover", showVariables)
      .on("mouseout", hideVariables)
    node.exit().remove()
  
  updateLinks = () ->
    link = linksG.selectAll("line.link")
      .data(links, (d) -> "#{d.source.id}_#{d.target.id}")
    link.enter().append("line")
      .attr("class", "link")
      .attr("stroke", "#ddd")
      .attr("stroke-opacity", 0.8)
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)
      .lower()
    link.exit().remove()

  # initialize & return the new network object
  network.init()
  return network



# main class
Network = (selection, data) ->
  width = 500
  height = 500
  vn = null

  # constructor
  network = () ->
  
  network.init = () ->
    # create the canvas
    vis = d3.select(selection)
      .append("svg")
      .attr("width", width)
      .attr("height", height)

    # transform the input data
    data = setupData(data)
    # variables go before commits to paint them below
    vn = VariablesNetwork(50, vis)
    links = filterLinks(data.links, data.commits)
    cn = CommitsNetwork(data.commits, links, 500, 500, vis, showVariables, hideVariables)

  # transform the data set
  setupData = (data) ->
    # initialize positioning of commits
    data.commits.forEach (n) ->
      n.x = Math.floor(Math.random()*width)
      n.y = Math.floor(Math.random()*height)

    # find parent comments for all variables
    data.variables.forEach (v) ->
      link = data.links.filter (l) ->
        l.target == v.id
      if !link.length
        throw "cannot find link for variable #{v.id}"
      v.parent = link[0].source
    
    # replace target/source references in links with actual objects
    allNodes = data.commits.concat(data.variables)
    nodesMap = mapNodes(allNodes)
    data.links.forEach (l) ->
      l.source = nodesMap.get(l.source)
      l.target = nodesMap.get(l.target)

    return data

  # Helper function to map node id's to node objects.
  # Returns d3.map of ids -> nodes
  mapNodes = (nodes) ->
    nodesMap = d3.map()
    nodes.forEach (n) ->
      nodesMap.set(n.id, n)
    nodesMap

  # helper function
  filterLinks = (allLinks, curNodes) ->
    curNodes = mapNodes(curNodes)
    allLinks.filter (l) ->
      curNodes.get(l.source.id) and curNodes.get(l.target.id)

  # show details of a commit
  showVariables = (d, i) ->
    vars = data.variables.filter (v) ->
      v.parent == d.id
    links = filterLinks(data.links, vars.concat(d))
    vn.update(d, vars, links)
    vn.show()
    
  # hide the detailed view of a commit
  hideVariables = (d, i) ->
    vn.hide()

  network.init()
  return network

# make it global
window.Network = Network
