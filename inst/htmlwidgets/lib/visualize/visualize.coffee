UpdateNodes = (type, vis, rx, ry) ->

  # links group goes before nodes to paint them below
  linksG = vis.append("g").attr("id", "links_#{type}")
  nodesG = vis.append("g").attr("id", "nodes_#{type}")

  updateNodes = () ->

  updateNodes.update = (nodes, links) -> 
    # nodes: group element, background node, text, mouse-face
    node = nodesG.selectAll("g.#{type}")
      .data(nodes, (d) -> d.id)
    enter = node.enter().append("g")
      .attr("class", "#{type}")
    enter.append("ellipse")
      .attr("rx", rx)
      .attr("ry", ry)
    enter.append("text")
      .attr("dy", ry / 3)
      .attr("text-anchor", "middle")
      .attr("class", "label")
      .text((d) -> d.label)
    enter.append("ellipse")
      .attr("class", "face")
      .attr("rx", rx)
      .attr("ry", ry)
      .style('fill', 'transparent')
    node.exit().remove()

    # links
    link = linksG.selectAll("line.link")
      .data(links, (d) -> "#{d.source.id}_#{d.target.id}")
    link.enter().append("line")
      .attr("class", "link")
      .attr("stroke", "#ddd")
    link.exit().remove()

    this.updatePosition()
  
  updateNodes.updatePosition = () ->
    node = nodesG.selectAll("g.#{type}")
      .attr("transform", (d) -> "translate(#{d.x},#{d.y})")
    link = linksG.selectAll("line.link")
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)
  
  updateNodes.remove = () ->
    node = nodesG.selectAll("g.#{type}")
      .remove()
  
  updateNodes.on = (what, callback) ->
    nodesG.selectAll(".face")
      .on(what, callback)
  
  return updateNodes


# compute positioning of objects
VariablesNetwork = (radius, vis) ->

  center = {}
  vars  = []
  links = []
  updater = UpdateNodes('variable', vis, 20, 10)
  animation = null

  variablesNetwork = () ->
  
  variablesNetwork.update = (newCenter, newVars, newLinks) ->
    center = newCenter
    vars   = newVars
    links  = newLinks

    # initialize all variables inside the commit node
    vars.forEach (v) ->
      v.x = center.x
      v.y = center.y
    
    updater.update(vars, links)
    updater.updatePosition()

  # returns a callback used with d3.timer
  move = (alpha) ->
    vars.forEach (d, i) ->
      p = computePosition(i * 360 / vars.length, alpha)
      d.x = p.x
      d.y = p.y
    updater.updatePosition()
    
  # transition outside
  variablesNetwork.show = (timeout = 150) ->
    animation?.stop()
    animation = self = d3.timer (elapsed) ->
      if elapsed > timeout
        self.stop()
      move(Math.min(elapsed/timeout, 1))      

  # transition inside
  variablesNetwork.hide = (timeout = 150) ->
    animation?.stop()
    animation = self = d3.timer (elapsed) ->
      if elapsed > timeout
        self.stop()
      move(1 - Math.min(elapsed/timeout, 1))      

  computePosition = (angle, alpha) ->
    x = (center.x + alpha * radius * Math.cos(angle * Math.PI / 180))
    y = (center.y + alpha * radius * Math.sin(angle * Math.PI / 180))
    {"x":x,"y":y}
  
  return variablesNetwork


# compute positioning of commits
CommitsNetwork = (vis, showVariables, hideVariables) ->
  updater = UpdateNodes('commit', vis, 40, 15)
  force = null
  dist = 150

  network = () ->

  network.setData = (commits, links) ->
    updater.update(commits, links)
    updater.on("mouseover", showVariables)
    updater.on("mouseout", hideVariables)
    force = d3.forceSimulation(commits)
      .force("charge", d3.forceManyBody())
      .force("link", d3.forceLink(links).distance(dist))
      .alphaMin(.1)
      .on("tick", (e) -> updater.updatePosition())    

  return network



# main class
Network = (selection, data = {}) ->
  options = {
    width: 500,
    height: 500,
    varRadius: 75
  }
  vn = null
  cn = null
  vis = null

  # constructor
  network = () ->
  
  network.init = () ->
    # create the canvas
    vis = d3.select(selection)
      .append("svg")

    # objects go before commits to paint them below
    vn = VariablesNetwork(options.varRadius, vis)
    cn = CommitsNetwork(vis, showVariables, hideVariables)

    this.setData(data)
    this.setSize(options.width, options.height)

  # update the data set
  network.setData = (newData) ->
    if not ('commits' of newData and 'objects' of newData and 'links' of newData)
      return data

    data = setupData(newData)
    links = filterLinks(data.links, data.commits)
    cn.setData(data.commits, links)
  
  # update widget size
  network.setSize = (width, height) ->
    options.width = width
    options.height = height
    vis.attr("width", width)
       .attr("height", height)


  # transform the data set
  setupData = (data) ->
    # initialize positioning of commits
    data.commits.forEach (n) ->
      n.x = Math.floor(Math.random()*options.width)
      n.y = Math.floor(Math.random()*options.height)

    # find parent comments for all objects
    data.objects.forEach (v) ->
      link = data.links.filter (l) ->
        l.target == v.id
      if !link.length
        throw "cannot find link for variable #{v.id}"
      v.parent = link[0].source
    
    # replace target/source references in links with actual objects
    allNodes = data.commits.concat(data.objects)
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
    vars = data.objects.filter (v) ->
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
export { Network }
