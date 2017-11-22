# compute positioning of variables
RadialLocation = (c, r, s) ->

  center = c
  radius = r
  size   = s

  computePosition = (angle) ->
    x = (center.x + radius * Math.cos(angle * Math.PI / 180))
    y = (center.y + radius * Math.sin(angle * Math.PI / 180))
    {"x":x,"y":y}
  
  radialLocation = () ->
  
  radialLocation.position = (i) ->
    computePosition(i * 360 / s)

  return radialLocation


# main class
Hover = () ->
  width = 500
  height = 500;

  allData = []
  nodesG = null
  linksG = null
  varsG  = null

  simulation = {}

  # constructor
  hover = (selection, data) ->
    allData = setupData(data)
    console.log(allData)
    simulation = d3.forceSimulation(allData.commits)
      .force("charge", d3.forceManyBody())
      .force("link", d3.forceLink(allData.links))
      .on("tick", forceTick)

    vis = d3.select(selection)
      .append("svg")
      .attr("width", width)
      .attr("height", height)

    nodesG = vis.append("g").attr("id", "nodes")
    linksG = vis.append("g").attr("id", "links")
    varsG  = vis.append("g").attr("id", "variables")
    
    updateNodes()

  # transform data set
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
        throw "cannot find link for variable #{v}"
      v.parent = link[0].source
    
    # replace target/source references in links with actual objects
    nodesMap  = mapNodes(data.commits)
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

  # update nodes according to data
  updateNodes = () ->
    node = nodesG.selectAll("circle.node")
      .data(allData.commits, (d) -> d.id)

    node.enter().append("circle")
      .attr("class", "node")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
      .attr("r", 10)
      .on("mouseover", showVariables)
      .on("mouseout", hideVariables)

    node.exit().remove()
  
  updateLinks = () ->
    link = linksG.selectAll("line.link")
      .data(curLinksData, (d) -> "#{d.source.id}_#{d.target.id}")
    link.enter().append("line")
      .attr("class", "link")
      .attr("stroke", "#ddd")
      .attr("stroke-opacity", 0.8)
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)

    link.exit().remove()

  forceTick = (e) ->
    nodesG.selectAll("circle.node")
      .attr("cx", (d) -> d.x )
      .attr("cy", (d) -> d.y )

    linksG.selectAll("line.link")
      .attr("x1", (d) -> d.source.x )
      .attr("y1", (d) -> d.source.y )
      .attr("x2", (d) -> d.target.x )
      .attr("y2", (d) -> d.target.y )

  # show details of a commit
  showVariables = (d, i) ->
    thisVars = allData.variables.filter (v) ->
      v.parent == d.id

    radial = RadialLocation(d, 50, thisVars.length)

    thisVars.forEach (v) ->
      v.x = d.x
      v.y = d.y

    thisVars = varsG.selectAll("circle.node")
      .data(thisVars, (d) -> d.id)

    thisVars.enter().append("circle")
      .attr("class", "node")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
      .attr("r", 5)

    thisVars.exit().remove()

    varsG.selectAll("circle.node")
      .transition()
      .duration(150)
      .attr('cx', (v, i) -> radial.position(i).x)
      .attr('cy', (v, i) -> radial.position(i).y)
    
  # hide the detailed view of a commit
  hideVariables = (d, i) ->
    varsG.selectAll("circle.node")
      .transition()
      .duration(750)
      .attr('cx', (v, i) -> d.y)
      .attr('cy', (v, i) -> d.y)
      .remove()



  return hover


# --------------------------

data = JSON.parse($("#graph").html())
myHover = Hover()
myHover('#canvas', data)


# var linksG = vis.append("g").attr("id", "links");
# var nodesMap  = mapNodes(data.nodes);

# data.links.forEach(function (l) {
#     l.source = nodesMap.get(l.source);
#     l.target = nodesMap.get(l.target);
# });


# var node = nodesG.selectAll("circle.node")
#       .data(data.nodes, function (d) { return d.id; });

# node.enter().append("circle")
#       .attr("class", "node")
#       .attr("cx", function (d) { return d.x; })
#       .attr("cy", function (d) { return d.y; })
#       .attr("r", 5);

# node.exit().remove();

# var link = linksG.selectAll("line.link")
#       .data(data.links, function (d) { d.source.id + "_" + d.target.id; });

# link.enter().append("line")
#       .attr("class", "link")
#       .attr("stroke", "#ddd")
#       .attr("stroke-opacity", 0.8)
#       .attr("x1", function (d) { d.source.x; })
#       .attr("y1", function (d) { d.source.y; })
#       .attr("x2", function (d) { d.target.x; })
#       .attr("y2", function (d) { d.target.y; });

# link.exit().remove();



# //        .linkDistance(50);

# nodesG.selectAll("circle.node")
#     .style("fill", function (d) {
#         if (d.rclass == "commit") return "red";
#     })
#     .on("mouseover", function (d) {
#         console.log(d);
#     });
