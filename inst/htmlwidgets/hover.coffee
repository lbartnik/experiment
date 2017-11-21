Hover = () ->
  width = 500
  height = 500;

  allData = []
  nodesG = null
  variablesG = null


  hover = (selection, data) ->
    allData = setupData(data)

    vis = d3.select(selection)
      .append("svg")
      .attr("width", width)
      .attr("height", height)

    nodesG = vis.append("g").attr("id", "nodes")
    variablesG = vis.append("g").attr("id", "variables")
    
    updateNodes()

  setupData = (data) ->
    data.commits.forEach (n) ->
      n.x = width/2
      n.y = height/2

    data.variables.forEach (v) ->
      link = data.links.filter (l) ->
        l.target == v.id
      if !link.length
        throw "cannot find link"
      v.parent = link[0].source

    return data

  showDetails = (d, i) ->
    thisVars = allData.variables.filter (v) ->
      v.parent == d.id
    thisVars.forEach (v) ->
      v.x = d.x
      v.y = d.y
    console.log(thisVars)
    thisVars = variablesG.selectAll("circle.node")
      .data(thisVars, (d) -> d.id)

    thisVars.enter().append("circle")
      .attr("class", "node")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
      .attr("r", 5)

    thisVars.exit().remove()

    variablesG.selectAll("circle.node")
      .transition()
      .duration(750)
      .attr('cx', (v, i) -> d.y - 10)
      .attr('cy', (v, i) -> d.y - 10)
    

  hideDetails = (d, i) ->
    variablesG.selectAll("circle.node")
      .transition()
      .duration(750)
      .attr('cx', (v, i) -> d.y)
      .attr('cy', (v, i) -> d.y)



  updateNodes = () ->
    node = nodesG.selectAll("circle.node")
      .data(allData.commits, (d) -> d.id)

    node.enter().append("circle")
      .attr("class", "node")
      .attr("cx", (d) -> d.x)
      .attr("cy", (d) -> d.y)
      .attr("r", 10)
      .on("mouseover", showDetails)
      .on("mouseout", hideDetails)

    node.exit().remove()

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


# function forceTick (e) {
#     d3.selectAll("circle.node")
#       .attr("cx", function (d) { return d.x; })
#       .attr("cy", function (d) { return d.y; });

#     d3.selectAll("line.link")
#       .attr("x1", function (d) { return d.source.x; })
#       .attr("y1", function (d) { return d.source.y; })
#       .attr("x2", function (d) { return d.target.x; })
#       .attr("y2", function (d) { return d.target.y; });
# }

# var simulation = d3.forceSimulation(data.nodes)
#     .on("tick", forceTick)
#     .force("charge", d3.forceManyBody())
#     .force("link", d3.forceLink(data.links));

# //        .linkDistance(50);

# nodesG.selectAll("circle.node")
#     .style("fill", function (d) {
#         if (d.rclass == "commit") return "red";
#     })
#     .on("mouseover", function (d) {
#         console.log(d);
#     });
