Array::unique = () ->
      output = {}
      (output[@[key]] = @[key]) for key in [0...@length]
      value for key, value of output

# Helper function to map node id's to node objects.
# Returns d3.map of ids -> nodes
mapNodes = (nodes) ->
  nodesMap = d3.map()
  nodes.forEach (n) ->
    nodesMap.set(n.id, n)
  nodesMap

euclidean = (a, b) ->
  Math.sqrt((a.x - b.x)**2 + (a.y - b.y)**2)


# --- Utils ------------------------------------------------------------

UI = (selection, nodeR = 25, innerR = 25) ->
  outer  = null
  canvas = null
  linksG = null
  nodesG = null

  ui = () ->

  ui.initialize = () ->
    outer = d3.select(selection)
      .append("div")
      .attr("class", "widget")
      .style("overflow", "auto")
      .style('overflow-y', 'auto')
    canvas = outer.append("svg")
    linksG = canvas.append("g").attr("id", "links")
    nodesG = canvas.append("g").attr("id", "nodes")
  
  ui.setSize = (width, height) ->
    # reduce the size to make sure scrolls don't show right away
    canvas.attr("width", width - 10)
      .attr("height", height - 10)
      .attr("viewBox", "0 0 #{width} #{height}")

  ui.setData = (data) ->
    createGraphics(data)

  # create all graphical elements on the canvas
  createGraphics = (data) ->
    node = nodesG.selectAll("svg.variable")
      .data(data.steps, (d) -> d.id)
    enter = node.enter().append("svg")
      .attr("class", (d) -> "variable #{d.type}")
      .attr("id", (d) -> d.id)
      .attr("viewBox", "0 0 #{2*innerR} #{2*innerR}")
      .attr("width", 2*nodeR)
      .attr("height", 2*nodeR)
    enter.each (d) ->
      element = d3.select(this)
      if d.type is 'object'
        element.append("rect")
          .attr("width", 2*innerR)
          .attr("height", 2*innerR)
          .attr("rx", innerR/2)
          .attr("ry", innerR/2)
        element.append("text")
          .attr("class", "label")
          .attr("text-anchor", "middle")
          .attr("alignment-baseline", "middle")
          .attr("y", '50%')
          .attr("x", '50%')
          .text((d) -> d.name)
        element.append("rect")
          .attr("class", "face")
          .attr("width", 2*innerR)
          .attr("height", 2*innerR)
      else
        if d.contents
          element.append("image")
            .attr("width", 2*innerR)
            .attr("height", 2*innerR)
            .attr("xlink:href", $("#plot#{d.id}").attr("href"))
        else
          element.append("rect")
            .attr('width', 2*innerR)
            .attr('height', 2*innerR)
            .style("fill", "grey")
    node.exit().remove()
    
    link = linksG.selectAll("line.link")
      .data(data.links, (d) -> "#{d.source.id}_#{d.target.id}")
    link.enter().append("line")
      .attr("class", "link")
      .attr("stroke", "#ddd")
    link.exit().remove()
  # --- createGraphics

  ui.updatePositions = () ->
    nodesG.selectAll("svg.variable")
      .attr("x", (d) -> d.x - d.scale * nodeR)
      .attr("y", (d) -> d.y - d.scale * nodeR)
      .attr("width", (d) -> d.scale * 2*nodeR)
      .attr("height", (d) -> d.scale * 2*nodeR)

    link = linksG.selectAll("line.link")
      .attr("x1", (d) -> d.source.x)
      .attr("y1", (d) -> d.source.y)
      .attr("x2", (d) -> d.target.x)
      .attr("y2", (d) -> d.target.y)

  ui.mousePosition = () ->
    [x,y] = d3.mouse(canvas.node())
    {x: x, y: y}

  ui.nodesNear = (point, distance) ->
    rc = canvas.node().createSVGRect()
    rc.x = point.x - distance
    rc.y = point.y - distance
    rc.width = 2 * distance
    rc.height = 2 * distance
    # returns SVG node elements inside the parent svg
    intList = canvas.node().getIntersectionList(rc, nodesG.node())
    parents = (n.parentNode for n in intList).unique()
    parents.filter((n) -> euclidean(d3.select(n).datum(), point) <= distance)


  # --- events ---
  ui.on = (event, callback) ->
    # canvas-level events
    if event in ['canvas:mousemove', 'canvas:mouseout'] 
      canvas.on(event.substring(7), callback)
    # node-level events
    if event in ['node:mouseover', 'node:mouseout']
      nodesG.selectAll(".face,image")
        .on(event.substring(5), callback)

  ui.initialize()
  return ui
# --- UI ---------------------------------------------------------------

Data = (data) ->

  data.resetScale = () ->
    data.steps.forEach (s) ->
      s.scale = 1

  # pre-process the input data
  setupData = () ->
    data.resetScale()
    # pre-process nodes
    data.steps.forEach (s) ->
      if s.expr.constructor is Array
        s.expr = s.expr.join('\n')
    # replace target/source references in links with actual objects
    stepsMap = mapNodes(data.steps)
    data.links.forEach (l) ->
      l.source = stepsMap.get(l.source)
      l.target = stepsMap.get(l.target)

  # initialize the object
  setupData()
  return data

# --- Data -------------------------------------------------------------

Position = (width, height, margin) ->
  width  = width - margin * 2
  height = height - margin * 2

  position = () ->

  stratified = (data) ->
    parentsMap = d3.map()
    data.links.forEach (l) ->
      parentsMap.set(l.target.id, l.source.id)
    stratify = d3.stratify()
      .id((d) -> d.id)
      .parentId((d) -> parentsMap.get(d.id))
    stratify(data.steps)

  treed = (data) ->
    data.sort()
    tree = d3.tree()
      .size([width, height])
    tree(data)
  
  position.calculate = (data) ->
    # use d3 to calculate positions for a tree
    s = stratified(data)
    t = treed(s)

    # centralize the tree
    x = t.descendants().map((n) -> n.x)
    y = t.descendants().map((n) -> n.y)
    min_x = x.reduce((a,b) -> Math.min(a,b))
    max_x = x.reduce((a,b) -> Math.max(a,b))
    min_y = y.reduce((a,b) -> Math.min(a,b))
    max_y = y.reduce((a,b) -> Math.max(a,b))

    dx = (width - max_x) - min_x + margin
    dy = (height - max_y) - min_y + margin

    # update original nodes' positions
    nodesMap = mapNodes(data.steps)
    t.each (n) ->
      s = nodesMap.get(n.id)
      s.x = n.x + dx
      s.y = n.y + dy
  # --- calculate ---
  
  # return an instance of the Position object
  position
# --- Position ---------------------------------------------------------

Description = (element, step, outer) ->

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

  description = () ->
  description.show = () ->
    tooltip = $("<div>").addClass("tooltip").attr("id", "tooltip_#{step.id}")
    if step.type is "object"
      inner   = $("<div>").addClass("inner").appendTo(tooltip)
      $("<span>").addClass("name").appendTo(inner).text(step.name)
      $("<span>").addClass("description").appendTo(inner).text(step.desc)
      $("<pre>").appendTo(inner).append $("<code>").addClass("R").text(step.expr)
      inner.find("pre code").each (i, block) -> hljs.highlightBlock(block)
    else
      $("<img>", {src: 'links/' + step.contents, width: 300}).appendTo(tooltip)


    pos = $(outer).parent().position()
    bcr = element.getBoundingClientRect()

    tooltip
      .css({left: bcr.left + bcr.width, top: bcr.top + bcr.height})
    
    element.tooltip?.remove()
    element.tooltip = tooltip.appendTo(outer)
      .css({visibility: 'visible'})
      .fadeTo('fast', 1)

  description.hide = () ->
    element.tooltip?.fadeTo('fast', 0, () -> element.tooltip?.remove())

  return description



# --- Widget -----------------------------------------------------------
Widget = (selection) ->
  nodeR  = 15
  lenseR = 50
  ui     = UI(selection, nodeR)
  pos    = Position(500, 500, nodeR)
  data   = null

  widget = () ->

  widget.setData = (input) ->
    data = Data(input)
    ui.setData(data)
    pos.calculate(data)
    ui.updatePositions()
    setEvents()

  widget.setSize = (width, height) ->
    ui.setSize(width,height)
    pos = Position(width, height, nodeR)

  setEvents = () ->
    ui.on('canvas:mousemove', moveLenses)
    ui.on('canvas:mouseout', resetScale)
    ui.on('node:mouseover', showDialog)
    ui.on('node:mouseout', hideDialog)

  moveLenses = (d) ->
    data.resetScale()
    mouse  = ui.mousePosition()
    nodes = ui.nodesNear(mouse, lenseR)
    nodes.forEach (n) ->
      datum = d3.select(n).datum()
      scale = euclidean(mouse, datum)/lenseR
      datum.scale = 1 + lenseR/nodeR * Math.sqrt(1-scale)
    ui.updatePositions()
  
  resetScale = (d) ->
    data.resetScale()
    ui.updatePositions()
  
  showDialog = (d) ->
    this.description = Description(this, d, selection)
    this.description.show()
  
  hideDialog = (d) ->
    this.description.hide()

  return widget














Widget2 = (selection) ->

  lenses_r = 50
  timeout = 150
  thumbnail = 25
  zoomed = 250
  data  = null
  vis = d3.select(selection)
    .attr("class", "widget")
    .style("overflow", "auto")
    .style('overflow-y', 'auto')
    .append("svg")
  
  lenses = vis.append("circle")
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

    # the lense circle goes to the bottom
    lenses.attr("class", "lenses")
      .attr("r", 50)
    
    d3.select(window)
      .on("mousemove", moveLenses)

    # add regular steps
    steps = (step for step in data.steps when step.type is 'object')
    node = nodesG.selectAll("g.variable")
      .data(steps, (d) -> d.id)
    enter = node.enter().append("svg")
      .attr("class", "variable")
      .attr("viewBox", "-1 -2 52 29")
      .attr("width", "50")
      .attr("height", "25")
      .attr("id", (d) -> d.id)
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

  moveLenses = (e) ->
    m = d3.mouse(vis.node())
    lenses.attr("cx", m[0])
      .attr("cy", m[1])

    data.steps.forEach (s) ->
      s.scale = 1

    rc = vis.node().createSVGRect()
    rc.x = m[0] - lenses_r
    rc.y = m[1] - lenses_r
    rc.width = m[0] + lenses_r
    rc.height = m[1] + lenses_r
    il = vis.node().getIntersectionList(rc, nodesG.node())
    ps = (x.parentNode for x in il).unique()
    ps.forEach (p) ->
      datum = d3.select(p).datum()
      px = datum.x
      py = datum.y
      scale = Math.min(50, Math.sqrt((px - m[0])**2 + (py-m[1])**2))
      datum.scale = 2 - scale/50
    
    nodesG.selectAll("svg")
      .attr("width", (d) -> d.scale * 2*thumbnail)
      .attr("height", (d) -> d.scale * thumbnail)

  addPlot = (step) ->
    plot = vis.append("svg")
    if step.contents
      plot.append("image")
        .attr("width", 50)
        .attr("height", 50)
        .attr("xlink:href", $("#plot#{step.id}").attr("href"))
    else
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
