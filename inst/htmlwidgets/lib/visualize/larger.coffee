Array::unique = () ->
      output = {}
      (output[@[key].id ? @[key]] = @[key]) for key in [0...@length]
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


Viewport = (selection) ->
  viewport = () ->
  viewport.size = () ->
    # actual viewport; in R Studio, when running as AddIn, this is somehow
    # distorted and reports size larger than the actual viewport area
    w = Math.max(document.documentElement.clientWidth, window.innerWidth || 0)
    h = Math.max(document.documentElement.clientHeight, window.innerHeight || 0)
    # thus, we compare it with the size of the enclosing HTML element and
    # choose whatever is smaller
    w = Math.min(w, $(selection).width())
    h = Math.min(h, $(selection).height())
    {width: w, height: h}
  return viewport

# returns:
#   - the embedded image, if contents present
#   - the image from link, if can be found
#   - a grey 30x30 png, if nothing else works
plotHref = (step) ->
  if step.contents
    return "data:image/png;base64,#{step.contents}"
  from_id = $("#plots-#{step.id}-attachment").attr("href")
  if from_id
    return from_id
  return "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAIAAAC0Ujn1AAAACXBIWXMAAAsTAAALEwEAmpwY\nAAAAB3RJTUUH4gEMEg8VFQkJGwAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJ\nTVBkLmUHAAAAKUlEQVRIx+3MMREAAAgEILV/mI9oChcPAtBJ6sbUGbVarVar1Wr1/3oBRm8C\nTEfLR0EAAAAASUVORK5CYII="


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
        element.append("circle")
          .attr("cx", innerR)
          .attr("cy", innerR)
          .attr("r", innerR * .96)
        text = element.append("text")
          .attr("class", "label")
          .attr("text-anchor", "middle")
          .attr("alignment-baseline", "middle")
          .attr("y", '50%')
          .attr("x", '50%')
          .text((d) -> d.name)
        text.style('font-size', scaleText(text))
        element.append("rect")
          .attr("class", "face")
          .attr("width", 2*innerR)
          .attr("height", 2*innerR)
      else # type == plot
        element.append("image")
          .attr("width", 2*innerR)
          .attr("height", 2*innerR)
          .attr("xlink:href", plotHref(d))
    node.exit().remove()
    
    link = linksG.selectAll("line.link")
      .data(data.links, (d) -> "#{d.source.id}_#{d.target.id}")
    link.enter().append("line")
      .attr("class", "link")
      .attr("stroke", "#ddd")
    link.exit().remove()
  # --- createGraphics

  # make sure text fits inside the node icon
  scaleText = (text) ->
    textWidth = text.node().getBoundingClientRect().width
    fontSize  = parseFloat(text.style('font-size'))
    fontSize  = Math.min(12, fontSize * (innerR*1.6/textWidth))
    "#{fontSize}px"

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
    if event in ['node:mouseover', 'node:mouseout', 'node:click']
      nodesG.selectAll(".face,image")
        .on(event.substring(5), callback)

  ui.initialize()
  return ui
# --- UI ---------------------------------------------------------------

Data = (data) ->

  resetScale = () ->
    data.steps.forEach (s) ->
      s.scale = 1
  data = {resetScale: resetScale, data...}

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

Description = (element, step, outer, viewport) ->

  description = () ->
  description.show = () ->
    tooltip = $("<div>").addClass("tooltip").attr("id", "tooltip_#{step.id}")
    
    if step.type is "object"
      $("<span>").addClass("name").appendTo(tooltip).text(step.name)
      $("<span>").addClass("description").appendTo(tooltip).text(step.desc)
    else
      # 35 for the code
      height = Math.min(300, viewport.size().height - 65)
      # an image needs to be first loaded, before its dimensions and final
      # position can be calculated
      $("<img>", { src: plotHref(step), height: height })
        .appendTo(tooltip)
        .on('load', () -> position(element, tooltip))

    # add code describing this step
    $("<pre>").appendTo(tooltip).append $("<code>").addClass("R").text(step.expr)
    tooltip.find("pre code").each (i, block) -> hljs.highlightBlock(block)

    # regular object can be created and positioned right away
    if step.type is 'object'
      position(element, tooltip)
  
    # show
    element.tooltip?.remove()
    element.tooltip = tooltip.fadeTo('fast', 1)

  description.hide = () ->
    element.tooltip?.fadeTo('fast', 0, () -> element.tooltip?.remove())

  position = (element, tooltip) ->
    # append the <div> and collect its dimensions to see if it needs to
    # be moved up or to the right
    tooltip
      .css({left: 0, top: 0})
      .appendTo(outer)

    bcr  = tooltip.get(0).getBoundingClientRect()
    node = element.getBoundingClientRect()
    dx = Math.max(node.right + bcr.width - viewport.size().width, 0)
    dy = Math.max(node.bottom + bcr.height - viewport.size().height, 0)

    # place where it can be seen, move if necessary by [dx, dy]
    left = node.right - dx
    top  = node.bottom - dy
    tooltip.css({left: left, top: top})

    # when running as R Studio AddIn, viewport gets messed up, so here
    # we perform one more adjustment: if the actual BCR is moved according
    # top the requested (left, top) we move it again by the difference, in
    # the hope that this will finally place it withing the visible viewport
    bcr = tooltip.get(0).getBoundingClientRect()
    left += (left - bcr.left)
    top  += (top - bcr.top)

    tooltip
      .css({visibility: "visible", left: left, top: top})


  return description



# --- Widget -----------------------------------------------------------
Widget = (selection) ->
  options = { shiny: false }
  nodeR   = 15
  lenseR  = 30
  ui      = UI(selection, nodeR, 15)
  pos     = Position(500, 500, nodeR)
  data    = null

  widget = () ->

  widget.setData = (input) ->
    data = Data(input)
    ui.setData(data)
    updateCanvas()
    setEvents()

  widget.setSize = (width, height) ->
    ui.setSize(width,height)
    pos = Position(width, height, nodeR)
    updateCanvas()

  widget.setOption = (what, value) ->
    if what of options
      value = (options[what].constructor)(value)
      options[what] = value

  updateCanvas = () ->
    if data
      pos.calculate(data)
      ui.updatePositions()


  setEvents = () ->
    ui.on('canvas:mousemove', moveLenses)
    ui.on('canvas:mouseout', resetScale)
    ui.on('node:mouseover', showDialog)
    ui.on('node:mouseout', hideDialog)
    ui.on('node:click', clickNode)

  moveLenses = (d) ->
    data.resetScale()
    mouse  = ui.mousePosition()
    nodes = ui.nodesNear(mouse, lenseR)
    nodes.forEach (n) ->
      datum = d3.select(n).datum()
      scale = euclidean(mouse, datum)/lenseR
      datum.scale = 1 + lenseR/nodeR * (1-scale)**3
    ui.updatePositions()
  
  resetScale = (d) ->
    data.resetScale()
    ui.updatePositions()
  
  showDialog = (d) ->
    this.description = Description(this, d, selection, Viewport(selection))
    this.description.show()
  
  hideDialog = (d) ->
    this.description.hide()

  clickNode = (d) ->
    if options.shiny
      Shiny.onInputChange("object_selected", d.id)

  return widget

# export the Widget
window.Widget = Widget
