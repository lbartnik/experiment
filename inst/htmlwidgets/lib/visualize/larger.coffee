Array::unique = () ->
  output = {}
  (output[@[key].id ? @[key]] = @[key]) for key in [0...@length]
  value for key, value of output

Array::min = () ->
  @.reduce((a,b) -> Math.min(a, b))

Array::max = () ->
  @.reduce((a,b) -> Math.max(a, b))

if Math.sign is undefined
  sign = (x) -> if x < 0 then -1 else 1
else
  sign = Math.sign

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
  sizes  = { ui: { width: 500, height: 500}, canvas: {width: 500, height: 500}}
  data   = null

  ui = () ->

  ui.initialize = () ->
    outer = d3.select(selection)
      .append("div")
      .attr("class", "widget")
    canvas = outer.append("svg")
    linksG = canvas.append("g").attr("id", "links")
    nodesG = canvas.append("g").attr("id", "nodes")
  
  ui.setSize = (width, height) ->
    sizes.ui.width  = width
    sizes.ui.height = height
    # reduce the size to make sure scrolls don't show right away
    outer.style("width", width - 10)
      .style("height", height - 10)

  ui.setData = (Data) ->
    data = Data
    recalculateCanvas(data)
    createGraphics(data)

  # create all graphical elements on the canvas
  createGraphics = (data) ->
    node = nodesG.selectAll("svg.variable")
      .data(data.steps, (d) -> d.id)
    enter = node.enter().append("svg")
      .attr("class", (d) -> "variable #{d.type}")
      .attr("id", (d) -> "node_#{d.id}")
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
          .attr("xlink:href", plotHref(d))
        element.append("rect")
        element.append("rect")
          .classed("face", true)
        element.selectAll("image,rect")
          .attr("width", 2*innerR)
          .attr("height", 2*innerR)
    node.exit().remove()
    
    link = linksG.selectAll("line.link")
      .data(data.links, (d) -> "link_#{d.source.id}_#{d.target.id}")
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
    recalculateCanvas(data)

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

  # compute canvas size from data
  recalculateCanvas = (data) ->
    x = (step.x for step in data.steps)
    y = (step.y for step in data.steps)
    setCanvasSize(x.min()-nodeR, x.max()+nodeR, y.min()-nodeR, y.max()+nodeR)
    # now sizes.canvas.* is updated an we can update nodes' coordinates
    data.centralize(sizes.canvas.width, sizes.canvas.height)

  # canvas size is set independently, and canvas might need to be
  # scrolled within the outer div element
  setCanvasSize = (xMin, xMax, yMin, yMax) ->
    if isNaN(xMin) or isNaN(xMax) or isNaN(yMin) or isNaN(yMax)
      return
    sizes.canvas.width  = Math.max(sizes.ui.width, xMax - xMin)
    sizes.canvas.height = Math.max(sizes.ui.height, yMax - yMin)
    
    canvas.attr("width", sizes.canvas.width)
      .attr("height", sizes.canvas.height)
      .attr("viewBox", "0 0 #{sizes.canvas.width} #{sizes.canvas.height}")

  # returns mouse position relatively to the SVG canvas
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
      nodesG.selectAll(".face")
        .on(event.substring(5), callback)
  
  # --- graphical node selection ---
  ui.select = (id) ->
    nodesG.selectAll(".variable")
      .classed("selected", false)
    if id
      nodesG.selectAll("#node_#{id}")
        .classed("selected", true)

  ui.initialize()
  return ui
# --- UI ---------------------------------------------------------------

Data = (data) ->

  resetScale = () ->
    data.steps.forEach (s) ->
      s.scale = 1
  
  centralize = (width, height) ->
    x = (step.x for step in data.steps)
    y = (step.y for step in data.steps)
    dx = x.min() - Math.max(width - (x.max() - x.min()), 0) / 2
    dy = y.min() - Math.max(height - (y.max() - y.min()), 0) / 2
    data.steps.forEach (s) ->
      s.x -= dx
      s.y -= dy

  data = {resetScale: resetScale, centralize: centralize, data...}

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

Position = (nodeR) ->
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
      .nodeSize([4*nodeR,4*nodeR])
    tree(data)
  
  position.calculate = (data) ->
    # use d3 to calculate positions for a tree
    s = stratified(data)
    t = treed(s)
    
    dx = t.descendants().map((n) -> n.x).min() - 2 * nodeR
    dy = t.descendants().map((n) -> n.y).min() - 2 * nodeR

    # update original nodes' positions
    nodesMap = mapNodes(data.steps)
    t.each (n) ->
      s = nodesMap.get(n.id)
      s.x = n.x - dx
      s.y = n.y - dy
  # --- calculate ---
  
  # return an instance of the Position object
  position
# --- Position ---------------------------------------------------------

Description = (element, step, outer, viewport, nodeR) ->

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

  # area: potential placement of the tooltip
  # box:  dimensions of the tooltip
  # center: where is the node we want to be close to
  # returns: {left, top, scale}
  compare = (area, box, center) ->
    scalex = (area.right - area.left) / box.width
    scaley = (area.bottom - area.top) / box.height
    scale  = Math.min(scalex, scaley, 1)
    width  = box.width * scale
    height = box.height * scale
    left   = area.left + (area.right - area.left - width)/2
    top    = area.top + (area.bottom - area.top - height)/2
    # direction towards the node
    dx = center.x - (left + width/2)
    dy = center.y - (top + height/2)
    dx = sign(dx) * Math.max(Math.abs(dx) - nodeR - width/2, 0)
    dy = sign(dy) * Math.max(Math.abs(dy) - nodeR - height/2, 0)

    { left: left + dx, top: top + dy, scale: scale }

  asArea = (numbers) ->
    { left: numbers[0], right: numbers[1], top: numbers[2], bottom: numbers[3] }

  position = (element, tooltip) ->
    # append the <div> and collect its dimensions to see if it needs to
    # be moved up or to the right
    tooltip
      .css({left: 0, top: 0})
      .appendTo(outer)

    box  = tooltip.get(0).getBoundingClientRect()
    node = element.getBoundingClientRect()
    view = viewport.size()
    center = {x: node.left + node.width/2, y: node.top + node.height/2}

    left   = compare(asArea([ 0, node.left, 0, view.height ]), box, center)
    right  = compare(asArea([ node.right, view.width, 0, view.height ]), box, center)
    top    = compare(asArea([ 0, view.width, 0, node.top ]), box, center)
    bottom = compare(asArea([ 0, view.width, node.bottom, view.height ]), box, center)
    choice = [left, right, top, bottom].reduce((a, b) -> if a.scale > b.scale then a else b)

    left = choice.left
    top  = choice.top
    tooltip.css({left: left, top: top, transform: "scale(#{choice.scale})"})

    # when running as R Studio AddIn, viewport gets messed up, so here
    # we perform one more adjustment: if the actual BCR is moved according
    # top the requested (left, top) we move it again by the difference, in
    # the hope that this will finally place it withing the visible viewport
    box = tooltip.get(0).getBoundingClientRect()
    left += (left - box.left)
    top  += (top - box.top)

    tooltip
      .css({visibility: "visible", left: left, top: top})


  return description



# --- Widget -----------------------------------------------------------
Widget = (selection) ->
  options = { shiny: false }
  nodeR   = 15
  lenseR  = 30
  ui      = UI(selection, nodeR, 15)
  pos     = Position(nodeR)
  data    = null
  
  widget = () ->

  widget.setData = (input) ->
    data = Data(input)
    ui.setData(data)
    updateCanvas()
    setEvents()

  widget.setSize = (width, height) ->
    ui.setSize(width,height)
    pos = Position(nodeR)
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
    this.description = Description(this, d, selection, Viewport(selection), nodeR)
    this.description.show()
  
  hideDialog = (d) ->
    this.description.hide()

  clickNode = (d) ->
    this.selected = not this.selected
    id = if this.selected then d.id else null

    ui.select(id)
    if options.shiny
      Shiny.onInputChange("object_selected", id)

  return widget

# export the Widget
window.Widget = Widget
