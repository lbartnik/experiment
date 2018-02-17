Controls = (selection, min = .5, max = 2, step = 1.1) ->
  outer    = null
  plus     = null
  minus    = null
  search   = null
  current  = 1
  callback = null
  keys =
    enter: null
    up:    null
    down:  null
    left:  null
    right: null

  controls = () ->

  controls.initialize = () ->
    outer = $("<div>", {class: "controls"}).appendTo(selection)
    plus  = $("<div>", {class: "button", id: "plus"}).appendTo(outer).text("+")
    minus = $("<div>", {class: "button", id: "minus"}).appendTo(outer).text("-")
    search = $("<input>", {id: "search", class: "search", type: "text"}).appendTo(outer)

    plus.on('click', () -> zoom(current / step))
    minus.on('click', () -> zoom(current * step))

    zoomer = d3.zoom()
      .scaleExtent([min, max])
      .on("zoom", () -> zoom(d3.event.transform.k))
    d3.select(selection).select("#plus").call(zoomer).on("dblclick.zoom", null)
    d3.select(selection).select("#minus").call(zoomer).on("dblclick.zoom", null)

    $(window).on 'keydown', keyDown
    $('iframe', parent.document).on 'keydown', keyDown

  # --- configure events -----------------------------------------------
  controls.on = (event, fn) ->
    if event is 'zoom'
      callback = fn
    if event.substring(0,3) is 'key'
      keys[event.substring(4)] = fn

  # --- zooming --------------------------------------------------------
  zoom = (k) ->
    k = Math.max(min, Math.min(max, k))
    if k is current then return
    current = k
    if callback
      callback(k)

  # --- keyboard -------------------------------------------------------

  keyDown = (e) ->
    key = translateKey(e)
    if key of keys and keys[key]
      keys[key](key)
      e.preventDefault()

  # --- translate key to its name ---
  translateKey = (e) ->
    Codes =
      13: "enter",
      37: "left",
      38: "up",
      39: "right",
      40: "down"
    keyCode = e.originalEvent?.keyCode
    if keyCode of Codes then return Codes[keyCode]

    return null
  # --------------------------------------------------------------------

  controls.initialize()
  return controls

window.Controls = Controls
