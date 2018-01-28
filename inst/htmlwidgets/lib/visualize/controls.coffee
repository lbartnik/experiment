Controls = (selection, min = .5, max = 2, step = 1.1) ->
  outer    = null
  plus     = null
  minus    = null
  current  = 1
  callback = null

  controls = () ->

  controls.initialize = () ->
    outer = $("<div>", {class: "controls"}).appendTo(selection)
    plus  = $("<div>", {class: "button", id: "plus"}).appendTo(outer).text("+")
    minus = $("<div>", {class: "button", id: "minus"}).appendTo(outer).text("-")

    plus.on('click', () -> zoom(current / step))
    minus.on('click', () -> zoom(current * step))

    zoomer = d3.zoom()
      .scaleExtent([min, max])
      .on("zoom", () -> zoom(1/d3.event.transform.k))
    d3.select(selection).select("#plus").call(zoomer).on("dblclick.zoom", null)
    d3.select("#minus").call(zoomer).on("dblclick.zoom", null)

  controls.on = (event, fn) ->
    if event is 'zoom'
      callback = fn
 
  zoom = (k) ->
    k = Math.max(min, Math.min(max, k))
    if k is current then return
    current = k
    if callback
      callback(k)

  controls.initialize()
  return controls

window.Controls = Controls
