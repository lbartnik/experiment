Controls = (selection) ->
  outer = null
  plus  = null
  minus = null

  controls = () ->

  controls.initialize = () ->
    outer = $("<div>", {class: "controls"}).appendTo(selection)
    plus  = $("<div>", {class: "button", id: "plus"}).appendTo(outer).text("+")
    minus = $("<div>", {class: "button", id: "minus"}).appendTo(outer).text("-")

  controls.on = (event, callback) ->
    if event is 'zoom:in'
      plus.on('click', callback)
    if event is 'zoom:out'
      minus.on('click', callback)
    if event is 'zoom'
      zoom = d3.zoom()
        .scaleExtent([.1, 2])
        .on("zoom", () -> callback(1/d3.event.transform.k))
      d3.select("#plus").call(zoom).on("dblclick.zoom", null)
      d3.select("#minus").call(zoom).on("dblclick.zoom", null)

  controls.initialize()
  return controls



window.Controls = Controls
