Controls = (selection) ->
  outer = null
  plus  = null
  minus = null

  controls = () ->

  controls.initialize = () ->
    outer = $("<div>", {class: "controls"}).appendTo(selection)
    plus  = $("<div>", {class: "button"}).appendTo(outer).text("+")
    minus = $("<div>", {class: "button"}).appendTo(outer).text("-")

  controls.on = (event, callback) ->
    if event is 'zoom:in'
      plus.on('click', callback)
    if event is 'zoom:out'
      minus.on('click', callback)


  controls.initialize()
  return controls



window.Controls = Controls
