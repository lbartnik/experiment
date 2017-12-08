Widget = (selection) ->

  Data = null
  Vis  = d3.select(selection).append("svg")
  
  widget = () ->
  widget.setData = (data) ->
    Data = data
    dec = atob(data.steps[2].contents)
    parser = new DOMParser()
    doc = parser.parseFromString(dec, "application/xml")
    plot = Vis.node()
              .appendChild(doc.documentElement)
    bb = plot.getBBox()
    d3.select(plot)
      .attr("viewBox", "#{bb.x} #{bb.y} #{bb.width} #{bb.height}")
      .attr("width", 150)
    console.log(plot.getBBox())
  
  widget.setSize = (width, height) ->
    Vis.attr("width", width)
       .attr("height", height)

  widget

window.Widget = Widget