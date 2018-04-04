class HistoryBrowser
  constructor: (container, @data) ->
    @container = $(container)
    @frame = $("<div>", {id: "details-container"})
      .load $("#browser-1-attachment").attr("href"), null, () -> console.log('done')

  setData: (data) ->
    @data = data

  render: () ->
    for step in @data.sequence()
      console.log(step)
      frame = @frame.clone()
      frame.find("code")
        .text(step.expr)
        .each (i, block) -> hljs.highlightBlock(block)
      @container.append(frame)



  initialize = () ->
    outer.width(width)
      .appendTo(selection)
      .height(height - parseInt(outer.css("top")))

    if step.type is "object"
      outer.find(".image").remove()
      outer.find(".name").text(step.name)
      outer.find(".description").text(step.desc)
    else
      outer.find(".object").remove()
      outer.find(".image img")
        .attr("src", plotHref(step))
        .on('load', () -> $(this).width(Math.min(width, this.width)))

    # add code describing this step

    # handle comment
    comment = outer.find(".comment")
    comment.on 'keydown', (e) -> e.stopPropagation()
      .on 'keyup', (e) ->
        step.comment = this.value
        clearTimeout(this.commentUpdate)
        cb = () -> commentCallback(step.id, step.comment)
        this.commentUpdate = setTimeout(cb, 3000)
        e.stopPropagation()
    
    if step.comment
      comment.text(step.comment).removeClass("empty")
    else
      initial = comment.attr("initial")
      outer.find(".comment")
        .text(initial)
        .focus () -> $(this).text("").removeClass("empty")
        .focusout () -> if not this.value then $(this).addClass("empty").text(initial)



window.HistoryBrowser = HistoryBrowser