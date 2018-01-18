PopUp = (selection, text) ->
    outer  = null
    dialog = null

    popup = () ->
    popup.initialize = () ->
        outer = $("<div/>")
            .addClass("popup-background")
            .appendTo($(selection))
        dialog = $("<div/>")
            .addClass("popup-dialog")
            .appendTo(outer)
        $("<div>")
            .html(text)
            .appendTo(dialog)
        $("<input>", { type: 'button', value: 'OK' })
            .appendTo(dialog)
        
        outer.click(() -> close())
        dialog.click(() -> )

    close = () ->
        outer.remove()

    popup.initialize()
    return popup


window.PopUp = PopUp
