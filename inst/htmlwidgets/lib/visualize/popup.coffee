PopUp = (selection) ->
    outer  = null
    dialog = null
    text   = null

    popup = () ->
    popup.initialize = () ->
        outer = $("<div/>")
            .addClass("popup-background")
            .appendTo($(selection))
        dialog = $("<div/>")
            .addClass("popup-dialog")
            .appendTo(outer)
        text = $("<div>")
            .appendTo(dialog)
        $("<input>", { type: 'button', value: 'OK' })
            .appendTo(dialog)
            .click(remove)
        
        outer.click(remove)
        dialog.click(() -> false)

    popup.show = (message) ->
        text.html(message)
        outer.css('visibility', 'visible')

    popup.hide = remove

    remove = () -> outer.remove()

    popup.initialize()
    return popup


window.PopUp = PopUp
