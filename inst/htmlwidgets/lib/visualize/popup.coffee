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
            .click(clicked)
        
        outer.click(clicked)
        dialog.click(() -> false)

    popup.show = (message) ->
        text.html(message)
        outer.css('visibility', 'visible')

    popup.hide = clicked

    clicked = () ->
        outer.remove()
        Shiny.onInputChange("popup_clicked", true)

    popup.initialize()
    return popup


window.PopUp = PopUp
