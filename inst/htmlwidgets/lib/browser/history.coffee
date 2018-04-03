class HistoryBrowser
    constructor: (container, @data) ->
        @container = $(container)
    
    setData: (data) ->
        @data = data

    render: () ->
        for step in @data.sequence()
            @container.append()



window.HistoryBrowser = HistoryBrowser