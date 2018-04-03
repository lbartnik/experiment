window.isShiny = () ->
    typeof HTMLWidgets is not 'undefined' && HTMLWidgets.shinyMode

# --- simple logger ----------------------------------------------------
Log = () ->
  enabled = false

  callerName = () ->
    re = /([^(]+)@|at ([^(]+) \(/gm
    st = new Error().stack
    re.exec(st) # skip 1st line
    re.exec(st) # skip 2nd line
    re.exec(st) # skip 3rd line
    res = re.exec(st)
    if not res then return "unknown"
    return res[1] || res[2]
  
  showMessage = (level, message) ->
    if not enabled then return
    caller = callerName()
    console.log("#{level} #{caller}: #{message}")

  log = () ->
  log.debug = (message) -> showMessage("DEBUG", message)
  log.info  = (message) -> showMessage("INFO ", message)

  log.enable = (onoff) ->
    enabled = onoff

  return log

window.log = Log()



