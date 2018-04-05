window.utils = {}

window.utils.isShiny = () ->
    typeof HTMLWidgets is not 'undefined' && HTMLWidgets.shinyMode

# returns:
#   - the embedded image, if contents present
#   - the image from link, if can be found
#   - a grey 30x30 png, if nothing else works
window.utils.plotHref = (step) ->
  if step.contents
    return "data:image/png;base64,#{step.contents}"
  from_id = $("#plots-#{step.id}-attachment").attr("href")
  if from_id
    return from_id
  return "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAIAAAC0Ujn1AAAACXBIWXMAAAsTAAALEwEAmpwY\nAAAAB3RJTUUH4gEMEg8VFQkJGwAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJ\nTVBkLmUHAAAAKUlEQVRIx+3MMREAAAgEILV/mI9oChcPAtBJ6sbUGbVarVar1Wr1/3oBRm8C\nTEfLR0EAAAAASUVORK5CYII="


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



