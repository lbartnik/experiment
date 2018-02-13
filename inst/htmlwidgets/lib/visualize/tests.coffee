registerTests = () ->
  assert = chai.assert
  suite  = Mocha.suite
  test   = Mocha.test

  sampleData = (path = 'data-1/data.json') ->
    req = new XMLHttpRequest()
    req.open("GET", path, false)
    req.overrideMimeType('application/json')
    req.send()
    if req.status isnt 200
      throw 'Cannot load sample data ' + path
    JSON.parse(req.responseText)


  suite 'Data', () ->
    # helpers
    extractScale = (steps) -> step.scale for step in steps when step.scale isnt undefined
    unique = (a) ->
      output = {}
      (output[a[key].id ? a[key]] = a[key]) for key in [0...a.length]
      value for key, value of output

    # sample data
    setup () ->
      this.data = sampleData()

    # test cases
    suite 'Data construction', () ->
      test 'data sanity', () ->
        assert('links' of this.data)
        assert('steps' of this.data)
        assert.lengthOf(this.data.steps, 16)
        assert.lengthOf(extractScale(this.data.steps), 0)
        assert.sameMembers(unique(step.id isnt undefined for step in this.data.steps), [true])
        assert.sameMembers(unique(typeof step.expr for step in this.data.steps), ['object', 'string'])

      test 'maintains keys', () ->
        sd = Data(this.data)
        assert('steps' of sd)
        assert('links' of sd)

      test 'sets scale', () ->
        sd = Data(this.data)
        assert.lengthOf(sd.steps, 16)
        assert.lengthOf(extractScale(sd.steps), 16)
        assert.sameMembers(unique(extractScale(sd.steps)), [1])

      test 'concat expression', () ->
        sd = Data(this.data)
        assert.sameMembers(unique(typeof step.expr for step in sd.steps), ['string'])

      test 'replace id with object', () ->
        sd = Data(this.data)
        sd.links.forEach (link) ->
          assert.hasAllKeys(link, ['target', 'source'])
          assert.includeDeepMembers(sd.steps, [link.target])

window.registerTests = registerTests
