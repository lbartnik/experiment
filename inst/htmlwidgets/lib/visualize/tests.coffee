registerTests = (sampleData) ->
  assert = chai.assert
  suite  = Mocha.suite
  test   = Mocha.test

  suite 'Idioms', () ->
    test 'clone array and elements', () ->
    x = [{x: 1}, {x: 2}]
    y = x.map (e) -> {e...}
    y[0].x = 3
    assert.equal(x[0].x, 1)


  suite 'Data', () ->
    # helpers
    extractScale = (steps) -> step.scale for step in steps when step.scale isnt undefined
    unique = (a) ->
      output = {}
      (output[a[key].id ? a[key]] = a[key]) for key in [0...a.length]
      value for key, value of output

    # test cases
    suite 'Data construction', () ->
      # sample data
      setup () ->
        this.data = JSON.parse(JSON.stringify(sampleData))

      test 'data sanity', () ->
        assert('links' of this.data)
        assert('steps' of this.data)
        assert.lengthOf(this.data.steps, 4)
        assert.lengthOf(extractScale(this.data.steps), 0)
        assert.sameMembers(unique(step.id isnt undefined for step in this.data.steps), [true])
        assert.sameMembers(unique(typeof step.expr for step in this.data.steps), ['object', 'string'])

      test 'maintains keys', () ->
        sd = Data(this.data)
        assert('steps' of sd)
        assert('links' of sd)

      test 'sets scale', () ->
        sd = Data(this.data)
        assert.lengthOf(sd.steps, this.data.steps.length)
        assert.lengthOf(extractScale(sd.steps), this.data.steps.length)
        assert.sameMembers(unique(extractScale(sd.steps)), [1])

      test 'concat expression', () ->
        sd = Data(this.data)
        assert.sameMembers(unique(typeof step.expr for step in sd.steps), ['string'])

      test 'replace id with object', () ->
        sd = Data(this.data)
        sd.links.forEach (link) ->
          assert.hasAllKeys(link, ['target', 'source'])
          assert.includeDeepMembers(sd.steps, [link.target])
      
      test 'filter elements', () ->
        sd = Data(this.data)
        sd.filter("x")
        assert.lengthOf(sd.steps, 3) # two regular nodes + artificial head

      test 'filter head', () ->
        sd = Data(this.data)
        sd.filter("y")
        assert.lengthOf(sd.steps, 2) # two regular nodes + artificial head
        assert.lengthOf(sd.links, 1)

      test 'remove filter', () ->
        sd = Data(this.data)
        sd.filter("y")
        sd.filter("")
        assert.deepEqual(sd.steps, this.data.steps)
        assert.deepEqual(sd.links, this.data.links)
      
      test 'd3.tree with filter', () ->
        sd = Data(this.data)
        sd.filter("x")
        sd.stratified()

  return null

window.registerTests = registerTests
