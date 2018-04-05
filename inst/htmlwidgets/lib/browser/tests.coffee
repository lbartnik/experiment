registerTests = (sampleData) ->
  assert = chai.assert
  suite  = Mocha.suite
  test   = Mocha.test

  # check that basic CoffeeScript idioms work as expected
  suite 'Idioms', () ->
    test 'clone array and elements', () ->
      x = [{x: 1}, {x: 2}]
      y = x.map (e) -> {e...}
      y[0].x = 3
      assert.equal(x[0].x, 1)


  suite 'Data', () ->
    # helpers
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
        assert.sameMembers(unique(step.id isnt undefined for step in this.data.steps), [true])
        assert.sameMembers(unique(typeof step.expr for step in this.data.steps), ['object', 'string'])

      test 'concat expression', () ->
        sd = new Data(this.data)
        assert.sameMembers(unique(typeof step.expr for step in sd.steps()), ['string'])

      test 'replace id with object', () ->
        sd = new Data(this.data)
        sd.links().forEach (link) ->
          assert.hasAllKeys(link, ['target', 'source'])
          assert.includeDeepMembers(sd.steps(), [link.target])

  return null

window.registerTests = registerTests
