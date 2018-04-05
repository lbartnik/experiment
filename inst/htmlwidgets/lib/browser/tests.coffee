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
        @raw = sampleData
        @data = new Data(JSON.parse(JSON.stringify(sampleData)))

      test 'data sanity', () ->
        assert('links' of @raw)
        assert('steps' of @raw)
        assert.lengthOf(@raw.steps, 4)
        assert.sameMembers(unique(step.id isnt undefined for step in @raw.steps), [true])
        assert.sameMembers(unique(typeof step.expr for step in @raw.steps), ['object', 'string'])

      test 'concat expression', () ->
        assert.sameMembers(unique(typeof step.expr for step in @data.steps()), ['string'])

      test 'replace id with object', () ->
        steps = @data.steps()
        @data.links().forEach (link) ->
          assert.hasAllKeys(link, ['target', 'source'])
          assert.includeDeepMembers(steps, [link.target])

      test 'sequence', () ->


  return null

window.registerTests = registerTests
