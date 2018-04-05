class Data
  # The Data object owns the @raw data passed to it.
  #
  constructor: (@raw) ->
    @preprocess()

  # Starting with the root, forms an ordered path through the graph.
  # If there is more than one child at any level, throws an exception.
  #
  sequence: () ->
    head = @treed()
    seq = []
    while head.children?.length is 1
      seq.push(head.data)
      head = head.children[0]
    if head.children?.length > 1
      throw "Node " + head.id + " has more than one child"
    seq.push(head.data)
    seq

  # Getters.
  #
  steps: () -> @raw.steps
  links: () -> @raw.links

  # Replace target/source identifiers in the @raw.links array with
  # actual objects.
  #
  preprocess: () ->
    stepsMap = d3.map()
    @raw.steps.forEach (n) ->
      stepsMap.set(n.id, n)

    @raw.links.forEach (l) ->
      l.source = stepsMap.get(l.source)
      l.target = stepsMap.get(l.target)

    @raw.steps.forEach (s) ->
      if s.expr?.constructor is Array
        s.expr = s.expr.join('\n')


  # Turns the @raw data structure into a d3.stratified hierarchy.
  #
  stratified: () ->
    parents = d3.map()
    @raw.links.forEach (l) ->
      parents.set(l.target.id, l.source.id)
    stratify = d3.stratify()
      .id((d) -> d.id)
      .parentId((d) -> parents.get(d.id))
    stratify(@raw.steps)

  # Computes positions in d3.stratified hierarchy assuming nodes
  # form a tree.
  #
  treed: (spacing = 25) ->
    tree = d3.tree()
      .nodeSize([spacing, spacing])
    stfd = @stratified()
    stfd.sort()
    tree(stfd)


window.Data = Data