graph_to_frames <- function (graph)
{
  stopifnot(is_graph(graph))

  artifacts <- list()
  frames <- lapply(graph, function (commit) {
    # store artifacts
    mapply(object = commit$objects,
           id     = as.character(commit$object_ids),
           tags   = commit$tags,
      function(object, id, tags)
      {
        id <- crc32(id)
        artifacts[[id]] <<- list(
          id               = id,
          parent_artifacts = vapply(tags$parents, crc32, character(1)),
          parent_commit    = crc32(tags$commit),
          class            = tags$class,
          time             = tags$time
        )
      })
    # return a frame
    list(
      id         = crc32(commit$id),
      artifacts  = lapply(commit$object_ids, crc32),
      parent     = crc32(commit$parent),
      introduced = introduced_in(graph, commit$id),
      expression = format_expression(commit$expr)
    )
  })

  list(frames = unname(frames), artifacts = unname(artifacts))
}
