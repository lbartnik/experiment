#' @importFrom scales ordinal
branch_name <- function (no) {
  switch(no,
    `1` = 'first',
    `2` = 'second',
    `3` = 'third',
    `4` = 'fourth',
    `5` = 'fifth',
    `6` = 'sixth',
    `7` = 'seventh',
    `8` = 'eigth',
    `9` = 'ninth',
    ordinal(no)
  )
}


count_branches <- function (storage)
{
  cmts <- read_commits(storage)
  branches <- vapply(cmts, `[[`, numeric(1), 'branch')
  length(unique(branches))
}

