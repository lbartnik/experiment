% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track.R
\name{task_callback}
\alias{task_callback}
\alias{task_callback}
\alias{update_current_commit}
\title{A callback run after each top-level expression is evaluated.}
\usage{
task_callback(expr, result, successful, printed)

update_current_commit(state, env, plot, expr)
}
\arguments{
\item{expr}{Expression for the top-level task.}

\item{result}{Result of the top-level task.}

\item{successful}{A logical value indicating whether it was
successfully completed or not (always \code{TRUE} at present).}

\item{printed}{A logical value indicating whether the result was
printed or not.}

\item{state}{Global state, an \code{\link[=environment]{environment()}}; passed as a parameter
for testing purposes.}

\item{env}{Environment this commits represents.}

\item{plot}{The last plot (see \code{\link[=recordPlot]{recordPlot()}}).}
}
\value{
A logical value indicating whether to keep this function in
the list of active callbacks.
}
\description{
From \code{\link[=addTaskCallback]{addTaskCallback()}}: if the data argument was specified in
the call to addTaskCallback, that value is given as the fifth
argument.

\code{update_current_commit} is a part of \code{task_callback}
made separate due to testing purposes.
}
