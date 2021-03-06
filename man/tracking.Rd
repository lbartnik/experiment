% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track.R
\name{tracking_on}
\alias{tracking_on}
\alias{tracking_off}
\title{Turn tracking on or off}
\usage{
tracking_on(path = file.path(getwd(), "project-store"), .global = "abort")

tracking_off()
}
\arguments{
\item{path}{Where to locate the object store (see \link[storage:object_store]{storage::object_store}).}

\item{.global}{How to handle \link{globalenv} when it is not empty.}
}
\description{
\code{tracking_on} turns the tracking mode on, which is
signaled by a new prompt, \code{[tracked] >}. It also attaches to an
object store (see \link[storage:object_store]{storage::object_store}), if one can be found under
\code{path}. If no object store can be found, and \code{path} points to a
non-existing directory whose parent directory does exist, then that
top-level directory is created and a new object store is created in
it.

\code{tracking_off} reverses the effect of \code{tracking_on}. It
removes the callback and brings back the original value of that R
session's prompt.
}
\details{
When an existing object store is found, and it is not empty,
that is, it contains artifacts and \link{commit}s from previous R sessions,
the current R session is set as a continuation of one of those
\code{commit}s. However, if the current \emph{global environment} (see
\link{globalenv}) is not empty, it needs to be replaced or merged with the
chosen \code{commit}. To that extent, the \code{.global} argument is consulted.
It can take one of the following values:
\itemize{
\item \code{"abort"} - the default, aborts the tracking of \code{globalenv} is not
empty
\item \code{"replace"} - replace the contents of \code{globalenv} with the chosen
commit
\item \code{"merge"} - merge the contents of \code{globalenv} with the chosen commit;
this creates a new commit in the process, which is immediately written
back to the object store
}

When tracking is enabled a task callback installed via
\link{addTaskCallback}. It is used to examine the contents of the
\emph{global environment}  each time an R command is successfully executed.
}
\examples{
\dontrun{
# if no object store exists, a new one is created under the
# default "project-store" directory located in the current
# working directory
tracking_on()

# as above, but the new directory is "my-store"
tracking_on("my-store")
}

}
