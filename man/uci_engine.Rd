% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uci_engine.R
\name{uci_engine}
\alias{uci_engine}
\title{Create an engine handler in R}
\usage{
uci_engine(path)
}
\arguments{
\item{path}{path to engine file. Make sure you have executable permission on this file.}
}
\value{
engine object (list of two: pipe to engine and temp as a result from stdout engine)
}
\description{
Create an engine handler in R and send command isready
}
\examples{
\donttest{
# Linux (make sure you have executable permission):
engine_path <- "./stockfish_10_x64"
# Windows
# engine_path <- "./stockfish_10_x64.exe"
e <- uci_engine(engine_path)
uci_quit(e)

# Using pipe '\%>\%' from magrittr:
require(magrittr)
uci_engine(engine_path) \%>\% uci_quit()}
}
