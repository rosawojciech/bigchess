% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uci_position.R
\name{uci_position}
\alias{uci_position}
\title{Sending command position for chess engine}
\usage{
uci_position(engine, moves = NULL, startpos = TRUE, fen = NULL)
}
\arguments{
\item{engine}{engine object}

\item{moves}{string in long algebraic notation}

\item{startpos}{boolean default TRUE}

\item{fen}{string}
}
\value{
engine object
}
\description{
Sending command position for chess engine. Info about position command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
set up the position described in fenstring on the internal board and play the moves on the internal chess board. if the game was played  from the start position the string "startpos" will be sent Note: no "new" command is needed. However, if this position is from a different game than the last position sent to the engine, the GUI should have sent a "ucinewgame" inbetween.
}
\examples{
\donttest{
# Linux (make sure you have executable permission):
engine_path <- "./stockfish_10_x64"
# Windows
# engine_path <- "./stockfish_10_x64.exe"
e <- uci_engine(engine_path)
e <- uci_position(e,moves = "e2e4")
e <- uci_go(e,depth = 10)
uci_quit(e)
# Using pipe '\%>\%' from magrittr:
require(magrittr)
uci_engine(engine_path) \%>\% uci_position(moves = "e2e4") \%>\%
  uci_go(depth = 10) \%>\% uci_quit() \%>\% uci_parse()}
}
