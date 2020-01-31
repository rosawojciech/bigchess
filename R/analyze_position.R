#' Analyze position
#'
#' Analyze position using UCI engine and R API
#' @param engine engine object
#' @param san movetext in short algebraic notation, default NULL
#' @param lan movetext in long algebraic notation, default NULL
#' @param ... further arguments passed directly to uci_go()
#' @return list containg bestomove, score and bestlines
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' require(magrittr)
#' ap <- analyze_position(engine_path,san = "1. e4",depth = 20)
#' ap$bestmove_lan
#' # "e7e5"
#' ap$score
#' # -7
#' ap$bestmove_san
#' # "e5"
#' ap$curpos_lan
#' # "e2e4"
#' ap$curpos_san
#' # "1. e4"
#' ap$bestline_san
#' # "e5 2. Nf3 Nc6 3. d4 exd4 4. Bc4 Nf6 5. O-O Be7 6. Re1 d6 7. Nxd4 Ne5 8. Bb3 O-O 9. Nc3 c5 10. Nf5 Bxf5 11. exf5 c4 12. Ba4 a6 13. Qe2"
#' ap$bestline_lan
#' # "e7e5 g1f3 b8c6 d2d4 e5d4 f1c4 g8f6 e1g1 f8e7 f1e1 d7d6 f3d4 c6e5 c4b3 e8g8 b1c3 c7c5 d4f5 c8f5 e4f5 c5c4 b3a4 a7a6 d1e2"
#' }
#' @export
analyze_position <- function(engine,san = NULL, lan = NULL, ...){

  if(class(engine)=="character") e <- uci_engine(path = engine)
  else e <- engine
  if(!is.null(san)) lan <- san2lan(san)
  if(!is.null(lan)) san <- lan2san(lan)
  e <- uci_position(e,moves = lan)
  e <-  uci_go(e,...)
  ucilog <-  uci_quit(e)
  r <- list()
  r$bestmove_lan <-  uci_parse(ucilog)
  r$score <- uci_parse(ucilog,"score")
  r$bestline_lan <- uci_parse(ucilog,"bestline")
  r$curpos_lan <- lan
  r$curpos_san <- san
  l2s <- lan2san(paste(r$curpos_lan,r$bestline_lan))
  r$bestline_san <- substr(l2s,nchar(r$curpos_san)+2,nchar(l2s))
  r$bestmove_san <- strsplit(gsub("[0-9]+\\. ","",r$bestline_san)," ")[[1]][1]
  r$comment <- ""
  return(r)
}
