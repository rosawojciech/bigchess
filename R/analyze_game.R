utils::globalVariables(c("LAN"))
#' Analyze game
#'
#' Analyze game using UCI engine and R API
#' @param engine engine path or engine object from uci_engine()
#' @param san movetext in short algebraic notation, default NULL
#' @param lan movetext in long algebraic notation, default NULL
#' @param quiet boolean, hide system messages? Default FALSE
#' @param ... further arguments passed directly to uci_go(), i.e. depth = 10
#' @return list containg analyze_position() result (score and bestlines) for each move in the game. Note that if black moves, then score is multiplied by -1.
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' g <- "1. e4 e5 2. Nf3 Nc6 3. d4 exd4 4. Bc4 Nf6 5. O-O Be7"
#' G <- analyze_game(engine_path,san = g ,depth = 20)
#' G[[1]] # handles info about first move in the game
#' G[[1]]$comment # "book"
#' G[[10]]$curmove_san # "Be7"
#' G[[10]]$score # 62
#' }
#' @export
analyze_game <- function (engine, san = NULL, lan = NULL,quiet = FALSE, ...)
{
  if (class(engine) == "character")
    e <- uci_engine(path = engine)
  else e <- engine
  if (!is.null(san))
    lan <- san2lan(san)
  if (!is.null(lan))
    san <- lan2san(lan)

  P <- list()

  guci <- lan
  gucil <- strsplit(guci," ")[[1]]

  gsan <- strsplit(gsub("[0-9]+\\. ","",san)," ")[[1]]
  for(i in 1: length(gucil))
  {
    t <- list()
    t$curmove_lan <- gucil[i]
    t$curmove_san <- gsan[i]
    t$curpos_lan <- paste0(gucil[1:i],collapse = " ")
    t$curpos_san <- lan2san(t$curpos_lan)
    if(nrow(subset(bigchess::eco,LAN == t$curpos_lan))==0){
      e <- uci_position(e, moves = t$curpos_lan)
      e <- uci_go(e, ...)
      t$ucilog <- uci_read(e)$temp
      t$score <-  uci_parse(t$ucilog, "score")
      if(i %% 2) t$score <- -t$score
      t$bestmove <- uci_parse(t$ucilog)
      t$bestline_lan <- uci_parse(t$ucilog, "bestline")
      l2s <- lan2san(paste(t$curpos_lan,t$bestline_lan))
      t$bestline_san <- substr(l2s,nchar(t$curpos_san)+2,nchar(l2s))
      t$bestmove_san <- strsplit(gsub("[0-9]+\\. ","",t$bestline_san)," ")[[1]][1]
      t$comment <- ""
    }
    else{ t$comment <- "book"}
    P[[i]] <- t
    if(!quiet) message(paste("Move",i,Sys.time()))
  }
  uci_quit(e)
  if(!quiet) message("Done!")
  return(P)
}
