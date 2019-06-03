#' Sending command stop for chess engine
#'
#' Sending command stop for chess engine. Info about stop command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' stop calculating as soon as possible, don't forget the "bestmove" and possibly the "ponder" token when finishing the search
#' @param engine engine object
#' @return engine object
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' e <- uci_go(depth = 100)
#' Sys.sleep(1)
#' e <- uci_stop(e)
#' uci_quit(e)}
#' @export
uci_stop <- function(engine){
  return(uci_cmd(engine,"stop"))
}
