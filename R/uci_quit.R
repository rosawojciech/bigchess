#' Sending quit command to chess engine
#'
#' Sending quit command to chess engine and cleaning temps from R
#'
#' @param engine engine object
#' @return strings from uci chess engine GUI
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' uci_quit(e)
#' # Using pipe '%>%' from magrittr:
#' require(magrittr)
#' uci_engine(engine_path) %>% uci_quit()}
#' @export
uci_quit <- function(engine){
  uci_cmd(engine,"quit")
  # process_close_input(engine$pipe) #subprocess

  rslt <- uci_read(engine)$temp
  engine$pipe$kill()
  return(rslt)
}
