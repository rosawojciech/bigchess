#' Sending command to chess engine
#'
#' Sending command to chess engine
#'
#' @param engine engine object
#' @param command string command
#' @return engine object
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' e <- uci_command(e,"go depth 10")
#' uci_quit(e)
#' # Using pipe '%>%' from magrittr:
#' require(magrittr)
#' uci_engine(engine_path) %>% uci_command("go depth 10") %>% uci_quit()}
#' @export
#' @importFrom subprocess process_write
uci_cmd <- function(engine, command = ""){
  process_write(engine$pipe,paste0(command,"\n"))
  return(engine)
}
