#' Create an engine handler in R
#'
#' Create an engine handler in R and send command isready
#'
#' @param path path to engine file. Make sure you have executable permission on this file.
#' @return engine object (list of two: pipe to engine and temp as a result from stdout engine)
#'
#' @examples
#'\donttest{
#' # Linux (make sure you have executable permission):
#' engine_path <- "./stockfish_10_x64"
#' # Windows
#' # engine_path <- "./stockfish_10_x64.exe"
#' e <- uci_engine(engine_path)
#' uci_quit(e)
#'
#' # Using pipe '%>%' from magrittr:
#' require(magrittr)
#' uci_engine(engine_path) %>% uci_quit()}
#' @export
#' @importFrom subprocess spawn_process
uci_engine <- function(path){
  engine <- list()
  engine$pipe <- spawn_process(path)
  engine$temp <- character(0)
  return(uci_isready(engine))
}
