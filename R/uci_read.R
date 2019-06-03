#' Read current stdout from chess engine
#'
#' Read current stdout from chess engine
#'
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
#' e <- uci_read(e)
#' e$temp
#' uci_quit(e)}
#' @export
#' @importFrom subprocess process_read
uci_read <- function(engine){
  prs <- process_read(engine$pipe)$stdout
  if(length(prs)>0) engine$temp <- c(engine$temp,prs)
  return(engine)
}
