#' Sending command ponderhit for chess engine
#'
#' Sending command ponderhit for chess engine. Info about ponderhit command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' the user has played the expected move. This will be sent if the engine was told to ponder on the same move the user has played. The engine should continue searching but switch from pondering to normal search.
#' @param engine engine object
#' @return engine object
#'
#' @export
uci_ponderhit <- function(engine){
  return(uci_cmd(engine,"ponderhit"))
}
