#' Sending command debug for chess engine
#'
#' Sending command debug for chess engine. Info about debug command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' switch the debug mode of the engine on and off. In debug mode the engine should sent additional infos to the GUI, e.g. with the "info string" command, to help debugging, e.g. the commands that the engine has received etc. This mode should be switched off by default and this command can be sent any time, also when the engine is thinking.
#' @param engine engine object
#' @param on boolean default TRUE
#' @return engine object
#'
#' @export
uci_debug <- function(engine,on = TRUE){
  if(on) return(uci_cmd(engine,"debug on"))
  else return(uci_cmd(engine,"debug off"))
}
