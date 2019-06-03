#' Sending command setoption for chess engine
#'
#' Sending command setoption for chess engine. Info about setoption command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' this is sent to the engine when the user wants to change the internal parameters of the engine. For the "button" type no value is needed. One string will be sent for each parameter and this will only be sent when the engine is waiting. The name of the option in  should not be case sensitive and can inludes spaces like also the value. The substrings "value" and "name" should be avoided in  and  to allow unambiguous parsing, for example do not use  = "draw value".
#' @param engine engine object
#' @param name string option name
#' @param value string option value
#' @return engine object
#'
#' @export
uci_setoption <- function(engine,name = NULL, value = NULL){
  if(!is.null(name) & !is.null(value))
    return(uci_cmd(engine,paste("setoption",name,"value",value)))
}
