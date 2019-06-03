#' Sending command register for chess engine
#'
#' Sending command register for chess engine. Info about register command from http://wbec-ridderkerk.nl/html/UCIProtocol.html
#' this is the command to try to register an engine or to tell the engine that registration will be done later. This command should always be sent if the engine	has send "registration error" at program startup.
#' @param engine engine object
#' @param later boolean default TRUE
#' @param name string
#' @param code string
#' @return engine object
#'
#' @export
uci_register <- function(engine,later = TRUE, name = NULL, code = NULL){
  if(later) uci_cmd(engine,"register later")
  if(!is.null(name)) uci_cmd(engine,paste("register name",name))
  if(!is.null(name)) uci_cmd(engine,paste("register code",code))
  return(engine)
}
