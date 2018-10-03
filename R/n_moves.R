#' Compute number of moves
#'
#' Compute total number of moves given movetext string (or string vector)
#'
#' @param movetext movetext string (or string vector)
#' @return n integer (or integer vector)
#'
#' @examples
#' n_moves(c("1. e4 e5 2. Nf3 Nf5 3. d5 ","1. d4 d5"))
#' # 3 1
#' @export
n_moves <- function(movetext){
  s <- "([0-9+]{0,}\\.)"
  w <- regmatches(movetext,gregexpr(s,movetext))
  return(unlist(lapply(w, function(i) {max(as.integer(gsub("\\.","",tail(i,1))),0)})))
}
