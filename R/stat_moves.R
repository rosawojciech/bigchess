#' Extract statistics of moves
#'
#' Extract statistics of moves (counts figure moves) from movetext vector into data frame
#'
#' @param movetext movetext string (or string vector). The standard English values are required: pawn = "P" (often not used), knight = "N", bishop = "B", rook = "R", queen = "Q", and king = "K".
#' @param sides "both" (default),"white" or "black"
#' @return Data frame containing moves count statistics for white and for black and total.
#'
#' @examples
#' stat_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ")
#' @export
stat_moves <- function(movetext,sides = "both"){
w <- data.frame()
  if(sides %in% c("both","white"))
  w <- stat_moves_white(movetext)
    else w<-stat_moves_white(movetext)
  if(sides %in% c("both","black"))
    if(sides == "black") w <- stat_moves_black(movetext)
    else w <- cbind(w,stat_moves_black(movetext))
  if(sides %in% c("both"))
    for(i in c("B","K","N","O","Q","R"))
      w[,paste0(i,"_moves")] <- w[,paste0("W_",i,"_moves")]+w[,paste0("B_",i,"_moves")]
  return(w)
}
