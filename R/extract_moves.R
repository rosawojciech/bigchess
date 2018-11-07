#' Extract first N moves
#'
#' Extract first N moves from pgn movetext into data frame
#'
#' @param movetext movetext string (or string vector). The standard English values are required: pawn = "P" (often not used), knight = "N", bishop = "B", rook = "R", queen = "Q", and king = "K".
#' @param N integer (default 10) determines how many first N moves will be extracted. Default is 10, should be greater than 0.
#' @param last.move boolean (default TRUE) indicating whether to calculate the last move
#' @return Data frame containing first N moves for white and for black, named as W1, B1, W2 and so on, up to WN and BN (where N is input argument). If N is greater than total moves number then NA's generated. Column complete.movetext flag is indicating if movetext string begin with "1.'move'".
#'
#' @examples
#' extract_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ",N = 3)
#' # e4 e5 Nf3 Nf5 d5 NA TRUE
#' extract_moves("1. e4 e5 2. Nf3 Nf5 3. d5 ",N = 3, last.move = TRUE)
#' # e4 e5 Nf3 Nf5 d5 NA d5 TRUE
#' @export
extract_moves <- function(movetext,N = 10,last.move = T){
  x <- gsub("([0-9+]{0,}\\.)","",movetext,perl = T)
  x <- gsub("1-0","",x,perl = T)
  x <- gsub("0-1","",x,perl = T)
  x <- gsub("1/2-1/2","",x,perl = T)
  x <- gsub("\\*","",x,perl = T)
  x <- gsub("{[^}]+}","",x,perl = T)
  x <- trimws(gsub("  "," ",x))
  sx <- strsplit(x," ")
  r <- as.data.frame(t(sapply(sx,function(i){
    return(  i[1:(N*2)])
  } )))
  colnames(r) <- paste0(rep(c("W","B"),times = N),rep(1:N,each = 2))
  rownames(r) <- NULL
  cmplt <- substr(trimws(gsub("  "," ",movetext)),1,3)
  if(last.move) r$last.move <- vapply(sx,function(i) {if(length(i)>1) return(tail(i,n = 1)) else return("")},character(1))
  r$complete.movetext <- substr(cmplt,1,2)=="1." & substr(cmplt,2,3)!=".."
  r[!r$complete.movetext,-ncol(r)] <- NA
  return(r)
}
