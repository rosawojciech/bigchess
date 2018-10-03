#' Plot tree for a given tree move table
#'
#' Plot tree (barplot percentages) for a given tree move data frame.
#'
#' @param tr data frame containg tree move
#' @param main string for main title, default is ""
#' @param add.lines boolean (default TRUE) add weighted mean lines?
#' @param add.labels boolean (default TRUE) add labels?
#' @return Barplot with white scores, draws percent and black scores.
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,stat.moves = FALSE)
#' tr <- tree_move(subset(df,W1=="e4"),"B1")
#' plot_tree_move(tr,"1. e4 ... ?")
#' # Plot tree move openings in aggregated data
#' tr <- tree_move(FirstTwoMoves,"W1")
#' plot_tree_move(tr,paste0("1. ... ?\n",sum(FirstTwoMoves$Freq)," total games"))
#' @importFrom graphics barplot lines text
#' @export
plot_tree_move <- function(tr,main = "",add.lines = T,add.labels = T){
  rtr <- tr[rev(1:nrow(tr)),]
  ptr <- barplot(as.table(t(rtr[,c("White_score","Draws_percent","Black_score")])),horiz = T,names.arg = paste0("(",rtr[,"N"],") ",rtr[,1]),las = 1,col = c("White","grey","black"),cex.names=.8,main = main)
  if(add.lines){
  lines(rep(sum(tr$White_score*tr$N)/sum(tr$N),nrow(tr)),ptr,col = "black")
  lines(rep(100-sum(tr$Black_score*tr$N)/sum(tr$N),nrow(tr)),ptr,col = "white")
  }
  if(add.labels){
    prws <- paste0(rev(format(tr$White_score,nsmall = 1)),"%")
    prws[rev(tr$White_score)==0] <- " "
  text(x = 5, y = ptr,labels = prws)
  prbs <- paste0(rev(format(tr$Black_score,nsmall = 1)),"%")
  prbs[rev(tr$Black_score)==0] <- " "
  text(x = 95, y = ptr,labels = prbs,col = "White")
  prdp <- paste0(rev(format(tr$Draws_percent,nsmall = 1)),"%")
  prdp[rev(tr$Draws_percent)==0] <- " "
  text(x = rev(tr$White_score+tr$Draws_percent/2), y = ptr,labels = prdp,col = "Black")
  }
}
