utils::globalVariables(c("Result"))
#' Compute tree for a given move
#'
#' Compute tree for a given move (frequencies and winning percent)
#'
#' @param df data frame containg move and Result column from pgn function or data frame containing aggregated data from such df (containg columns: Result, W1, B1, W2, ..., WN, BN, Freq)
#' @param move character indicating which move should be browsed, example "W1"
#' @return Data frame containg White_score (White winning percent), Draws_percent, Black_score and N (number of games). Sorted by power of move (White_score times N which describes popularity and score of move) descending.
#' @importFrom stats aggregate as.formula
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,stat.moves = FALSE)
#' # Analyze best answers to 1. e4 in Kasparov games (both white and black)
#' tree_move(subset(df,W1=="e4"),move = "B1")
#' # Analyze openings in aggregated data
#' tree_move(FirstTwoMoves,"W1")
#' @export
tree_move <- function(df,move){
  df[,move] <- droplevels(df[,move])
  if(!("Freq" %in% colnames(df))) df$Freq <- 1
  t <- aggregate(as.formula(paste("Freq ~ Result +",move)),df,FUN = sum)
  r <-subset(t,Result == "1-0")
  for(i in c("1/2-1/2","0-1"))
    r <- merge(r,subset(t,Result == i), by = move,all = T)
  r <- r[,c(1,3,5,7)]
  r[is.na(r)]<-0
  r$N <- rowSums(r[,-1])
  r[,2:4]<-round(r[,2:4]/r$N*100,1)
  colnames(r) <- c(move,"White_score","Draws_percent","Black_score","N")

  r <- r[order(r$White_score*r$N,decreasing = T),]
  return(r)
}
