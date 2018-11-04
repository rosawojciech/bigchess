utils::globalVariables(c("Result"))
#' Compute ECO tree
#'
#' Compute ECO tree (frequencies and winning percent)
#'
#' @param df data frame containg ECO and Result columns
#' @param topn integer, default 0, indicating how many top openings should be included, 0 means show all openings
#' @return Data frame containg White_score (White winning percent), Draws_percent, Black_score and N (number of games). Sorted by power of ECO (White_score * N which describes popularity and score of move) descending.
#' @importFrom stats aggregate as.formula
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,stat.moves = FALSE, add.tags = "ECO")
#' @export
tree_eco <- function(df,topn = 0){
  #df[,"ECO"] <- droplevels(df[,"ECO"])
  if(!("Freq" %in% colnames(df))) df$Freq <- 1
  t <- aggregate(as.formula(paste("Freq ~ Result + ECO")),df,FUN = sum)
  r <-subset(t,Result == "1-0")
  for(i in c("1/2-1/2","0-1"))
    r <- merge(r,subset(t,Result == i), by = "ECO",all = T)
  r <- r[,c(1,3,5,7)]
  r[is.na(r)]<-0
  r$N <- rowSums(r[,-1])
  r[,2:4]<-round(r[,2:4]/r$N*100,1)
  colnames(r) <- c("ECO","White_score","Draws_percent","Black_score","N")

  r <- r[order(r$White_score*r$N,decreasing = T),]
  if(topn>0) r <- r[1:topn,]
  return(r)
}
