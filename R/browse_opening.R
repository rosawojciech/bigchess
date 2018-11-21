#' Browse opening
#'
#' Browse opening winning and drawing percentages by table and barplot
#'
#' @param df data frame with imported chess games from read.pgn() function.
#' @param movetext movetext string, default is "" means browse first move for White. The standard English values are required: pawn = "P" (often not used), knight = "N", bishop = "B", rook = "R", queen = "Q", and king = "K".
#' @return Data frame from tree_move function and plot from plot_tree_move function.
#'
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,ignore.other.games = TRUE,stat.moves = FALSE)
#' # Analyze best Kasparov openings:
#' bo <- browse_opening(subset(df,grepl("Kasparov",White)))
#' # Analyze 'best' answer to Kasparov Ruy Lopez:
#' bo <- browse_opening(subset(df,grepl("Kasparov",White)),"1.e4 e5 2.Nf3 Nc6 3.Bb5")
#' # Analyze best answer to "1.e4 e5 2.Nf3" in aggregated data
#' browse_opening(FirstTwoMoves,"1.e4 e5 2.Nf3")
#' @export
browse_opening <- function(df,movetext = ""){
  opening <- movetext
  if(opening == ""){
    r <- df
    move = "W1"
  }
  else{
  no <- n_moves(opening)
  emo <- data.frame(extract_moves(opening,no,F),stringsAsFactors = T)
  ncemo <- ncol(emo)-1
  if(is.na(emo[,ncemo]))
  {
    r <- merge(df,emo[,-ncemo])
    move = colnames(emo)[ncemo]
  }else
  {
    r <- merge(df,emo)
    move = paste0("W",ncemo/2+1)
  }
  }
  if(nrow(r)>0){
    tr <- tree_move(r,move)
    plot_tree_move(tr,main = paste(opening,"\n N = ",sum(tr$N)))
    return(tr)
  }
  else{
    message("No openings found in data frame")
    return(data.frame())
  }
}
