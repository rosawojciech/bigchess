utils::globalVariables(c("White", "Black"))
#' Compute player profile
#'
#' Computes players profile from data frame obtained from read.pgn() function into data frame
#' @param df data frame from read.pgn or read.pgn.ff files with stats computed.
#' @param player string used in grepl(player,White) and grepl(player,Black)
#' @return Data frame with player (column prefix P_) and opponent (column prefix O_) figure move counts. Column Player_Col indicating pieces colour for player (factor White or Black).
#' Example column P_Q_moves means number of player Queen moves count.
#' @examples
#' f <- system.file("extdata", "Kasparov.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,ignore.other.games = TRUE)
#' nrow(df) # 2109
#' df_pp <- player_profile(df,"Kasparov, Gary")
#' nrow(df_pp) # 1563
#' df_pp <- player_profile(df,"Kasparov,G")
#' nrow(df_pp) # 543
#' df_pp <- player_profile(df,"Kasparov, G\\.")
#' nrow(df_pp) # 2
#' df_pp <- player_profile(df,"Kasparov")
#' nrow(df_pp) # 2109 - correct
#' boxplot(P_Q_moves/NMoves~Player_Col,df_pp,
#' main = "Average Queen Moves\n Kasparov as Black (909 games) vs Kasparov as White (1200 games)",
#' col = c("black","white"),border = c("black","black"),notch = TRUE)
#' # Magnus Carlsen data example
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,ignore.other.games = TRUE)
#' nrow(df) # 2410
#' df_pp <- player_profile(df,"Carlsen")
#' nrow(df_pp) # 2411 - ??
#' # One game was played by Carlsen,H
#' df_pp <- player_profile(df,"Carlsen,M")
#' nrow(df_pp) # 2410 - correct
#' @export
player_profile <- function(df,player){
  dw <- subset(df,grepl(player,White))
  if(nrow(dw)>0){
  fg <- c("B","K","N","O","Q","R")
  rdw <- dw[,c(paste0("W_",fg,"_moves"),paste0("B_",fg,"_moves"))]
  colnames(rdw) <- c(paste0("P_",fg,"_moves"),paste0("O_",fg,"_moves"))
  rdw$Player_Col <- "White"
  rrdw <- cbind(dw,rdw)
  }
  else { rrdw <- data.frame()}
  db <- subset(df,grepl(player,Black))
  if(nrow(db)>0){
  rdb <- db[,c(paste0("B_",fg,"_moves"),paste0("W_",fg,"_moves"))]
  colnames(rdb) <- c(paste0("P_",fg,"_moves"),paste0("O_",fg,"_moves"))
  rdb$Player_Col <- "Black"
  rrdb <- cbind(db,rdb)
  }
  else {rrdb <- data.frame()}
  if(nrow(db) + nrow(dw)>0)
    return(rbind(rrdw,rrdb))
  else {
    message("No data found for this player name")
    return(data.frame())
              }
}
