stat_moves_white <- function(moves){
  x <- gsub("([0-9+]{0,}\\.)","",moves,perl = T)
  x <- trimws(gsub("  "," ",x))
  sx <-  strsplit(x," ")
  r <- data.frame(t(sapply(sx,function(i){
    li <- length(i)
    if(li>0) return(as.vector(unlist(table(substr(i[seq(1,li,by = 2)],1,1))[c("B","K","N","O","Q","R")])))
    else return(rep(0, times = 6))
  } )),row.names = NULL)
  r[is.na(r)]<-0
  colnames(r) <- paste0(c("B","K","N","O","Q","R"),"_moves")
  r$R_moves <- r$R_moves + r$O_moves
  r$K_moves <- r$K_moves + r$O_moves
  colnames(r) <- paste0("W_",colnames(r))
  return(r)
}
stat_moves_black <- function(moves){
  x <- gsub("([0-9+]{0,}\\.)","",moves,perl = T)
  x <- trimws(gsub("  "," ",x))
  sx <-  strsplit(x," ")
  r <- data.frame(t(sapply(sx,function(i){
    li <- length(i)
    if(li>1) sli <- seq(2,li,by = 2)
    else sli <- 0
    if(li>1) return(as.vector(unlist(table(substr(i[sli],1,1))[c("B","K","N","O","Q","R")])))
    else return(rep(0, times = 6))
  } )),row.names = NULL)
  r[is.na(r)]<-0
  colnames(r) <- paste0(c("B","K","N","O","Q","R"),"_moves")
  r$R_moves <- r$R_moves + r$O_moves
  r$K_moves <- r$K_moves + r$O_moves
  colnames(r) <- paste0("B_",colnames(r))
  return(r)
}
