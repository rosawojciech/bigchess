#' Reads PGN files into ff data frame
#'
#' Reads PGN files into ff data frame
#'
#' @param con connection argument passed directly to readLines() function. String - the name of the file which the data are to be read from or connection object or URL.
#' @param batch.size number of lines to read in one batch, default is 10^6.
#' @param ignore.other.games boolean (default FALSE) if TRUE result is subset of original dataset without games with result marked as "*", i.e. ongoing games. The only one argument which is not passed directly to read.pgn function.
#' @param ... further arguments passed directly to read.pgn() function (besides ignore.other.games and big.mode)
#' @return ff data frame like from read.pgn() function. Since character values are not supported in ffdf object, "Movetext" column is ommited.
#'
#' @examples
#' require(ff)
#' require(ffbase)
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f,"rbt",encoding = "latin1")
#' # options("fftempdir"="/path/"...) # if necessarily
#' fdf <- read.pgn.ff(con,stat.moves = FALSE)
#' delete(fdf)
#' # Works with all types of connections (also gz or zip files).
#' # con argument is passed directly to readLines(con,batch.size)
#' # so (if total number of lines to read is greater then batch.size)
#' # depending on platform use it correctly:
#'\donttest{
#' # Windows ('rb' opening mode for loop over readLines):
#' con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),"rb",encoding = "latin1")
#' # con <- file("path_to_big_chess_file.pgn","rb",encoding = "latin1")
#' fdf <- read.pgn.ff(con)
#' delete(fdf)}
#'\donttest{
#' # Linux ('r' opening mode for loop over readLines):
#' con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),"r",encoding = "latin1")
#' # con <- file("path_to_big_chess_file.pgn","r",encoding = "latin1")
#' fdf <- read.pgn.ff(con)
#' delete(fdf)}
#' @import ff
#' @importFrom ffbase ffdfappend
#' @export
read.pgn.ff <- function(con,batch.size = 10^6,ignore.other.games = F,...){
  rl <- readLines(con,batch.size)
  lrl <- length(rl)
  n<-0
  while(lrl>0){
    if(exists("res")) {
      wrp <- read.pgn(rl,big.mode = T,ignore.other.games = F,...)[sel_cols]
      for(i in sel_cols) if(is.character(wrp[,i]))
        wrp[,i] <- factor(wrp[,i])
      res <- ffdfappend(res,as.ffdf(wrp[,sel_cols]))
    }
    else {
      wrp <- read.pgn(rl,big.mode = T,ignore.other.games = F,...)
      sel_cols <- setdiff(colnames(wrp),"Movetext")
      for(i in sel_cols) if(is.character(wrp[,i])) wrp[,i] <- factor(wrp[,i])

        res <- ff::as.ffdf(data.frame(wrp[,sel_cols]))
    }
    x <- grepl("^\\[Event ",rl,perl = T)
    mwx <- max(which(x))

    if(nrow(wrp)<sum(x)){
      rl <- c(rl[mwx:length(rl)],readLines(con,batch.size))
    }
    else{
      rl <- readLines(con,batch.size)
    }
    lrl <-length(rl)
    n <- n+1
    message(paste0(Sys.time(),", batch ",n," processed"))
  }

  close(con)
  message(paste0(Sys.time(),", end of file: ",nrow(res)," games imported successfully"))
  if(ignore.other.games) res <- subset(res,Result !="*")
  res <- droplevels(res)
  return(res)
}
