#' Reads PGN files into database table
#'
#' Reads PGN files into database table
#'
#' @param con connection argument passed directly to readLines() function. String - the name of the file which the data are to be read from or connection object or URL.
#' @param batch.size number of lines to read in one batch, default is 10^6.
#' @param conn connection argument created by dbConnect
#' @param table.name string (default "pgn"), table name, used in dbWriteTable(conn, table.name, read.pgn(batch))
#' @param ... further arguments passed directly to read.pgn() function (besides ignore.other.games and big.mode)
#'
#' @examples
#'\donttest{
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f,"rbt",encoding = "latin1")
#' require(RSQLite)
#' conn <- dbConnect(SQLite())
#' read.pgn.db(con,stat.moves = FALSE,conn = conn)
#' dbGetQuery(conn, "SELECT COUNT(*) FROM pgn") #2410
#' dbDisconnect(conn)
#' # Works with all types of connections (also gz or zip files).
#' # con argument is passed directly to readLines(con,batch.size)
#' # so (if total number of lines to read is greater then batch.size)
#' # depending on platform use it correctly:
#' # Windows ('rb' opening mode for loop over readLines):
#' con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),"rb",encoding = "latin1")
#' # con <- file("path_to_big_chess_file.pgn","rb",encoding = "latin1")
#' read.pgn.db(con,conn = conn)
#' }
#'\donttest{
#' # Linux/Mac OS X ('r' opening mode for loop over readLines):
#' con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),"r",encoding = "latin1")
#' # con <- file("path_to_big_chess_file.pgn","r",encoding = "latin1")
#' read.pgn.db(con,conn = conn)
#' }
#' \donttest{
#' # Windows (example of zipped file handling)
#' unzf <- unzip("zipped_pgn_file.zip")
#' read.pgn.db(con,conn = conn)
#' }
#' @export
read.pgn.db <- function(con,batch.size = 10^6,conn,table.name = "pgn",...){
  rl <- readLines(con,batch.size)
  lrl <- length(rl)
  n<-0
  first <- T #indicating first batch
  while(lrl>0){
    if(!first) {
      wrp <- read.pgn(rl,big.mode = T,ignore.other.games = F,...)
      RSQLite::dbWriteTable(conn,table.name,wrp,append = T)
    }
    else {
      wrp <- read.pgn(rl,big.mode = T,ignore.other.games = F,...)
      RSQLite::dbWriteTable(conn,table.name,wrp)
      first <- F
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
  message(paste0(Sys.time(),", end of file: ",RSQLite::dbGetQuery(conn, "SELECT COUNT(*) FROM pgn")," games imported successfully"))
}
