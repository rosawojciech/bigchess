utils::globalVariables(c("Result"))
#' Reads PGN files into data frame
#'
#' Reads PGN files into data frame
#'
#' @param con connection argument passed directly to readLines() function. String - the name of the file which the data are to be read from or connection object or URL.
#' @param add.tags string vector containing additional tags to be parsed.
#' According to Seven Tag Roster rule:
#' http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1
#' The STR tag pairs appear before any other tag pairs: "Event", "Site", "Date", "Round", "White", "Black" and "Result".
#' Using this argument you can specify supplemental tag names, such as: Player related information, Event related information, Opening information (locale specific), Opening information (third party vendors), Time and date related information, Time control, Alternative starting positions, Game conclusion and Miscellaneous.
#' Most popular: "WhiteElo", "BlackElo","ECO","SetUp" or "FEN". Case sensitive.
#' @param n.moves boolean (default TRUE), compute number of moves?
#' @param extract.moves integer (default 10) passed to extract_moves function. Additionaly value -1 will extract all moves from movetext (not recommended for big files). Value 0 means that moves will not be extracted.
#' @param last.move boolean (default TRUE) passed to extract_moves, ignored when extract.moves = 0
#' @param stat.moves boolean (default TRUE), compute moves count statistics? Could take a long time for big file.
#' @param big.mode boolean (default FALSE) used in read.pgn.ff function
#' @param quiet boolean (default FALSE), indicating if messages should appear.
#' @param ignore.other.games boolean (default FALSE) if TRUE result is subset of original dataset without games with result marked as "*", i.e. ongoing games
#' @param source.movetext boolean (default FALSE, experimental!) if TRUE column with original movetext will be added
#' @return Data frame containg STR, additional tags (conditionally), Movetext, NMoves (conditionally), extracted moves (conditionally) with complete.movetext flag, figure moves count statistics (conditionally).
#'
#' @examples
#' f <- system.file("extdata", "2016_Candidates.pgn", package = "bigchess")
#' df <- read.pgn(f)
#' # ...successfully imported 56 games...
#'\donttest{
#' # Example downloaded from https://www.pgnmentor.com/files.html#players and gzipped:
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE)
#' # Fastest mode:
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,quiet = TRUE,n.moves = FALSE,extract.moves = FALSE,
#' stat.moves = FALSE, ignore.other.games = FALSE)
#' # Parse additional tags and extract all moves:
#' con <- gzfile(f,encoding = "latin1")
#' df <- read.pgn(con,add.tags = c("WhiteElo", "BlackElo", "ECO"),extract.moves = -1)
#' # Example of direct downloading data from chess.com using API:
#' df <- read.pgn("https://api.chess.com/pub/player/fabianocaruana/games/2013/03/pgn")
#' # Warning of incomplete line could appear}
#'\donttest{
#' # Example of scraping all of games given user:
#'user <- "fabianocaruana"
#'library("rjson")
#'json_file <- paste0("https://api.chess.com/pub/player/",user,"/games/archives")
#'json_data <- fromJSON(paste(readLines(json_file), collapse=""))
#'result <- data.frame()
#'for(i in json_data$archives)
#'result <- rbind(result,read.pgn(paste0(i,"/pgn")))}
#' @importFrom utils head tail
#' @export
read.pgn <- function(con,add.tags = NULL,n.moves = T, extract.moves = 10,last.move = T,stat.moves = T,big.mode = F,quiet = F,ignore.other.games = F,source.movetext = F){
  st <- Sys.time()
  tags <- c(c("Event","Site","Date","Round","White","Black","Result"),add.tags)
  if(big.mode) al = con
  else{
    al = readLines(con)
    if("connection" %in% class(con)) close(con)
    }
  s <- "^\\[([\\S]+)\\s\"([\\S\\s]+|\\B)\"\\]$"
  tmp1 <- gsub(s,"\\1",al,perl = T)
  tmp2 <- gsub(s,"\\2",al, perl = T)
  tmp3 <- grepl("^\\[[^%]+\\]$",al,perl = T)
  tmp4 <- cumsum(grepl("\\[Event ",al))
  tmp1[!tmp3] <- "Movetext"
  r2 <- data.frame(tmp1,tmp2,tmp3,tmp4,stringsAsFactors = F)
  gt <- paste(subset(r2,tmp1=="Movetext",select = c(tmp2))[,1],collapse = " ")
  if(source.movetext) gt2 <- gt
  gt <- gsub("{[^}]+}","",gt,perl = T) #remove comments
  gt <- gsub("\\((?>[^()]|(?R))*\\)","",gt,perl = T) #remove variants (Recursive Annotation Variation)
  gt <- gsub("[\\?\\!]","",gt,perl = T) # remove ?! chars
  gt <- gsub("[0-9]+\\.\\.\\.","",gt,perl = T)
  gt <- gsub("\\$[0-9]+","",gt,perl = T) # remove NAG and SAN suffix
  for(i in c("1-0","1\\/2-1\\/2","0-1","\\*"))
    gt <- unlist(strsplit(gt,split = i))
  if(source.movetext) for(i in c("1-0","1\\/2-1\\/2","0-1","\\*")) gt2 <- unlist(strsplit(gt2,split = i))
  r <- subset(r2,tmp1 == "Event",select = c(tmp4,tmp2))
  colnames(r) <- c("GID","Event")
  #tags <- c("Event","Site","Date","Round","White","Black","Result")
  for(i in c(setdiff(tags,"Event"))){
    tmp <- subset(r2,tmp1 == i,select = c(tmp4,tmp2))
    colnames(tmp) <- c("GID",i)
    r <- merge(r,tmp,all.x = T)
  }
  r$Movetext <- trimws(gsub("[[:space:]]+"," ",head(gt,nrow(r))))
  if(source.movetext) r$SourceMovetext  <- head(gt2,nrow(r))
  tal <- tail(al,1) # check if scanning ended properly
  # otherwise mark last movetext as empty
  if(big.mode) if(!grepl("\\[",tal)&!(tal=="")) {
    r[nrow(r),"Movetext"] <- ""

  }
  # and remove this record from data frame
  if(big.mode) if(r[nrow(r),"Movetext"]=="")   r <- r[-nrow(r),]
  if(!quiet) message(paste0(Sys.time(),", successfully imported ",nrow(r)," games"))
  if(n.moves||extract.moves)
    r$NMoves <- n_moves(r$Movetext)
  if(!quiet) message(paste0(Sys.time(),", N moves computed"))
  if(extract.moves){
    if(extract.moves==-1) {N <- max(r$NMoves)}
    else {N <- extract.moves}
    r <- cbind(r,extract_moves(r$Movetext,N,last.move = last.move))
    if(!quiet) message(paste0(Sys.time(),", extract moves done")) }
  if(stat.moves) {
    r <- cbind(r,stat_moves(r$Movetext))
    if(!quiet) message(paste0(Sys.time(),", stat moves computed"))
    }
  if(ignore.other.games)
  {
    nr <- nrow(r)
    r <- subset(r,Result!="*")
    r$Result <- factor(r$Result,levels = c("1-0","1/2-1/2","0-1"),labels=c("1-0","1/2-1/2","0-1"),ordered = T)
    if(!quiet) message(paste0(Sys.time(),", subset done (",nr-nrow(r)," games with Result '*' removed) "))

  }
  else{
    r$Result <- factor(r$Result,levels = c("1-0","1/2-1/2","0-1","*"),labels=c("1-0","1/2-1/2","0-1","*"),ordered = T)
  }
  r <- droplevels(r)
  for(i in intersect(colnames(r),c("WhiteElo","BlackElo","SetUp")))
    r[,i] <- as.integer(r[,i])

  return(r[,-1])
}
