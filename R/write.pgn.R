#' Write PGN data.frames into file
#'
#' Write PGN data.frames into file
#'
#' @param df data.frame from read.pgn()
#' @param file string path to destination file
#' @param append boolean (default FALSE), should games be appended to current file?
#' @param add.tags string vector containing additional tags to be parsed.
#' According to Seven Tag Roster rule:
#' http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1
#' The STR tag pairs appear before any other tag pairs: "Event", "Site", "Date", "Round", "White", "Black" and "Result".
#' Using this argument you can specify supplemental tag names, such as: Player related information, Event related information, Opening information (locale specific), Opening information (third party vendors), Time and date related information, Time control, Alternative starting positions, Game conclusion and Miscellaneous.
#' Most popular: "WhiteElo", "BlackElo","ECO","SetUp" or "FEN". Case sensitive.
#'
#' @examples
#' f <- system.file("extdata", "2016_Candidates.pgn", package = "bigchess")
#' df <- read.pgn(f)
#' write.pgn(df, file = "my_file.pgn")
#' df2 <- read.pgn("games.pgn")
#' all.equal(df,df2) # TRUE
#' unlink("games.pgn") # clean up
#' @export
write.pgn <- function(df, file, add.tags = NULL, append = FALSE){
  tags <- c(c("Event","Site","Date","Round","White","Black","Result"),add.tags)
  write("",file = file, append = append)
for(i in 1:nrow(df)){
  tmp <- df[i,]
  for(t in tags){
    write(paste0('[',t,' "',as.character(tmp[[t]]),'"]'),file = file,append = TRUE)
  }
  write(paste(as.character(tmp$Movetext),as.character(tmp$Result),"\n"),file = file,append = TRUE)
}
}
