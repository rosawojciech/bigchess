utils::globalVariables(c("Result"))
#' Read a PGN file into a data frame
#'
#' This function reads a PGN file into a data frame.
#'
#' @details The `con` argument can be a string with the path to a file to read,
#'   a connection object, or a URL. The function reads the PGN files and writes
#'   them to a data frame. Additional tags besides those in the Seven Tag Roster
#'   can be specified with the `add.tags` parameter. The function can also
#'   compute the number of moves, extract moves, and compute move statistics
#'   based on the provided arguments.
#'
#' @param con (default = NULL) A string with a path to a file or URL, or a
#'   connection object.
#' @param add.tags (default = NULL) A string vector containing additional tags
#'   to be parsed.
#' @param n.moves (default = TRUE) A Boolean indicating whether to count the
#'   moves.
#' @param extract.moves (default = 10) An integer specifying the number of moves
#'   to extract.
#' @param last.move (default = TRUE) A Boolean  indicating whether to include
#'   the last move in the extracted moves.
#' @param stat.moves (default = TRUE) A Boolean indicating whether to compute
#'   move statistics.
#' @param big.mode (default = FALSE) A Boolean indicating whether to use big
#'   mode.
#' @param quiet (default = FALSE) A Boolean indicating whether to suppress
#'   messages.
#' @param ignore.other.games (default = FALSE) A Boolean indicating whether to
#'   ignore ongoing games marked with "*".
#' @param source.movetext (default = FALSE) A boolean value indicating whether
#'   to include original movetext in the output data frame.
#'
#' @return The function returns a data frame that contains parsed information
#'   from the PGN file. The data frame includes:
#' - Seven Tag Roster
#' - Additional tags (if specified)
#' - Movetext
#' - NMoves (if `n.moves` is TRUE)
#' - Extracted moves and a completeness flag (if `extract.moves` is non-zero)
#' - Figure moves count statistics (if `stat.moves` is TRUE)
#' - Original movetext (if `source.movetext` is TRUE)
#'
#' @examples
#' \dontrun{
#' # Load required package
#' library(bigchess)
#'
#' # Specify file and connection
#' f <- system.file("extdata", "2016_Candidates.pgn", package = "bigchess")
#'
#' # Read PGN data into data frame
#' df <- read.pgn(f)
#' }
#' \donttest{
#' # Example downloaded from https://www.pgnmentor.com/files.html#players
#' # and gzipped:
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE)
#'
#' # Fastest mode:
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con, quiet = TRUE, n.moves = FALSE, extract.moves = FALSE,
#'   stat.moves = FALSE, ignore.other.games = FALSE)
#'
#' # Parse additional tags and extract all moves:
#' con <- gzfile(f, encoding = "latin1")
#' df <- read.pgn(con,add.tags = c("WhiteElo", "BlackElo", "ECO"),
#'   extract.moves = -1)
#'
#' # Example of direct downloading data from chess.com using API:
#' df <- read.pgn("https://api.chess.com/pub/player/fabianocaruana/games/2013/03/pgn")
#'
#' # Warning of incomplete line could appear
#' }
#' \dontrun{
#' # Example of scraping all of games given user:
#' user <- "fabianocaruana"
#' library("rjson")
#' json_file <- paste0("https://api.chess.com/pub/player/",user,"/games/archives")
#' json_data <- fromJSON(paste(readLines(json_file), collapse=""))
#' result <- data.frame()
#' for(i in json_data$archives)
#' result <- rbind(result, read.pgn(paste0(i, "/pgn")))
#' }
#' @importFrom utils head tail
#' @export
# Function to read PGN files
read.pgn <- function(con, add.tags = NULL, n.moves = TRUE, extract.moves = 10,
                     last.move = TRUE, stat.moves = TRUE, big.mode = FALSE,
                     quiet = FALSE, ignore.other.games = FALSE, source.movetext = FALSE) {

  # Start time
  st <- Sys.time()

  # Define tags
  tags <- c("Event", "Site", "Date", "Round", "White", "Black", "Result",
            add.tags)

  # If big.mode is TRUE, read the connection directly
  if (big.mode) {
    al <- con
  } else {
    # Otherwise, read lines from the connection and close it if it's a connection
    al <- readLines(con)
    if ("connection" %in% class(con)) close(con)
  }

  # Define regex pattern for tags and movetext
  s <- "^\\[([\\S]+)\\s\"([\\S\\s]+|\\B)\"\\]$"

  # Extract tag names and values
  tmp1 <- gsub(s, "\\1", al, perl = TRUE)
  tmp2 <- gsub(s, "\\2", al, perl = TRUE)

  # Identify lines with tags
  tmp3 <- grepl("^\\[[^%]+\\]$", al, perl = TRUE)

  # Cumulative sum of lines starting a new game
  tmp4 <- cumsum(grepl("\\[Event ", al))

  # Assign 'Movetext' to non-tag lines
  tmp1[!tmp3] <- "Movetext"

  # Create a data frame with game ID, tag names and values
  r2 <- data.frame(tmp1, tmp2, tmp3, tmp4, stringsAsFactors = FALSE)

  # Concatenate movetext lines into one string per game
  gt <- paste(
    subset(r2, tmp1 == "Movetext", select = c(tmp2))[,1], collapse = " "
  )

  # If source.movetext is TRUE, keep original movetext
  if (source.movetext) gt2 <- gt

  # Remove comments and variants from movetext
  gt <- gsub("{[^}]+}", "", gt, perl = TRUE) # remove comments
  gt <- gsub("\\((?>[^()]|(?R))*\\)", "", gt, perl = TRUE) # remove variants (Recursive Annotation Variation)

  # Remove special characters from movetext
  gt <- gsub("[\\?\\!]", "", gt, perl = TRUE) # remove ?! chars

  # Remove move numbers from movetext
  gt <- gsub("[0-9]+\\.\\.\\.", "", gt, perl = TRUE)

  # Remove NAG and SAN suffix from movetext
  gt <- gsub("\\$[0-9]+", "", gt, perl = TRUE)

  # Split movetext at game end markers
  for (i in c("1-0", "1\\/2-1\\/2", "0-1", "\\*")) {
    gt <- unlist(strsplit(gt, split = i))
    if (source.movetext) gt2 <- unlist(strsplit(gt2, split = i))
  }

  # Create a data frame with game ID and event name for each game
  r <- subset(r2, tmp1 == "Event", select = c(tmp4,tmp2))

  # Rename columns
  colnames(r) <- c("GID", "Event")

  # Merge tag values into the data frame for each game
  for (i in setdiff(tags, "Event")) {
    tmp <- subset(r2, tmp1 == i, select = c(tmp4, tmp2))
    colnames(tmp) <- c("GID", i)
    r <- merge(r, tmp, all.x = TRUE)
  }

  # Add movetext to the data frame
  r$Movetext <- trimws(gsub("[[:space:]]+", " ", head(gt, nrow(r))))

  # If source.movetext is TRUE, add original movetext to the data frame
  if (source.movetext) r$SourceMovetext <- head(gt2, nrow(r))

  # Check if scanning ended properly
  tal <- tail(al, 1)

  # If not, mark last movetext as empty and remove this record from data frame
  if (big.mode) {
    if (!grepl("\\[", tal) & !(tal == "")) {
      r[nrow(r), "Movetext"] <- ""
    }
    if (r[nrow(r), "Movetext"] == "") r <- r[-nrow(r), ]
  }

  # Compute number of moves if n.moves or extract.moves is TRUE
  if (n.moves || extract.moves) r$NMoves <- n_moves(r$Movetext)

  # Extract moves if extract.moves is TRUE
  if (extract.moves) {
    if (extract.moves == -1) {
      N <- max(r$NMoves)
    } else {
      N <- extract.moves
    }
    r <- cbind(r, extract_moves(r$Movetext, N, last.move = last.move))
  }

  # Compute move statistics if stat.moves is TRUE
  if (stat.moves) {
    r <- cbind(r, stat_moves(r$Movetext))
  }

  # Remove games with Result '*' if ignore.other.games is TRUE
  if (ignore.other.games) {
    nr <- nrow(r)
    r <- subset(r, Result != "*")
    r$Result <- factor(r$Result,
                       levels = c("1-0", "1/2-1/2", "0-1"),
                       labels = c("1-0", "1/2-1/2", "0-1"),
                       ordered = TRUE)
  } else {
    r$Result <- factor(r$Result,
                       levels = c("1-0", "1/2-1/2", "0-1", "*"),
                       labels = c("1-0", "1/2-1/2", "0-1", "*"),
                       ordered = TRUE)
  }

  # Drop unused levels and convert some columns to integer
  r <- droplevels(r)

  for (i in intersect(colnames(r), c("WhiteElo", "BlackElo", "SetUp"))) {
    r[, i] <- as.integer(r[, i])
  }

  return(r[, -1])
}

