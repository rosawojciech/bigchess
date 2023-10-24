#' Read a PGN file into a database table
#'
#' This function reads a PGN file into a database table to more efficiently
#' manage and analyze a large collection of chess games.
#'
#' @details The `con` argument can be a string with the path to a file to read,
#'   a connection object, or a URL. The function reads the PGN files in batches
#'   and writes them to a database table. The size of the batches and the name
#'   of the database table can be specified by the user.
#'
#' @param con A string with a path to a file or URL, or a connection object.
#' @param batch.size (default = 10^6) An integer of the number of lines to read
#'   per batch.
#' @param conn A connection created by `dbConnect`.
#' @param table.name (default = "pgn") A string with the database table's name.
#' @param ... Further arguments passed to `read.pgn()` (excluding
#'   `ignore.other.games` and `big.mode`).
#'
#' @return A message stating the number of games imported.
#'
#' @examples
#' \dontrun{
#' # Load required package
#' library(RSQLite)
#'
#' # Specify file and connection
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f, "rbt", encoding = "latin1")
#'
#' # Connect to SQLite database
#' conn <- dbConnect(SQLite())
#'
#' # Read PGN data into database
#' read.pgn.db(con, stat.moves = FALSE, conn = conn)
#'
#' # Query the database
#' dbGetQuery(conn, "SELECT COUNT(*) FROM pgn") #2410
#'
#' # Disconnect from the database
#' dbDisconnect(conn)
#'
#' # Example with different types of connections (also gz or zip files)
#' # Connection argument is passed directly to readLines(con, batch.size)
#' # So if total number of lines to read is greater than batch.size,
#' # use the correct opening mode for readLines depending on platform:
#'
#' # Windows ('rb' opening mode for loop over readLines):
#' con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),
#'   "rb", encoding = "latin1")
#' # Alternatively: con <- file("path_to_big_chess_file.pgn",
#'   "rb", encoding = "latin1")
#'
#' # Connect to SQLite database again
#' conn <- dbConnect(SQLite())
#'
#' read.pgn.db(con, conn = conn)
#'
#' # Disconnect from the database
#' dbDisconnect(conn)
#' }
#' \dontrun{
#' # Linux/Mac OS X ('r' opening mode for loop over readLines):
#' con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),
#'   "r", encoding = "latin1")
#' # Alternatively: con <- file("path_to_big_chess_file.pgn",
#'   "r", encoding = "latin1")
#'
#' # Connect to SQLite database again
#' conn <- dbConnect(SQLite())
#'
#' read.pgn.db(con, conn = conn)
#'
#' # Disconnect from the database
#' dbDisconnect(conn)
#' }
#' \dontrun{
#' # Windows (example of zipped file handling)
#' unzf <- unzip("zipped_pgn_file.zip")
#'
#' # Connect to SQLite database again
#' conn <- dbConnect(SQLite())
#'
#' read.pgn.db(unzf, conn = conn)
#'
#' # Disconnect from the database
#' dbDisconnect(conn)
#' }
#'
#' @export
read.pgn.db <- function(con, batch.size = 10^6, conn, table.name = "pgn", ...) {
  # Read lines from PGN file
  rl <- readLines(con, batch.size)
  lrl <- length(rl)
  n <- 0
  first <- TRUE # indicating first batch

  # Loop over batches
  while (lrl > 0) {
    # Read PGN data
    wrp <- read.pgn(rl, big.mode = TRUE, ignore.other.games = FALSE, ...)

    # Write data to database table
    if (!first) {
      RSQLite::dbWriteTable(conn, table.name, wrp, append = TRUE)
    } else {
      RSQLite::dbWriteTable(conn, table.name, wrp)
      first <- FALSE
    }

    # Check for remaining lines in current batch
    x <- grepl("^\\[Event ", rl, perl = TRUE)
    mwx <- max(which(x))

    if (nrow(wrp) < sum(x)) {
      rl <- c(rl[mwx:length(rl)], readLines(con, batch.size))
    } else {
      rl <- readLines(con, batch.size)
    }

    lrl <- length(rl)
    n <- n + 1

    message(paste0(Sys.time(), ", batch ", n, " processed"))
  }

  # Close connection and print message
  close(con)
  message(
    paste0(
      Sys.time(),
      ", end of file: ",
      RSQLite::dbGetQuery(conn, "SELECT COUNT(*) FROM pgn"),
      " games imported successfully"
    )
  )
}
