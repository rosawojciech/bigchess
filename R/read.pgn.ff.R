#' Reads a PGN file into an ff data frame
#'
#' This function reads a PGN file into an ff data frame to more efficiently
#' manage and analyze a large collection of chess games.
#'
#' @details The `con` argument can be a string with the path to a file to read,
#'   a connection object, or a URL. The function reads the PGN files in batches
#'   and writes them to an ff data frame. The size of the batches can be
#'   specified by the user.
#'
#' @param con A string with a path to a file or URL, or a connection object.
#' @param batch.size (default = 10^6) An integer of the number of lines to read
#'   per batch.
#' @param ignore.other.games (default = FALSE) A Boolean indicating if games
#'   where the result is "*", (i.e. ongoing games) should be excluded.
#' @param ... Further arguments passed to `read.pgn()` (excluding
#'   `ignore.other.games` and `big.mode`).
#'
#' @return A message stating the number of games imported.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required package
#' library(ff)
#' library(ffbase)
#'
#' # Specify file and connection
#' f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
#' con <- gzfile(f, "rbt", encoding = "latin1")
#'
#' # Read PGN data into ff data frame
#' fdf <- read.pgn.ff(con, stat.moves = FALSE)
#'
#' # Query the ff data frame
#' nrow(fdf) #2410
#'
#' # Delete the ff data frame
#' delete(fdf)
#'
#' # Example with different types of connections (also gz or zip files)
#' # Connection argument is passed directly to readLines(con, batch.size)
#' # So if total number of lines to read is greater than batch.size,
#' # use the correct opening mode for readLines depending on platform:
#'
#' # Windows ('rb' opening mode for loop over readLines):
#' con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),
#'   "rb", encoding = "latin1")
#'
#' fdf <- read.pgn.ff(con)
#'
#' # Delete the ff data frame
#' delete(fdf)
#' }
read.pgn.ff <- function(con, batch.size = 10^6, ignore.other.games = F, ...) {
  # Read lines from PGN file
  rl <- readLines(con, batch.size)
  lrl <- length(rl)
  n <- 0
  first <- TRUE # indicating first batch

  # Loop over batches
  while (lrl > 0) {
    # Read PGN data
    wrp <- read.pgn(rl, big.mode = TRUE, ignore.other.games = FALSE, ...)
    sel_cols <- setdiff(colnames(wrp),"Movetext")

    for(i in sel_cols) if(is.character(wrp[,i])) wrp[,i] <- factor(wrp[,i])

    # Write data to ff data frame
    if (!first) {
      res <- ffbase::ffdfappend(res, ff::as.ffdf(wrp[,sel_cols]))
    } else {
      res <- ff::as.ffdf(data.frame(wrp[,sel_cols]))
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

  close(con)

  if(ignore.other.games) res <- subset(res,Result !="*")

  res <- droplevels(res)

  message(
    paste0(
      Sys.time(),
      ", end of file: ",
      nrow(res),
      " games imported successfully"
    )
  )

  return(res)
}
