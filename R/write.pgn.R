#' Write a PGN file
#'
#' This function writes a data frame of chess game data to a PGN file.
#'
#' @details The function takes a data frame of chess game data in the same
#'   format obtained from `read.pgn()` and writes it to a file. The function
#'   also allows for additional tags to be parsed and for games to be appended
#'   to an existing file.
#'
#'   According to the Seven Tag Roster rule
#'   (http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1), the
#'   STR tag pairs ("Event", "Site", "Date", "Round", "White", "Black" and
#'   "Result") appear before any other tag pairs. The `add.tags` argument allows
#'   you to specify supplemental tag names, such as player-related information,
#'   event-related information, opening information (locale-specific), opening
#'   information (third-party vendors), time and date-related information, time
#'   control, alternative starting positions, game conclusion, and
#'   miscellaneous. The most popular tags include: "WhiteElo", "BlackElo",
#'   "ECO", "SetUp" or "FEN". Note that these tags are case sensitive.
#'
#' @param df A data frame of chess game data in the format used by `read.pgn()`.
#' @param file A string specifying the path to the destination file.
#' @param append (default = `FALSE`) A Boolean indicating if games should be
#'   appended to the current file.
#' @param add.tags A string vector containing additional tags to be parsed.
#' @param source.movetext (default = `TRUE`) A Boolean. If `TRUE` and
#'   SourceMovetext appears in df, then write SourceMovetext as Movetext; else
#'   use parsed Movetext.
#'
#' @return NULL
#'
#' @examples
#' f <- system.file("extdata", "2016_Candidates.pgn", package = "bigchess")
#' df <- read.pgn(f)
#' write.pgn(df, file = "my_file.pgn")
#' df2 <- read.pgn("my_file.pgn")
#' all.equal(df, df2) # TRUE
#' unlink("my_file.pgn") # clean up
#'
#' @export
write.pgn <- function(df, file, add.tags = NULL, append = FALSE,
                      source.movetext = TRUE) {
  # Define the tags
  tags <- c("Event", "Site", "Date", "Round", "White", "Black","Result",
            add.tags)

  # Initialize the file with an empty string
  write("", file = file, append = append)

  # Loop over each row of the data frame
  for (i in seq_len(nrow(df))) {
    tmp <- df[i, ]

    # Write each tag to the file
    for (t in tags) {
      write(
        paste0("[", t, " \"", as.character(tmp[[t]]), "\"]"),
        file = file,
        append = TRUE
      )
    }

    # Check if SourceMovetext is present in the data frame and if
    # source.movetext is TRUE
    if (source.movetext & ("SourceMovetext" %in% colnames(df))) {
      # Write SourceMovetext as Movetext to the file
      write(
        paste(as.character(tmp$SourceMovetext), as.character(tmp$Result), "\n"),
        file = file,
        append = TRUE
      )
    } else {
      # Write parsed Movetext to the file
      write(
        paste(as.character(tmp$Movetext), as.character(tmp$Result), "\n"),
        file = file,
        append = TRUE
      )
    }
  }
}
