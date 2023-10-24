#' Convert chess moves from SAN to LAN
#'
#' This function converts a string of chess moves in standard algebraic notation
#' (SAN) into UCI long algebraic notation (LAN), which is used by chess engines.
#'
#' @details The `san2lan` function takes a string of chess moves in SAN and
#'   converts it to LAN. The string should not include comments, variants, etc.
#'
#' @note This function assumes that the moves are valid and does not check for
#'   errors or illegal moves.
#'
#' @param movetext.san A string of chess moves in SAN.
#'
#' @return A string of chess moves in LAN.
#'
#' @examples
#' san2lan("1. e4 e5 2. Nf3 Nf5 3. d5 ")
#'
#' @export
san2lan <- function(movetext.san) {
  # Initialize variables
  tmpr <- strsplit(cleanup_san(movetext.san), " ")[[1]]
  curlan <- ""
  position <- position.start()
  p <- 1  # Current player move (1 = white)

  for (ft in 1:length(tmpr)) {
    # Parse SAN move
    smm <- san.move2move(position, san.move = tmpr[ft], p)
    r1 <- smm[1]
    c1 <- smm[2]
    r2 <- smm[3]
    c2 <- smm[4]
    pr <- smm[5]
    curlan <- paste0(curlan, move2lan(r1, c1, r2, c2, pr), " ")

    # Update position
    position <- position.move(position, r1, c1, r2, c2, pr)
    # Change side
    p <- -p
  }

  return(trimws(curlan))
}
