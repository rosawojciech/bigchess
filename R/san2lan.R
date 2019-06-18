#' Movetext conversion from SAN to LAN
#'
#' Convert SAN movetext (FIDE) to LAN movetext (used by chess engines)
#'
#' @param movetext.san movetext string in standard algebraic notation (SAN) required by FIDE, but without comments, variants etc.
#' @return movetext in long algebraic notation
#'
#' @examples
#' san2lan("1. e4 e5 2. Nf3 Nf5 3. d5 ")
#' @export
san2lan <- function(movetext.san){
  {
    rn <- 1:8
    names(rn) <- 8:1
    cn <- 1:8
    names(cn) <- letters[1:8]
    figs <- 1:6
    names(figs) <- c("","b","n","r","q","k")
    diag1 <- row(ps)-col(ps) # possible moves for bishop
    diag2 <- -diag1[,8:1]
    kni <- matrix(data = c(-1,-1,-2,-2,1,1,2,2,2,-2,1,-1,2,-2,1,-1),nrow = 8,ncol = 2) # possible moves for knight
    kin <- matrix(data = c(-1,-1,-1,0,0,1,1,1,-1,0,1,-1,1,-1,0,1),nrow = 8,ncol = 2) # possible moves for king

  }#INIT
  tmpr <- strsplit(cleanup_san(movetext.san)," ")[[1]]
  curlan <- ""
  position <- position.start()
  p <- 1 # current player move (1 = white)
  for(ft in 1:length(tmpr)){
    # parse san move
    smm <- san.move2move(position,san.move = tmpr[ft],p)
    r1 <- smm[1]
    c1 <- smm[2]
    r2 <- smm[3]
    c2 <- smm[4]
    pr <- smm[5]
    curlan <- paste0(curlan,move2lan(r1,c1,r2,c2,pr)," ")

    # update position
    position <- position.move(position,r1,c1,r2,c2,pr)
    # change side
    p <- -p
  }
  return(trimws(curlan))
}
