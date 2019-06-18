#' Movetext conversion from LAN to SAN
#'
#' Convert LAN movetext (long algebraic notation used by chess engines) to SAN movetext (standard algebraic notation required by FIDE)
#'
#' @param movetext.lan movetext string in long algebraic notation (LAN), but without comments, variants etc.
#' @return movetext in standard algebraic notation
#'
#' @examples
#' lan2san("e2e4 c7c5")
#' @export
lan2san <- function(movetext.lan){
  tmpr <- strsplit(trimws(movetext.lan)," ")[[1]]
  cursan <- ""
  position <- position.start()
  for(ft in 1:length(tmpr)){
    # parse lan move
    slm <- string.lan.move2move(tmpr[ft])

    if(ft%%2) cursan <- paste0(cursan,round((ft+1)/2),". ")
    r1 = slm[1]
    c1 = slm[2]
    r2 = slm[3]
    c2 = slm[4]
    p = slm[5]
    p1 <- position[r1,c1]
    cursan <- paste0(cursan,move2san(position,slm[1],slm[2],slm[3],slm[4],slm[5]))

    # update position
    position <- position.move(position,slm[1],slm[2],slm[3],slm[4],slm[5])

    # check
    if(is.check(position,sign(p1))){
      if(is.mate(position,sign(p1)))
        cursan <- paste0(cursan,"# ")
      else cursan <- paste0(cursan,"+ ")
    }
    else {cursan <- paste0(cursan," ")}
  }
  return(trimws(cursan))
}
