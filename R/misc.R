stat_moves_white <- function(moves){
  x <- gsub("([0-9+]{0,}\\.)","",moves,perl = T)
  x <- trimws(gsub("  "," ",x))
  sx <-  strsplit(x," ")
  r <- data.frame(t(sapply(sx,function(i){
    li <- length(i)
    if(li>0) return(as.vector(unlist(table(substr(i[seq(1,li,by = 2)],1,1))[c("B","K","N","O","Q","R")])))
    else return(rep(0, times = 6))
  } )),row.names = NULL)
  r[is.na(r)]<-0
  colnames(r) <- paste0(c("B","K","N","O","Q","R"),"_moves")
  r$R_moves <- r$R_moves + r$O_moves
  r$K_moves <- r$K_moves + r$O_moves
  colnames(r) <- paste0("W_",colnames(r))
  return(r)
}
stat_moves_black <- function(moves){
  x <- gsub("([0-9+]{0,}\\.)","",moves,perl = T)
  x <- trimws(gsub("  "," ",x))
  sx <-  strsplit(x," ")
  r <- data.frame(t(sapply(sx,function(i){
    li <- length(i)
    if(li>1) sli <- seq(2,li,by = 2)
    else sli <- 0
    if(li>1) return(as.vector(unlist(table(substr(i[sli],1,1))[c("B","K","N","O","Q","R")])))
    else return(rep(0, times = 6))
  } )),row.names = NULL)
  r[is.na(r)]<-0
  colnames(r) <- paste0(c("B","K","N","O","Q","R"),"_moves")
  r$R_moves <- r$R_moves + r$O_moves
  r$K_moves <- r$K_moves + r$O_moves
  colnames(r) <- paste0("B_",colnames(r))
  return(r)
}
#### SAN2LAN & LAN2SAN
position.move <- function(position,r1,c1,r2,c2,p = NA){
  p1 <- position[r1,c1]
  p2 <- position[r2,c2]
  position[r2,c2] <- position[r1,c1]
  position[r1,c1] <- 0
  # castling
  if(abs(p1)==6 & abs(c1-c2)>1){
    # short castling
    if(c2 == 7) {
      position[r1,6] <- position[r1,8]
      position[r1,8] <- 0
    }
    # long castling
    if(c2 == 3) {
      position[r1,4] <- position[r1,1]
      position[r1,1] <- 0
    }
  }
  # en passant
  if(abs(p1)==1 & abs(c1-c2)>0 & p2==0){
    position[r1,c2] <-0
  }
  # promotion
  if(!is.na(p)) position[r2,c2] <- p*p1
  return(position)
}
position.start <- function(){
  pstns <- matrix(data = rep(0,times = 64),ncol = 8,nrow = 8,dimnames = list(8:1,letters[1:8]))
  # 1 pawns
  pstns["2",] <- 1
  # 4 rook
  # 3 knight
  # 2 bishop
  # 5 queen
  # 6 king
  pstns["1",] <- c(4,3,2,5,6,2,3,4)
  # the same story for black with minus sign
  pstns["7",] <- -1
  pstns["8",] <- -c(4,3,2,5,6,2,3,4)
  return(pstns)
}
print.pos <- function(position){
  white_ucode <- c('\U2659','\U2657','\U2658','\U2656','\U2655','\U2654')
  black_ucode <- c('\U265F','\U265D','\U265E','\U265C','\U265B','\U265A')
  for(fr in 1:8){
    rslt <- NULL
    for(fc in 1:8){
      pff <- position[fr,fc]
      if(pff>0) tmp <- white_ucode[pff]
      if(pff<0) tmp <- black_ucode[-pff]
      if(pff == 0)  tmp <- '\U26DD'
      rslt <- paste0(rslt,tmp)
    }
    cat(paste0(rslt,'\n'))
  }
}
string.lan.move2move <- function(slm){
  lslm <- strsplit(slm,"")[[1]]
  rslm <- NULL
  rslm[2] <- cn[lslm[1]]
  rslm[1] <- rn[lslm[2]]
  rslm[4] <- cn[lslm[3]]
  rslm[3] <- rn[lslm[4]]
  rslm[5] <- figs[casefold(lslm[5])]
  return(rslm)
}
internal <- function(vec,xp){ # internal function for computing possible piece xp moves in vector vec
  rslt <- NULL
  if(xp<length(vec)){ # moves to the right
    wvx <- which(vec[(xp+1):(length(vec))]!=0)
    if(length(wvx)>0) { # are there other pieces?
      mwvx <- min(wvx)
      if(vec[xp+mwvx]*vec[xp]>0) mwvx <- mwvx-1 # if this piece could not be taken then remove it from possible moves
      if(mwvx>0) rslt <- c(rslt,(xp+1):(xp+mwvx))
    }
    else {
      rslt <- c(rslt,(xp+1):length(vec))
    }
  }
  if(xp>1){ # moves to the left
    wvx <- which(vec[1:(xp-1)]!=0)
    if(length(wvx)>0) { # are there other pieces?
      mwvx <- max(wvx)
      if(vec[mwvx]*vec[xp]>0) mwvx <- mwvx+1 # if this piece could not be taken then remove it
      if(mwvx<xp) rslt <- c(rslt,(mwvx):(xp-1))
    }
    else {
      rslt <- c(rslt,1:(xp-1))
    }
  }
  return(rslt)
}
checking.moves <- function(position,r1,c1){
  p1 <- position[r1,c1]
  ap1 <- abs(p1)
  pm <- matrix(ncol = 2)[-1,]
  if(ap1 == 4 || ap1 == 5){ # rook and queen
    ipr1 <- internal(position[r1,],c1)
    if(length(ipr1)>0) pm <- rbind(pm,cbind(r1,ipr1))
    ipc1 <- internal(position[,c1],r1)
    if(length(ipc1)>0) pm <- rbind(pm,cbind(ipc1,c1))
  }
  if(ap1 == 2 || ap1 == 5){ # bishop and queen
    d1rc <- diag1[r1,c1]
    spd1 <- split(position,diag1)[[as.character(d1rc)]]
    if(length(spd1)>1){
      md1rc <- min(d1rc,0)
      is1 <- internal(spd1,c1+md1rc)
      if(length(is1)>0)
        pm <- rbind(pm,cbind(r1+is1-c1-md1rc,is1-md1rc))
    }
    d2rc <- diag2[r1,c1]
    spd2 <- split(position,diag2)[[as.character(diag2[r1,c1])]]
    if(length(spd2)>1){
      md2rc <- min(d2rc,0)
      is2 <- internal(spd2,c1+md2rc)
      if(length(is2)>0)
        pm <- rbind(pm,cbind(r1-is2+c1+md2rc,is2-md2rc))
    }
  }
  if(ap1 == 3){ # knight
    pm <- kni
    pm[,1] <- pm[,1]+r1
    pm[,2] <- pm[,2]+c1
    pm <- pm[pm[,1]>0 & pm[,1]<9 & pm[,2]>0 & pm[,2]<9,]
    pm <- pm[position[pm]*p1<=0,]
  }
  if(ap1 == 1){ #pawn
    if(position[r1-p1,c1]==0) pm <- rbind(pm,c(r1-p1,c1)) #typical pawn move
    if((p1 == 1 & r1 == 7)||(p1 == -1 & r1 == 2))
      if(position[r1-2*p1,c1] == 0 & position[r1-p1,c1] == 0)
        pm <- rbind(pm,c(r1-2*p1,c1)) # first doubled move
      #capturing:
      if(c1>1)
        if(position[r1-p1,c1-1]*p1<0)
          pm <- rbind(pm,c(r1-p1,c1-1))
        if(c1<8)
          if(position[r1-p1,c1+1]*p1<0)
            pm <- rbind(pm,c(r1-p1,c1+1))
  }
  if(ap1 == 6){ # king
    pm <- kin
    pm[,1] <- pm[,1]+r1
    pm[,2] <- pm[,2]+c1
    pm <- pm[pm[,1]>0 & pm[,1]<9 & pm[,2]>0 & pm[,2]<9,]
    pm <- pm[position[pm]*p1<=0,]
  }
  return(matrix(pm,ncol = 2))
}
possible.move <- function(position,r1,c1,r2,c2){ # return TRUE if [r1,c1] -> [r2,c2] is possible, FALSE otherwise
  pmp <- possible.moves(position,r1,c1)
  if(nrow(pmp)>0)
    return(sum(pmp[,1]==r2 & pmp[,2]==c2)>0)
  else return(FALSE)
}
is.check <- function(position,p){ # is p pieces checking -p king?
  rslt <- FALSE
  kp <- which(position == -p*6,arr.ind = T) # kingposition
  kp1 <- kp[,1]
  kp2 <- kp[,2]
  for(fr in 1:8){
    for(fc in 1:8){
      if(position[fr,fc]*p>0){
        pm <- checking.moves(position,fr,fc)
        if(nrow(pm)>0)
          if(sum(pm[,1]==kp1 & pm[,2]==kp2)>0)
            rslt <- TRUE
      }
      if(rslt) break()
    }
    if(rslt) break()
  }
  return(rslt)
}
is.mate <- function(position,p){ # is p pieces mating -p king?
  rslt <- TRUE
  for(fr in 1:8){
    for(fc in 1:8){
      if(position[fr,fc]*p<0){
        pm <- possible.moves(position,fr,fc)
        if(nrow(pm)>0) rslt <- FALSE
      }
      if(!rslt) break()
    }
    if(!rslt) break()
  }
  return(rslt)
}
possible.moves <- function(position,r1,c1){
  # ep wsc wlc bsc blc should be defined
  p1 <- position[r1,c1]
  ap1 <- abs(p1)
  psmv <- matrix(ncol = 2)[-1,]
  if(ap1 < 7){ #
    rslt <- checking.moves(position,r1,c1)
    if(nrow(rslt)>0)
      for(ff in 1:nrow(rslt)){
        tmpp <- position.move(position,r1,c1,rslt[ff,1],rslt[ff,2])
        if(!is.check(tmpp,sign(-p1))) psmv <- rbind(psmv,rslt[ff,])
      }
  }
  return(psmv)
}
disambiguating.move <- function(position,r1,c1,r2,c2){
  p1 <- position[r1,c1]
  ap1 <- abs(p1)
  tsr <- FALSE # the same rank
  tsf <- FALSE # the same file
  oth <- FALSE # other cases
  dsmb <- ""
  if(abs(p1)==4){ # rook
    wpp <- which(position == p1,arr.ind = T)
    wppp <- matrix(wpp[!(wpp[,1]==r1 & wpp[,2]==c1),],ncol = 2)
    nw <- nrow(wppp)
    if(nw>0){
      for(ff in 1:nw){
        dr <- wppp[ff,]
        if(possible.move(position,dr[1],dr[2],r2,c2))
        {if(dr[1]==r1) {
          dsmb <- letters[c1]
          break()
        }
          else{ dsmb <- as.character(9-r1)}}
      }
    }
  }

  if(abs(p1)>1 & abs(p1)<6){ # bishop, knight and queen
    wpp <- which(position == p1,arr.ind = T)
    wppp <- matrix(wpp[!(wpp[,1]==r1 & wpp[,2]==c1),],ncol = 2)
    nw <- nrow(wppp)
    if(nw>0){
      for(ff in 1:nw){
        dr <- wppp[ff,]
        if(possible.move(position,dr[1],dr[2],r2,c2)){
          if(dr[1]==r1) tsr <- TRUE
          else if(dr[2]==c1) tsf <- TRUE
          else oth <- TRUE
        }
      }
      if(tsr & tsf) dsmb <- paste0(letters[c1],9-r1)
      else if(tsr & !tsf) dsmb <- letters[c1]
      else if(tsf) dsmb <- as.character(9-r1)
      else if(oth) dsmb <- letters[c1]
    }
  }
  return(dsmb)
}
cleanup_san <- function(movetext.san){
  rslt <- gsub("[0-9]+\\.","",movetext.san)
  rslt <- gsub("  "," ",rslt)
  rslt <- gsub("[#x\\+]","",rslt)
  return(trimws(rslt))
}
move2san <- function(position,r1,c1,r2,c2,p){
  p1 <- position[r1,c1]

  # capture
  if(abs(position[r2,c2])>0) { # if there is a capture
    capt <- "x"
    if(abs(p1)==1) # capture by a pawn
    {capt <- paste0(letters[c1],"x")}
  }
  else{capt <- ""}

  sc2 <- letters[c2] # file of a destination
  sr2 <- c(8:1)[r2] # row of a destination

  m2s <- paste0(casefold(names(figs[abs(p1)]),upper = T),disambiguating.move(position,r1,c1,r2,c2),capt,sc2,sr2)
  # castling
  if(abs(p1)==6 & abs(c1-c2)>1){
    # short castling
    if(c2 == 7) m2s <- "O-O"
    # long castling
    if(c2 == 3) m2s <- "O-O-O"
  }
  # en passant
  if(abs(p1)==1 & abs(c1-c2)>0 & position[r2,c2]==0) m2s <- paste0(letters[c1],"x",sc2,sr2)
  # promotion
  if(!is.na(p)) {
    m2s <- paste0(capt,sc2,sr2,"=",casefold(names(figs[p]),upper = T))
  }

  return(m2s)
}
san.move2move <- function(position, san.move,p){
  ptrns <- c("[a-z]+[0-9]","[A-Z][a-z]+[0-9]","[A-Z][0-9][a-z][0-9]","[A-Z][a-z][0-9][a-z][0-9]","O-O","O-O-O","[a-z]+[0-9]=[A-Z]") # san.move2move
  for(fp in 1:length(ptrns)){
    m <- gregexpr(paste0("^",ptrns[fp],"$"),san.move)

    if(length(regmatches(san.move,m)[[1]])>0) break()
  }
  sm <- strsplit(san.move,"")[[1]]
  lsm <- length(sm)
  pr <- NULL
  switch(fp,
         '1' = { # pawn
           if(lsm == 2){
             r2 <- rn[sm[2]]
             c2 <- cn[sm[1]]
             wp <- which(position == p,arr.ind = T)
             wpp <- matrix(wp[wp[,2]==c2,],ncol = 2)
             for(fr in 1:nrow(wpp))
               if(possible.move(position,wpp[fr,1],wpp[fr,2],r2,c2)) break()
             r1 <- wpp[fr,1]
             c1 <- wpp[fr,2]
           }
           if(lsm == 3){ # pawn with capture
             r2 <- rn[sm[3]]
             c2 <- cn[sm[2]]
             c1 <- cn[sm[1]]
             #wp <- which(position == p,arr.ind = T)
             #wpp <- matrix(wp[wp[,2]==c1,],ncol = 2)
             #for(fr in 1:nrow(wpp))
             #if(possible.move(position,wpp[fr,1],c1,r2,c2)) break()
             #r1 <- wpp[fr,1]
             r1 <- r2+p
             #c1 <- wpp[fr,2]
           }
         },
         '2' = { # figure
           if(lsm == 3){ # without dsmb moves
             r2 <- rn[sm[3]]
             c2 <- cn[sm[2]]
             fig <- figs[casefold(sm[1])]*p
             wp <- which(position == fig,arr.ind = T)
             for(fr in 1:nrow(wp))
               if(possible.move(position,wp[fr,1],wp[fr,2],r2,c2)) break()
             r1 <- wp[fr,1]
             c1 <- wp[fr,2]
           }
           if(lsm == 4){ # dsmb move with file info
             r2 <- rn[sm[4]]
             c2 <- cn[sm[3]]
             c1 <- cn[sm[2]]
             fig <- figs[casefold(sm[1])]*p
             wp <- which(position == fig,arr.ind = T)
             wpp <- matrix(wp[wp[,2]==c1,],ncol = 2)
             for(fr in 1:nrow(wpp))
               if(possible.move(position,wpp[fr,1],c1,r2,c2)) break()
             r1 <- wpp[fr,1]
           }
         },
         '3' = {
           if(lsm == 4){ # dsmb move with range info
             r2 <- rn[sm[4]]
             c2 <- cn[sm[3]]
             r1 <- rn[sm[2]]
             fig <- figs[casefold(sm[1])]*p
             wp <- which(position == fig,arr.ind = T)
             wpp <- matrix(wp[wp[,1]==r1,],ncol = 2)
             for(fr in 1:nrow(wpp))
               if(possible.move(position,r1,wpp[fr,2],r2,c2)) break()
             c1 <- wpp[fr,2]
           }
         },
         '4' = { # rare case dsmb move with file and range info
           r2 <- rn[sm[5]]
           c2 <- cn[sm[4]]
           r1 <- rn[sm[3]]
           c1 <- cn[sm[2]]
         },
         '5' = { # short castling
           c2 <- 7
           if(p == 1) r1 <- r2 <- 8
           if(p == -1) r1 <- r2 <- 1
           c1 <- 5
         },
         '6' = { # long castling
           c2 <- 3
           if(p == 1) r1 <- r2 <- 8
           if(p == -1) r1 <- r2 <- 1
           c1 <- 5
         },
         '7' = { # promotion
           if(lsm == 4){ # without capture
             pr = figs[casefold(sm[4])]
             r2 <- rn[sm[2]]
             c1 <- c2 <- cn[sm[1]]
           }
           if(lsm == 5){ # with capture
             pr = figs[casefold(sm[5])]
             r2 <- rn[sm[3]]
             c2 <- cn[sm[2]]
             c1 <- cn[sm[1]]
           }
           if(p == 1) r1 <- 2
           if(p == -1) r1 <- 7
         }
  )
  return(c(r1,c1,r2,c2,pr))
}
move2lan <- function(r1,c1,r2,c2,pr){
  rslt <- paste0(letters[c1],9-r1,letters[c2],9-r2)
  if(!is.na(pr)) rslt <- paste0(rslt,casefold(names(figs)[abs(pr)],upper = T))
  return(rslt)
}
rn <- 1:8
names(rn) <- 8:1
figs <- 1:6
names(figs) <- c("","b","n","r","q","k")
ps <- position.start()
diag1 <- row(ps)-col(ps) # possible moves for bishop
cn <- 1:8
names(cn) <- letters[1:8]
diag2 <- -diag1[,8:1]
kni <- matrix(data = c(-1,-1,-2,-2,1,1,2,2,2,-2,1,-1,2,-2,1,-1),nrow = 8,ncol = 2) # possible moves for knight
kin <- matrix(data = c(-1,-1,-1,0,0,1,1,1,-1,0,1,-1,1,-1,0,1),nrow = 8,ncol = 2) # possible moves for king

