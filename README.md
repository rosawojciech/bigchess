# bigchess
Read, Manipulate, Explore Chess PGN Files and R API to UCI Chess Engines

[![version](http://www.r-pkg.org/badges/version/bigchess)](http://www.r-pkg.org/pkg/bigchess)
[![downloads](http://cranlogs.r-pkg.org/badges/bigchess)](http://www.r-pkg.org/pkg/bigchess)

The [`bigchess`] package - Read, Manipulate, Explore Chess PGN Files and R API to UCI Chess Engines
 
For installation you use:

```r
install.packages("bigchess")
# Or
devtools::install_github("rosawojciech/bigchess")
```
Provides functions for reading *.PGN files with more than one game, including large files without copying it into RAM (using 'ff' package or 'RSQLite' package). Handle chess data and chess aggregated data, count figure moves statistics, create player profile, plot winning chances, browse openings. Set of functions of R API to communicate with UCI-protocol based chess engines.

## Getting Started
Core function of this package was initially read.pgn():
### read.pgn()
```r
f <- system.file("extdata", "2016_Candidates.pgn", package = "bigchess")
df <- read.pgn(f)
# ...successfully imported 56 games...
# Example downloaded from https://www.pgnmentor.com/files.html#players and gzipped:
f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
con <- gzfile(f,encoding = "latin1")
df <- read.pgn(con,quiet = TRUE)
# Fastest mode:
con <- gzfile(f,encoding = "latin1")
df <- read.pgn(con,quiet = TRUE,n.moves = FALSE,extract.moves = FALSE,
stat.moves = FALSE, ignore.other.games = FALSE)
# Parse additional tags and extract all moves:
con <- gzfile(f,encoding = "latin1")
df <- read.pgn(con,add.tags = c("WhiteElo", "BlackElo", "ECO"),extract.moves = -1)
# Example of direct downloading data from chess.com using API:
df <- read.pgn("https://api.chess.com/pub/player/fabianocaruana/games/2013/03/pgn")
# Warning of incomplete line could appear

# Example of scraping all of games given user:
user <- "fabianocaruana"
library("rjson")
json_file <- paste0("https://api.chess.com/pub/player/",user,"/games/archives")
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
result <- data.frame()
for(i in json_data$archives)
result <- rbind(result,read.pgn(paste0(i,"/pgn")))
```
In case of big files use read.pgn.ff() or read.pgn.db() instead of read.pgn():
```r
require(ff)
require(ffbase)
f <- system.file("extdata", "Carlsen.gz", package = "bigchess")
con <- gzfile(f,"rbt",encoding = "latin1")
# options("fftempdir"="/path/"...) # if necessarily
fdf <- read.pgn.ff(con,stat.moves = FALSE)
delete(fdf)
# Works with all types of connections (also gz or zip files).
# con argument is passed directly to readLines(con,batch.size)
# so (if total number of lines to read is greater then batch.size)
# depending on platform use it correctly:

# Windows ('rb' opening mode for loop over readLines):
con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),"rb",encoding = "latin1")
# con <- file("path_to_big_chess_file.pgn","rb",encoding = "latin1")
fdf <- read.pgn.ff(con)
delete(fdf)

# Linux/Mac OS X ('r' opening mode for loop over readLines):
con <- gzfile(system.file("extdata", "Carlsen.gz", package = "bigchess"),"r",encoding = "latin1")
# con <- file("path_to_big_chess_file.pgn","r",encoding = "latin1")
fdf <- read.pgn.ff(con)
delete(fdf)

# Windows (example of zipped file handling)
unzf <- unzip("zipped_pgn_file.zip")
fdf <- read.pgn.ff(file(unzf,"rb"))
delete(fdf)
```
### UCI protocol API in R
```r
# Linux (make sure you have executable permission):
engine_path <- "./stockfish_10_x64"
# Windows
# engine_path <- "./stockfish_10_x64.exe"
e <- uci_engine(engine_path)
uci_quit(e)

# Using pipe '%>%' from magrittr:
require(magrittr)
uci_engine(engine_path) %>% uci_quit()
```
### uci_go()
```r
# Linux (make sure you have executable permission):
engine_path <- "./stockfish_10_x64"
# Windows
# engine_path <- "./stockfish_10_x64.exe"
e <- uci_engine(engine_path)
e <- uci_go(depth = 10)
uci_quit(e)
# Using pipe '%>%' from magrittr:
require(magrittr)
uci_engine(engine_path) %>% uci_go(depth = 10) %>% uci_quit()
# Find best answer for black after 1. e4 in 100 seconds:
uci_engine(engine_path) %>% uci_position(moves = "e2e4") %>%
  uci_go(depth = 20) %>% uci_quit() %>% uci_parse()
# Find best answer for black after 1. e4 in 100 seconds:
uci_engine(engine_path) %>% uci_position(moves = "e2e4") %>%
  uci_go(infinite = TRUE,stoptime = 100) %>% uci_quit() %>% uci_parse()
```
### Algebraic notation conversion
```r
san2lan("1. e4 e5 2. Nf3 Nf5 3. d5 ")
lan2san("e2e4 c7c5")
```
## Author

* **Wojciech Rosa** - *Initial work* - [RosaWojciech](https://github.com/RosaWojciech)
