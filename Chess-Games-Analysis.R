library(ggplot2)
library(rchess)
library(dplyr)

#################
###### Read pgn file 
filename = "chess.txt"
chess_txt = readChar(filename, file.info (filename)$size)
x = read.delim(filename, header = FALSE, sep=" ")
chess_txt = read.delim(filename, header = FALSE, stringsAsFactors = FALSE)
chess_df = data.frame(Date=as.Date(character()), My_Color=as.character(), Opp_Color=as.character(), 
                      My_Result=as.character(), My_Rank=numeric(), Opp_Rank=as.numeric(), 
                      Termination_Reason=as.character(), Sequence=as.character(), stringsAsFactors = FALSE)
j = 1

######################################################################################################
###### Parsing pgn file and splitting columns
for (i in 1:nrow(chess_txt)) {
  if (chess_txt[i,]=="[Event Live Chess]")
  {
    i <- i + 2
    # Date
    chess_df[j, 1] = as.Date(regmatches(chess_txt[i,],regexpr("[0-9]{4}.[0-9]{2}.[0-9]{2}", chess_txt[i,])), format = "%Y.%m.%d")
    #chess_df$Date[[j]] = as.Date(regmatches(chess_txt[i,],regexpr("[0-9]{4}.[0-9]{2}.[0-9]{2}", chess_txt[i,])), format = "%Y.%m.%d")
    i <- i + 1
    if(chess_txt[i,] == "[White pramanat]")
       {
        # My Color
         chess_df[j,2] <- "White"
         # Opponent COlor
         chess_df[j,3] <- "Black"
       }else {
         chess_df[j,2] <- "Black"
         chess_df[j,3] <- "White"
       }
    my_color <- chess_df[j,2]
    i = i + 2
    if((chess_txt[i,] == "[Result 1-0]" & my_color=="White") | (chess_txt[i,] == "[Result 0-1]" & my_color=="Black"))
    {
      # My Result
      chess_df[j,4] <- "Won"
     }else if((chess_txt[i,] == "[Result 1-0]" & my_color=="Black") | (chess_txt[i,] == "[Result 0-1]" & my_color=="White"))
      {
      chess_df[j,4] <- "Loss"
      } else if (chess_txt[i,] == "[Result 1/2-1/2]")
      {
        chess_df[j,4] <- "Draw"
      }else {chess_df[j,4] = "Oops!Unknown"}
    i = i + 1
    white_rank <- as.numeric(regmatches(chess_txt[i,],regexpr("[[:digit:]]+", chess_txt[i,])))
    black_rank <- as.numeric(regmatches(chess_txt[i+1,],regexpr("[[:digit:]]+", chess_txt[i+1,])))
    
    if(my_color == "White") {
      # My Rank
      chess_df[j,5] <-  white_rank
      # Opponent Rank
      chess_df[j,6]<- black_rank
    } else {
      chess_df[j,5] <- black_rank
      chess_df[j,6] <- white_rank
    }
    i <- i + 3
    term_array <- unlist(strsplit(chess_txt[i,], " "))
    # Termination Reason
    chess_df[j,7] <- gsub("]", "", term_array[length(term_array)])
    i <- i+1
    # Move to append all lines of the sequence
    seq_text <- ''; continue <- TRUE;
    while(continue) {
      seq_text <- paste(seq_text,chess_txt[i,])
      if (i+1 > nrow(chess_txt) || chess_txt[i+1,]=="[Event Live Chess]")
      {
        continue <- FALSE
        break
      }
      i <- i+1
    }
    # Sequence
    chess_df[j,8] <- seq_text
    j <- j + 1
  }
  i <- i + 1
}
head(chess_df)

#####################################################################################
#### Lets run some analytics on win vs loss

table(chess_df$My_Color)
table(chess_df$My_Result)
table(chess_df$My_Result, chess_df$My_Color)
prop.table(table(chess_df$My_Result, chess_df$My_Color),2)

table(chess_df$Termination_Reason[chess_df$My_Result=="Won"])
table(chess_df$Termination_Reason[chess_df$My_Result=="Loss"])

table(chess_df$My_Result[chess_df$My_Rank < chess_df$Opp_Rank])
table(chess_df$My_Result[chess_df$My_Rank >= chess_df$Opp_Rank])

#####################################################################################
#### Plot my ranks against my opponents  (fun surprise: it's a notmal distribution!)
hist(chess_df$Opp_Rank, col = 'Salmon', xlab = 'Rating Distribution (Opponent is Red)', main = 'My ELO Rating vs Opponents - ALL')
hist(chess_df$My_Rank, col = 'DodgerBlue', add=T)

plot(chess_df$My_Rank[chess_df$My_Result=="Won"], chess_df$Opp_Rank[chess_df$My_Result=="Won"],col = 'DarkGreen', pch=19
     ,xlab = 'My Rank', ylab = 'Opponent Rank')
par(new=T)
plot(chess_df$My_Rank[chess_df$My_Result=="Loss"], chess_df$Opp_Rank[chess_df$My_Result=="Loss"],col = 'Red', pch=19
     ,xlab = 'My Rank', ylab = 'Opponent Rank', yaxt='n', xaxt='n', main='Red = Loss; Green = Win')

#hist(chess_df$Opp_Rank[chess_df$My_Result=="Won"], col = 'Salmon', xlab = 'Rating Distribution (Opponent is Red)', main = 'My ELO Rating vs Opponents - When I Win')
#hist(chess_df$My_Rank[chess_df$My_Result=="Won"], col = 'DodgerBlue', add=T)

#hist(chess_df$Opp_Rank[chess_df$My_Result=="Loss"], col = 'Salmon', xlab = 'Rating Distribution (Opponent is Red)', main = 'My ELO Rating vs Opponents - When I Lose')
#hist(chess_df$My_Rank[chess_df$My_Result=="Loss"], col = 'DodgerBlue', add=T)

#####################################################################################
##### Playin around with sequences........
loss <- vector("list", 50)
win <- vector("list", 50)

for (x in 1:nrow(chess_df))
  {
  seq = chess_df$Sequence[x]
  col = chess_df$My_Color[x]
  res = chess_df$My_Result[x]
  seq_array <- regmatches(seq, gregexpr(".[A-z|+|0-9]+ ", seq, perl=TRUE));#seq_array
  seq_array <- unlist(seq_array)
  seq_array <- gsub(".","",seq_array, fixed=TRUE)
  seq_array <- gsub(" ","",seq_array, fixed=TRUE)
  
  seq_split <- split(seq_array, 1:2)
  
  if (col == 'Black')
    {
      myseq <- seq_split[2]
    }else
    {
      myseq <- seq_split[1]
    }
  names(myseq) <- 'col'
  if (res=="Loss")
  {
    loss[[x]] <- myseq$col
  } else
  {
    win[[x]] <- myseq$col
  }
}
loss <- Filter(Negate(function(x) is.null(unlist(x))), loss)
win <- Filter(Negate(function(x) is.null(unlist(x))), win)


################# Using the rchess lib #################

# chesswc <- chess_df$Sequence %>% mutate(game_id = seq(nrow(.)))
# chess_df$game_id = as.numeric(chess_df$Date)
# 
# pgn <- chess_df[1,8]
# #str_sub(pgn, 0, 50)
# 
# chss <- Chess$new()
# chss$load_pgn(pgn)
# plot(chss)
# 
# chss$moves(verbose=TRUE)
# chss$history(verbose=TRUE)
# 
# x = chss$history_detail() %>%
#   arrange(number_move) %>% 
#   head(50)
####################################################################


# pgn <- system.file("chess.txt", package = "rchess")
# pgn <- readLines("chess.pgn", warn = FALSE)
# pgn <- paste(pgn, collapse = "n")
# 
# chsspgn <- Chess$new()
# 
# chsspgn$load_pgn(pgn)
# cat(chsspgn$pgn())
# 
# chsspgn$history_detail()
# chsspgn$history(verbose = TRUE)

#chesswc <- chesswc %>% mutate(game_id = seq(nrow(.)))
# 
# pgn  = NA
# x = data(chesswc$pgn)
# 
# 
#  dfmoves <- plyr::adply(chess_df %>% select(Sequence, game_id), .margins = 1, function(x){
#    chss <- Chess$new()
#    chss$load_pgn(x$Sequence)
#    chss$history_detail()
#  }, .parallel = TRUE, .paropts = list(.packages = c("rchess")))
# 
# 
# dfmoves <- plyr::adply(chess_df %>% select(pgn, Date, My_Color, Opp_Color, My_Result), .margins = 1, function(x){
#   chss <- Chess$new()
#   chss$load_pgn(x$pgn)
#   chss$history_detail()
# }, .parallel = TRUE, .paropts = list(.packages = c("rchess")))
