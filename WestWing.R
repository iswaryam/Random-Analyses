setwd("../UWDataScience-Spring2016")
source("hp_helper.R")
setwd("../Random-Analyses/")
library(dplyr)

season = 1
ww_df <- data.frame(characters=NA,dialogue=NA,season=NA,episode=NA)
last_episode=21

for (episode in 1:last_episode)
{
ww = read_html(paste("http://www.westwingtranscripts.com/search.php?flag=getTranscript&id=",episode, sep=""))
ww_txt <- ww %>% html_nodes("pre") %>% html_text()

gsub(' [OS]', '', ww_df$characters)
gsub(' [cont.]', '', ww_df$characters)
gsub(' [CONT]', '', ww_df$characters)
gsub(' ZIEGLER', '', ww_df$characters)
gsub(' SEABORN', '', ww_df$characters)


#head(ww_txt)

#x = gregexpr("\\n\\n[A-z ]+\\n", ww_txt, perl=TRUE)
#regmatches(ww_txt, x)

ww_array <- regmatches(ww_txt, gregexpr("\\n\\n[A-z |.]+\\n[[A-z --|.|!|?|'|,|\\[|\\] ]+\\n\\n", ww_txt, perl=TRUE))

ww_array <- unlist(ww_array)
len <- length(ww_array)

characters = c(NA); dialogue = c(NA)

for (x in 1:len)
{
line <- ww_array[x]
characters[x]<- gsub('\n', '',regmatches(line, gregexpr("\\n\\n[A-z |.]+\\n", line, perl=TRUE)))
dialogue[x] <- gsub('\n', '',regmatches(line, gregexpr("\\n[[A-z --|.|!|?|'|,|\\[|\\] ]+\\n\\n", line, perl=TRUE)))
}
ww_df <- rbind(ww_df,data.frame(characters,dialogue,season=rep(season,len),episode=rep(episode,len)))
}

freq <- head(sort(table(ww_df$characters), decreasing = TRUE),28)

women = c('DONNA','C.J.','MANDY','ABBEY','ZOEY', 'MALLORY', 'MARGARET', 'LAURIE', 'MRS. LANDINGHAM', 'JOEY [KENNY]', 
          'ANDY', 'JOEY', 'CAROL', 'CATHY', 'GINA', 'DAISY', 'BONNIE')

par(mar = c(7, 3, 2, 2) + 1.5)
barplot(freq, las=2, cex.names = 0.75, ylab = 'Number of Lines spoken',main='The West Wing-Season 1 Transcripts', col=ifelse(names(freq) %in% women ,"palegreen2","gray87"))

women_lines <- sum(freq[names(freq) %in% women])
men_lines <- sum(freq[!names(freq) %in% women])
women_lines; men_lines; women_lines/sum(freq)
