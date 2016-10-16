setwd("../UWDataScience-Spring2016")
source("hp_helper.R")
setwd("../Random-Analyses/")
library(dplyr)
library(tm)

url = "https://www.washingtonpost.com/news/the-fix/wp/2016/09/26/the-first-trump-clinton-presidential-debate-transcript-annotated/"
debate = read_html(url)
debate_txt <- debate %>% html_nodes("p") %>% html_text()
debate_txt <- debate_txt[12:544]

returntextforspeaker = function(names) {
    speakertext <- c(); speaker <- NA
    for (quote in debate_txt[1:length(debate_txt)]) {
      colon <- regexpr(':', quote)[1]
      if (colon != -1) {
        speaker <- substring(quote, 1, colon-1)
      if (speaker %in% names) {
        speakertext <- append(speakertext, substring(quote, colon+1, nchar(quote)))
      }
      } else {
          if (speaker %in% names) {
            speakertext <- append(speakertext, quote)
        }
      }
    }
  return (speakertext)
}

holt <- returntextforspeaker(c('HOLT', 'LESTER HOLT'))
clinton <- returntextforspeaker(c('CLINTON', 'HILLARY CLINTON'))
trump <- returntextforspeaker(c('TRUMP', 'DONALD CLINTON'))


clinton <- remove_punctuation(clinton)
trump <- remove_punctuation(trump)

clinton <- remove_stopwords(clinton)
trump <- remove_stopwords(trump)



trump_corpus <- Corpus(VectorSource(paste(trump, collapse=' ')))
clinton_corpus <- Corpus(VectorSource(paste(clinton, collapse=' ')))

#trump_corpus <- tm_map(trump_corpus, stripWhitespace)
#trump_corpus <- tm_map(trump_corpus, removePunctuation)
#trump_corpus <- tm_map(trump_corpus, removeNumbers)
#trump_corpus <- tm_map(trump_corpus, removeWords, c('CROSSTALK', 'APPLAUSE', 'LAUGHTER'))
# 
# wordcloud(trump_corpus, 
#           scale=c(5,0.5), 
#           max.words=50, 
#           random.order=FALSE, 
#           rot.per=0.35, 
#           colors=brewer.pal(6, 'Dark2'))
# 
#

wordcloud(clinton, rot.per = 0.25, max.words=100, random.color=T, random.order=F, colors = brewer.pal(8, 'Dark2'))
wordcloud(trump, rot.per = 0.25, max.words=100, random.color=T, random.order=F, colors = brewer.pal(8, 'Dark2'))





