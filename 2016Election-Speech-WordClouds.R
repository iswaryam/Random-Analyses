setwd("../UWDataScience-Spring2016")
source("hp_helper.R")
setwd("../Random-Analyses/")


plot_speech_wordcloud = function (speech, title)
{
  speech <- paste(speech, collapse=" ")
  speech <- remove_stopwords(speech)
  speech <- remove_punctuation (speech)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 40))
  par(mar=rep(0, 4));
  plot.new();text(x=0.5, y=0.5, title, col = 'Red', font = 2, family="sans")
  wordcloud(speech, scale=c(4,1.25),rot.per = 0.25, max.words=150, random.color=T, random.order=F, colors = brewer.pal(8, 'Dark2'))
}

trump = read_html("http://www.politico.com/story/2016/07/full-transcript-donald-trump-nomination-acceptance-speech-at-rnc-225974")
trump_txt <- trump %>% html_nodes("p") %>% html_text()
trump_txt <- trump_txt [155:263]
plot_speech_wordcloud (trump_txt, "Trump Nomination - RNC 2016")

michelle = read_html("http://abcnews.go.com/Politics/full-text-michelle-obamas-2016-democratic-national-convention/story?id=40884459")
michelle_txt <- michelle %>% html_nodes("p") %>% html_text()
michelle_txt <- michelle_txt [2:20]
plot_speech_wordcloud (michelle_txt, "Michelle Obama - DNC 2016")
