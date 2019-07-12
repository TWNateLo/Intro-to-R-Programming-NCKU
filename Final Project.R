## Final Project

## Author:
## Nate Lo (LO,CHENG-HSIN)
## from NCKU FLLD (B24046037)
## Free for non-business application

## Notes of this project:
## https://hackmd.io/@TWNateLo/ncku2019rprogrammingfinal

## References:
## https://bnosac.github.io/udpipe/docs/doc5.html
## http://ufal.mff.cuni.cz/udpipe

## Set WD
## setwd("D:/@@ R Language Course/Final")
setwd("C:/Users/USER/Documents")

## Loading required packages for basic Summary
library(dplyr)
library(ggplot2)


## Understanding the basic distribution of News articles published
news <- read.csv('abcnews-date-text.csv', header = T, stringsAsFactors = F)
news %>% group_by(publish_date) %>% count() %>% arrange(desc(n))


## Plotting to understand how the frequency of headlines is
news %>% group_by(publish_date) %>% count() %>% ggplot() + geom_line(aes(publish_date,n, group = 1))


## Before we move on to perform text analysis let's split year, month and date
library(stringr)
news_more <- news %>% mutate(year = str_sub(publish_date,1,4),
                             month = str_sub(publish_date,5,6),
                             date = str_sub(publish_date,7,8))


## Show distribution of data based on year
news_more %>% group_by(year) %>% count()  %>% ggplot() + geom_bar(aes(year,n), stat ='identity')



## ---------------------------------------------------


## Loading R package and Getting Language Model ready
library(udpipe)
#model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.3-181115.udpipe')


## Filtering data only for 2008
news_more_2008 <- news_more %>% filter(year == 2008)


## Annotate Input Text Data for 2008
s <- udpipe_annotate(udmodel_english, news_more_2008$headline_text)
x <- data.frame(s)


## Universal POS
library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## Most Occurring Nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


## Most Occurring Adjectives
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")


## Most Occurring Verbs
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")



## --------------------------------------------------------------------------



## Automated Keywords Extraction with RAKE

## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")



## TOP NOUN/NOUN-VERB Pairs as Keyword pairs
## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")



## EXTRA

## Using Pointwise Mutual Information Collocations (PMI)
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")



## Nouns / adjectives used in same sentence
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
term = "lemma", 
group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")


## Nouns / adjectives which follow one another
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")



