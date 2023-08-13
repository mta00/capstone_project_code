#import libraries
library(tidyverse)
library(stringr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.dictionaries)
library(ggplot2)

#paths to folders with CSV files extracted using the Reddit API
posts_path <- "/Users/masha/Desktop/reddit/posts"
comms_path <- "/Users/masha/Desktop/reddit/comments"

#get the paths to all files in the folders
p_files <- list.files(posts_path, all.files = TRUE, 
                      full.names = TRUE)

c_files <- list.files(comms_path, all.files = TRUE, 
                      full.names = TRUE)

#eliminate unnecessary files
p_files <- p_files[4:length(p_files)]

c_files <- c_files[3:length(c_files)]

#make a list of columns we want to extract
cols <- c("subreddit", "body", "utc_datetime_str")

#read the first CSV file into R to initialize a table
p_init <- read.csv(p_files[1])

#concatenate the title and the body of text posts
p_init <- p_init %>%
  mutate_at(c('selftext'), ~replace_na(.,""))
p_init$body <- paste(p_init$title, p_init$selftext)
p_init <- p_init[,cols]

#do the same for all remaining files in the folder
for (i in 2:length(p_files)){
  p <- read.csv(p_files[i])
  p <- p %>%
    mutate_at(c('selftext'), ~replace_na(.,""))
  p$body <- paste(p$title, p$selftext)
  p <- p[,cols]
  p_init <- rbind(p, p_init)
}

#read the comment files into R and bind them with the table created above
for (i in 1:length(c_files)){
  c <- read.csv(c_files[i])
  c <- c[,cols]
  p_init <- rbind(c, p_init)
}

#store as CSV
write.csv(p_init, "/Users/masha/Desktop/reddit/p_init.csv", row.names=FALSE)

#read the file
p_init <- read_csv("/Users/masha/Desktop/reddit/p_init.csv")

#get the names of subreddits on the dataset
subs <- unique(p_init$subreddit)

#initialize empty lists for storing data
lengths <- c()
removed <- c()
readability_scores <- c()
lexdiv_scores <- c()
tokens_num <- c()
types_num <- c()
punc_num <- c()
numbers_num <- c()
symbols_num <- c()
urls_num <- c()
tags_num <- c()
emojis_num <- c()

#gather corresponding information for all of the subreddits
for (i in 1:length(subs)){
  print(subs[i])
  
  sub_df <- p_init[p_init$subreddit == subs[i], ]
  
  lengths[i] <- nrow(sub_df)
  removed[i] <- nrow(sub_df[sub_df$body == "[removed]", ])
  
  all_tok <- tokens(str_c(sub_df$body, sep = "", collapse = " "))
  
  summary_all <- textstat_summary(all_tok)
  
  tokens_num[i] <- summary_all$tokens
  types_num[i] <- summary_all$types
  punc_num[i] <- summary_all$puncts
  numbers_num[i] <- summary_all$numbers
  symbols_num[i] <- summary_all$symbols
  urls_num[i] <- summary_all$urls
  tags_num[i] <- summary_all$tags
  emojis_num[i] <- summary_all$emojis
  
  readability_scores[i] <- textstat_readability(str_c(sub_df$body, sep = "", collapse = " "), measure = "Flesch")$Flesch
  lexdiv_scores[i] <- textstat_lexdiv(all_tok, measure = "TTR")$TTR
}

#create a dataframe out of the lists containing the information we just gathered
summary <- data.frame(
  subreddit = subs,
  n_documents = lengths,
  n_removed = removed,
  n_tokens = tokens_num,
  n_types = types_num,
  n_punct = punc_num,
  n_symbols = symbols_num,
  n_urls = urls_num,
  n_tags = tags_num,
  n_emojis = emojis_num,
  readability = readability_scores,
  lex_div = lexdiv_scores
)

#divide by the number of documents to account for the imbalanced classes
summary$n_tokens_adj <- summary$n_tokens_adj/summary$n_documents
summary$n_removed_adj <- summary$n_removed/summary$n_documents
summary$n_punct_adj <- summary$n_punct_adj/summary$n_documents
summary$n_symbols_adj <- summary$n_symbols_adj/summary$n_documents
summary$n_urls_adj <- summary$n_urls_adj/summary$n_documents
summary$n_tags_adj <- summary$n_tags_adj/summary$n_documents
summary$n_emojis_adj <- summary$n_emojis_adj/summary$n_documents

#store as CSV
write.csv(df, "/Users/masha/Desktop/reddit/summary.csv", row.names=FALSE)

#get the previously saved summary file
summary <- read_csv("/Users/masha/Desktop/reddit/summary.csv",
                    show_col_types = FALSE)

#let's work with the sentiment of the content
#create a corpus object
corpus_reddit <- corpus(p_init, text_field = "body")

#first, use the Lexicoder Sentiment Dictionary
toks_clean <- tokens(corpus_reddit, remove_punct = TRUE, 
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_symbols = TRUE,
                     remove_separators = TRUE) %>%
  tokens_remove(stopwords('en'))

#apply dictionary to a tokens object, convert into a DFM and group by subreddit
dfm_lsd_group <- tokens_lookup(toks_clean, dictionary = data_dictionary_LSD2015) %>%
  dfm() %>%
  dfm_weight(scheme = 'prop') %>%
  dfm_group(groups = subreddit)

df_lsd <- convert(dfm_lsd_group, to = "data.frame")
sentiment <- df_lsd[,1:3]

#all other sentiments had a score of 0

#let's also implement the MFD dictionary
dfm_mfd_group <- tokens_lookup(toks_clean, dictionary = data_dictionary_MFD) %>%
  dfm() %>%
  dfm_weight(scheme = 'prop') %>%
  dfm_group(groups = subreddit)

df_mfd <- convert(dfm_mfd_group, to = "data.frame")

sentiment <- cbind(sentiment, df_mfd[,2:ncol(df_mfd)])

#get the difference between positive and negative sentiment for convenience
#of analysis
sentiment$delta <- sentiment$positive - sentiment$negative

#format the table
summary <- summary[order(summary$subreddit),]
sentiment$n <- summary$n_documents

sentiment[,2:ncol(sentiment)] <- sentiment[,2:ncol(sentiment)]/sentiment[,ncol(sentiment)]
sentiment[,2:ncol(sentiment)] <- round(sentiment[,2:ncol(sentiment)], 2)

#save the tables
write.csv(summary, "/Users/masha/Desktop/reddit/summary.csv", row.names=FALSE)
write.csv(sentiment, "/Users/masha/Desktop/reddit/sentiment.csv", row.names=FALSE)

#obtain the correlation matrix for sentiment and save it
write.csv(round(cor(sentiment[,2:(ncol(sentiment)-1)]), 2),
          "/Users/masha/Desktop/reddit/corr_sentiment.csv", 
          row.names=TRUE)

#round the numbers up to 2 decimals
summary <- summary %>% mutate_if(is.numeric, ~round(., 2))
sentiment <- sentiment %>% mutate_if(is.numeric, ~round(., 2))

#save
write.csv(summary, "/Users/masha/Desktop/reddit/summary.csv", row.names=FALSE)
write.csv(sentiment, "/Users/masha/Desktop/reddit/sentiment.csv", row.names=FALSE)

#open the "neutral" subreddit and ad it to our dataset
neutral_sub <- read.csv("/Users/masha/Desktop/reddit/answers.csv")
neutral_sub <- neutral_sub %>% drop_na()

#add the number of rows equal to the size of our initial dataset, to create
#a reasonably balanced dataset
idx <- sample(1:nrow(neutral_sub), nrow(p_init))
neutral_sub <- neutral_sub[idx,]

#preprocess
askreddit <- as.data.frame(neutral_sub$subreddit.name)
askreddit$subreddit <- askreddit$`neutral_sub$subreddit.name`
askreddit$body <- neutral_sub$body
askreddit$utc_datetime_str <- neutral_sub$created_utc
askreddit <- askreddit[,2:ncol(askreddit)]

#plot the sentiment distribution of the neutral subreddit content
ggplot(neutral_sub, aes(x = sentiment)) + 
  geom_density() + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())

p_init <- rbind(p_init, askreddit)

#save
write.csv(p_init, "/Users/masha/Desktop/reddit/p_init.csv", row.names=FALSE)

#read the file
p_init <- read.csv("/Users/masha/Desktop/reddit/p_init.csv")

corpus_reddit <- corpus(p_init, text_field = "body")

subs <- unique(docvars(corpus_reddit)$subreddit)

keyness_sub <- function(x){
  #Accepts the string containing a subreddit name, treats this subreddit as the target
  #and the others as a reference group, outputs keyness statistics and a 
  #textplot based on it
  p_init$target <- ifelse(p_init$subreddit == x, 1, 0)
  
  reddit_corp <- corpus(p_init, text_field = "body")
  
  dfm_keyness <- tokens(reddit_corp,
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE,
                        remove_symbols = TRUE,
                        remove_separators = TRUE) %>%
    tokens_remove(stopwords("en")) %>%
    dfm(tolower = TRUE) 
  
  dfm_keyness <-  dfm_group(dfm_keyness, groups = dfm_keyness@docvars$target)
  
  dfm_subset <- dfm_keyness[dfm_keyness@docvars$subreddit == x, ]
  
  print(topfeatures(dfm_subset, 30))
  
  print(textplot_keyness(textstat_keyness(dfm_keyness, target = 1L), margin = 0.5, n = 10))
}

#carry out for all subreddits
for (sub in subs){
  keyness_sub(sub)
}

#investigate unusual cases in context
View(kwic(reddit_toks, "vegan", valuetype = "fixed"))

#ideological scaling

#rank the subreddits across both axes

#left -1 right 1
left_right <- c(
  "socialism" = -2/3,
  "SocialDemocracy" = -1/3,
  "progressive" = -1/3,
  "Marxism" = -1,
  "LibertarianSocialism" = -1, #
  "Libertarian" = 2/3,
  "Liberal" = -1/3, #
  "labor" = -2/3, #
  "GreenParty" = -2/3,
  "Egalitarianism" = -2/3,
  "democrats" = -2/3,
  "Conservative" = 2/3,
  "Anarcho_Capitalism" = 1, #
  "Anarchism" = -1, #
  "askreddit" = 0
)

#authoritarian 1 liberal -1
auth_lib <- c(
  "socialism" = 1/3,
  "SocialDemocracy" = -2/3,
  "progressive" = -2/3,
  "Marxism" = -1/3,
  "LibertarianSocialism" = -1, 
  "Libertarian" = -2/3,
  "Liberal" = -2/3, 
  "labor" = -2/3, 
  "GreenParty" = -1/3,
  "Egalitarianism" = -2/3,
  "democrats" = -2/3,
  "Conservative" = 2/3,
  "Anarcho_Capitalism" = -1, 
  "Anarchism" = -1, 
  "askreddit" = 0
)

#initialize the columns to add ideological scores later
p_init$auth_lib <- 0
p_init$left_right <- 0

p_init_train <- data.frame(
  subreddit = character(),
  body = character(),
  utc_datetime_str = character(),
  auth_lib = numeric(),
  left_right = numeric()
)

#create a "training set" out of our data to estimate models
for (sub in subs){
  sub_df <- p_init[p_init$subreddit == sub,]
  
  idx <- sample(1:nrow(sub_df), round(0.2*nrow(sub_df)))
  sub_df <- sub_df[idx,]
  
  sub_df$left_right[sub_df$subreddit == sub] <- left_right[[sub]]
  sub_df$auth_lib[sub_df$subreddit == sub] <- auth_lib[[sub]]
  
  p_init_train <- rbind(p_init_train, sub_df)
}

reddit_corp <- corpus(p_init_train, text_field = "body")

reddit_dfm <- tokens(reddit_corp,
                     remove_punct = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_symbols = TRUE,
                     remove_separators = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  dfm(tolower = TRUE) 

#estimate a wordscores model for the left-right axis
wscore_left_right <- textmodel_wordscores(reddit_dfm, p_init_train$left_right, scale = "linear", smooth = 0)
summary(wscore_left_right)

#estimate a wordscores model for the authoritarian-liberal axis
wscore_auth_lib <- textmodel_wordscores(reddit_dfm, p_init_train$auth_lib, scale = "linear", smooth = 0)
summary(wscore_auth_lib)

reddit_corp <- corpus(p_init, text_field = "body")

reddit_toks <- tokens(reddit_corp,
                      remove_punct = TRUE,
                      remove_numbers = TRUE,
                      remove_url = TRUE,
                      remove_symbols = TRUE,
                      remove_separators = TRUE) %>%
  tokens_remove(stopwords("en")) 

reddit_dfm <- reddit_toks %>%
  dfm(tolower = TRUE) 

#obtain scores for the left-right scale from the estimated model
pred_lr <- predict(wscore_left_right, 
                   reddit_dfm,
                   se.fit = FALSE,
                   interval = "none",
                   level = 0.95,
                   rescaling = "none",
                   force = TRUE)

#obtain scores for the authoritarian-liberal scale from the estimated model
pred_al <- predict(wscore_auth_lib, 
                   reddit_dfm,
                   se.fit = FALSE,
                   interval = "none",
                   level = 0.95,
                   rescaling = "none",
                   force = TRUE)

#add to our dataset
p_init$auth_lib <- pred_al
p_init$left_right <- pred_lr

#obtain grouped values for the access to see if our estimations make sense
p_init_grouped <- p_init %>%
  group_by(subreddit) %>%
  summarise(across(c(left_right, auth_lib), list(mean = mean)))

p_init$auth_lib[p_init$subreddit == "askreddit"] <- 0
p_init$auth_lib[p_init$subreddit == "askreddit"] <- 0

#save
write.csv(p_init, "/Users/masha/Desktop/reddit/p_init_wordscores.csv", row.names=FALSE)
write.csv(p_init_grouped, "/Users/masha/Desktop/reddit/p_init_grouped.csv", row.names=FALSE)

#delete the [removed] observations from the dataset
p_init <- p_init[p_init$body != "[removed]",]
write.csv(p_init, "/Users/masha/Desktop/reddit/p_init_non_removed.csv")

p_init <- read.csv("/Users/masha/Desktop/reddit/p_init_non_removed.csv")

#create a balanced version of our dataset to reduce the imbalances classes issue

#create an empty list to store the number of observations for each subreddit
ls <- c()

#obtain the lengths
for (i in 1:length(subs)){
  sub_df <- df[df$subreddit == subs[i],]
  ls[i] <- nrow(sub_df)
}

min_n <- min(ls)

balanced_df <- data.frame(
  subreddit = character(),
  body = character(),
  utc_datetime_str = character(),
  auth_lib = numeric(),
  left_right = numeric()
)

for (sub in subs){
  sub_df <- df[df$subreddit == sub,]
  idx <- sample(1:nrow(sub_df), min_n)
  sub_df <- sub_df[idx,]
  balanced_df <- rbind(balanced_df, sub_df)
}

reddit_corp <- corpus(balanced_df, text_field = "body")

reddit_toks <- tokens(reddit_corp,
                      remove_punct = TRUE,
                      remove_numbers = TRUE,
                      remove_url = TRUE,
                      remove_symbols = TRUE,
                      remove_separators = TRUE) %>%
  tokens_remove(stopwords("en")) 

#trim the dfm to reduce the number of features
reddit_dfm <- reddit_toks %>%
  dfm(tolower = TRUE) %>%
  dfm_trim(min_termfreq = 100, min_docfreq = 3)

#perform the TF-IDF transformation
rdfm <- reddit_dfm %>% 
  dfm_tfidf() %>%
  convert(to = "data.frame") %>% 
  cbind(docvars(reddit_dfm))

rdfm <- rdfm[,2:ncol(rdfm)]

#save as CSV
write.csv(rdfm, "/Users/masha/Desktop/reddit/reduced_dfm_tfidf.csv")
