---
title: "Text Analytics Project"
author: "Christian Kevin Kusuma"
date: "7/30/2020"
output: pdf_document
---
# Libraries
```{r, warning=FALSE, message=FALSE}
install.packages("quanteda.dictionaries")
library(tidyverse)
library(quanteda)
library(caret)
library(tidytext)
library(quanteda.dictionaries)
library(topicmodels)
library(doSNOW)
library(irlba)
library(wordVectors)
```

# Data
## Importing the data
```{r, warning=FALSE, message=FALSE}
data <- read_csv("pcc_reviews.csv")
```

## Breaking the data by activities
```{r, warning=FALSE, message=FALSE}
# Only Islands
data_islands <- data %>% 
  select(responseid, Year, islands.com)

# Only Guide
data_guide <- data %>% 
  select(responseid, Year, guide.com)

# Only HA Show
data_ha <- data %>%  
  select(responseid, Year, ha.com)

# Only Pageant
data_pageant <- data %>% 
  select(responseid, Year, pageant.com)

# Only Dinner
data_dinner <- data %>% 
  select(responseid, Year, dinner.com)

# All Comment
data_all <- data %>% 
  mutate(will_recommend = factor(ifelse(recommend == 10, 'Yes','No')), # mapping the recommend labels, only "Yes" when it's 10.
         com_island = factor(ifelse(is.na(islands.com) == T, 0, 1)),
         com_ha = factor(ifelse(is.na(ha.com) == T, 0, 1)),
         com_pageant = factor(ifelse(is.na(pageant.com) == T, 0, 1)),
         com_guide = factor(ifelse(is.na(guide.com) == T, 0, 1)),
         com_dinner = factor(ifelse(is.na(dinner.com) == T, 0, 1))
         ) %>% 
  select(responseid, 
         Year, 
         'All Comment', 
         will_recommend, 
         com_island, 
         com_island,
         com_ha,
         com_pageant,
         com_guide,
         com_dinner,
         r.islands,
         r.ha,
         r.guide,
         o.sat)
```

## Creating corpuses by activities
```{r, warning=FALSE, message=FALSE}
corpus_islands <- corpus(data_islands, text_field = 'islands.com')
corpus_guide <- corpus(data_guide, text_field = 'guide.com')
corpus_ha <- corpus(data_ha, text_field = 'ha.com')
corpus_pageant <- corpus(data_pageant, text_field = 'pageant.com')
corpus_dinner <- corpus(data_dinner, text_field = 'dinner.com')
corpus_all <- corpus(data_all, text_field = 'All Comment')
```

# Sentiment Analysis and Tagging
## Tagging with NRC
```{r, warning=FALSE, message=FALSE}
islands_tags <- liwcalike(corpus_islands, dictionary= data_dictionary_NRC, tolower=T)
guide_tags <- liwcalike(corpus_guide, dictionary= data_dictionary_NRC, tolower=T)
ha_tags <- liwcalike(corpus_ha, dictionary= data_dictionary_NRC, tolower=T)
pageant_tags <- liwcalike(corpus_pageant, dictionary= data_dictionary_NRC, tolower=T)
dinner_tags <- liwcalike(corpus_dinner, dictionary= data_dictionary_NRC, tolower=T)
all_tags <- liwcalike(corpus_all, dictionary = data_dictionary_NRC, tolower=T)
```

## Joining the Tags with the datasets by activities
```{r, warning=FALSE, message=FALSE}
data_islands$positive <- islands_tags$positive
data_islands$negative <- islands_tags$negative

data_guide$positive <- guide_tags$positive
data_guide$negative <- guide_tags$negative

data_ha$positive <- ha_tags$positive
data_ha$negative <- ha_tags$negative

data_pageant$positive <- pageant_tags$positive
data_pageant$negative <- pageant_tags$negative

data_dinner$positive <- dinner_tags$positive
data_dinner$negative <- dinner_tags$negative

data_all$positive <- all_tags$positive
data_all$negative <- all_tags$negative
```

## Categorizing the sentiments
```{r, warning=FALSE, message=FALSE}

data_islands <- data_islands %>% 
 mutate(sentiment = ifelse(positive-negative>0, 'Islands Positive','Islands Negative'))

data_guide <- data_guide %>% 
 mutate(sentiment = ifelse(positive-negative>0, 'Guide Positive','Guide Negative'))

data_ha <- data_ha %>% 
 mutate(sentiment = ifelse(positive-negative>0, 'HA Positive','HA Negative'))

data_pageant <- data_pageant %>% 
 mutate(sentiment = ifelse(positive-negative>0, 'Pageant Positive','Pageant Negative'))

data_dinner <- data_dinner %>% 
 mutate(sentiment = ifelse(positive-negative>0, 'Dinner Positive','Dinner Negative'))

data_all <- data_all %>% 
  mutate(sentiment = ifelse(positive-negative>0, 'Overall Positive','Overall Negative'))
```


# Word Cloud
## Word Cloud Data Preparation
```{r, warning=FALSE, message=FALSE}
# Creating Corpuses and DFM's
islands_cloud <- corpus(data_islands, text_field = 'islands.com') %>% 
  dfm(remove = stopwords('English'), remove_punct = TRUE, groups = 'sentiment') %>% 
  dfm_trim(min_termfreq = 3)

guide_cloud <- corpus(data_guide, text_field = 'guide.com') %>% 
  dfm(remove = stopwords('English'), remove_punct = TRUE, groups = 'sentiment') %>% 
  dfm_trim(min_termfreq = 3)

pageant_cloud <- corpus(data_pageant, text_field = 'pageant.com') %>% 
  dfm(remove = stopwords('English'), remove_punct = TRUE, groups = 'sentiment') %>% 
  dfm_trim(min_termfreq = 3)

ha_cloud <- corpus(data_ha, text_field = 'ha.com') %>% 
  dfm(remove = stopwords('English'), remove_punct = TRUE, groups = 'sentiment') %>% 
  dfm_trim(min_termfreq = 3)

dinner_cloud <- corpus(data_dinner, text_field = 'dinner.com') %>% 
  dfm(remove = stopwords('English'), remove_punct = TRUE, groups = 'sentiment') %>% 
  dfm_trim(min_termfreq = 3)

all_cloud <- corpus(data_all, text_field = 'All Comment') %>% 
  dfm(remove = stopwords('English'), remove_punct = TRUE, groups = 'sentiment') %>% 
  dfm_trim(min_termfreq = 3)
```

## Word Cloud Visualizations
```{r, warning=FALSE, message=FALSE}
# Single Word Cloud - Island
textplot_wordcloud(islands_cloud, comparison = TRUE, max_words = 300,min_size = 0.5,
                   max_size = 4, min_count = 3, color = c("red", "blue"))

# Single Word Cloud - Guide
textplot_wordcloud(guide_cloud, comparison = TRUE, max_words = 300,min_size = 0.5,
                   max_size = 4, min_count = 3, color = c("red", "blue"))

# Single Word Cloud - HA Show
textplot_wordcloud(ha_cloud, comparison = TRUE, max_words = 300,min_size = 0.5,
                   max_size = 4, min_count = 3, color = c("red", "blue"))

# Single Word Cloud - Pageant
textplot_wordcloud(pageant_cloud, comparison = TRUE, max_words = 300,min_size = 0.5,
                   max_size = 4, min_count = 3, color = c("red", "blue"))

# Single Word Cloud - Dinner
textplot_wordcloud(dinner_cloud, comparison = TRUE, max_words = 300,min_size = 0.5,
                   max_size = 4, min_count = 3, color = c("red", "blue"))

# Single Word Cloud - Overall
textplot_wordcloud(all_cloud, comparison = TRUE, max_words = 300,min_size = 0.5,
                   max_size = 4, min_count = 3, color = c("red", "blue"))

# Bi-gram Word Cloud
all_cloud_bigram <- tokens(data_all$`All Comment`, what = 'word',
                         remove_numbers = T, remove_punct = T,
                         remove_symbols = T, split_hyphens = T) %>%
  tokens_remove(stopwords('English')) %>% 
  tokens_wordstem() %>% 
  tokens_tolower() %>% 
  tokens_ngrams(n=2) %>%
  dfm()

textplot_wordcloud(all_cloud_bigram, min_size = 0.5,
max_size = 2,
min_count = 3,
max_words = 200,
color =  "blue",
font = NULL,
adjust = 0,
rotation = 0.1,
random_order = FALSE,
random_color = FALSE,
ordered_color = FALSE,
labelcolor = "gray20",
labelsize = 1.5,
labeloffset = 0,
fixed_aspect = TRUE,
comparison = F)
```

# Word Embeddings
## Getting the Text File Data
```{r, message=FALSE, warning=FALSE}
# Getting the All Comment Data
word_embedding_data <- data_all %>% 
  select(`All Comment`)

# Creating Text File to Feed to the Word-embedding Model
write.table(word_embedding_data, file = "pcc_review_file.txt", sep = "\t",
            row.names = FALSE)
```

## Preparing the Data and Training the Model
```{r, warning=FALSE, message=FALSE}
# Preparing the Text File to Feed to the Word-embedding Model
prep_word2vec(origin="pcc_review_file.txt",destination="pcc_review_file_ready.txt",
lowercase=T,bundle_ngrams=2)

# Training the Word-embedding Model
model = train_word2vec("pcc_review_file_ready.txt",
                       "pcc_review_file_ready_bin.bin", 
                       vectors=200,
                       threads=4,
                       window=12,
                       iter=5,
                       negative_samples=0,
                       force = T)
```

## Closest Words to the Most Frequent Words by Activities
```{r, warning=FALSE, message=FALSE}
# The Closest Words of the Most Common Words in Islands 
model %>% 
  closest_to(~"time"+"luau")

# The Closest Words of the Most Common Words in Guide 
model %>% 
  closest_to(~"group"+"smaller")

# The Closest Words of the Most Common Words in HA 
model %>% 
  closest_to(~"long"+"shorten"+"shorter")

# The Closest Words of the Most Common Words in Pageant 
model %>% 
  closest_to(~"seating"+"shade"+"better")

# The Closest Words of the Most Common Words in Dinner
model %>% 
  closest_to("buffet")
```

# Creating Prediction On Which Customers Will Most Likely Recommend
## Preparing the Training and Testing Datasets
```{r, warning=FALSE, message=FALSE}
# Tokenizing the Dataset and Creating DFM
tokenized_data <- 
  tokens(data_all$`All Comment`, what = "word",
         remove_numbers = TRUE, remove_punct = TRUE, 
         remove_symbols = TRUE, split_hyphens = TRUE) %>%
  tokens_remove(stopwords(language = 'English')) %>% 
  tokens_tolower() %>% 
  tokens_wordstem() %>%
  dfm() %>%
  dfm_tfidf() %>%
  dfm_trim( min_termfreq = 50, min_docfreq = 200)

# Converting the DFM to DataFrame
data_frame <- data.frame(tokenized_data)

# Joining Tokenized DFM DataFrame with the Target Variable and Sentiment Analysis Variables
joined_data <- cbind(Label = data_all$will_recommend, 
                     data_all$com_island,
                     data_all$com_ha,
                     data_all$com_pageant,
                     data_all$com_guide,
                     data_all$com_dinner,
                     data_islands$sentiment,
                     data_ha$sentiment,
                     data_guide$sentiment,
                     data_all$r.islands,
                     data_all$r.ha,
                     data_all$r.guide,
                     data_all$o.sat,
                     data_frame)

joined_data <- joined_data %>% 
  select(-doc_id)

# Creating DummyVariables For Categorical Variables
dummy_data <- dummyVars(Label ~., joined_data, fullRank = T) %>% 
  predict(newdata=joined_data)

ready_data <- data.frame(dummy_data)
ready_data$Label <- data_all$will_recommend
ready_data <- ready_data %>% 
  na.omit()
# Dividing the Dataset into training and testing sets
set.seed(212)
partition <- createDataPartition(ready_data$Label, p=0.7, list=F)

train_all <- ready_data[partition, ]
test_all <- ready_data[-partition, ]

# Checking the Proportions
table(train_all$Label) %>% prop.table()
table(test_all$Label) %>% prop.table()
```

## Training A Random Forest Model
```{r, warning=FALSE, message=FALSE}
# Running Random Forest from Ranger Package instead of from RF package For Model Efficiency
rf_tuning <- data.frame(mtry = 12,
                        splitrule = "extratrees",
                        min.node.size = 10)

set.seed(212)
ranfor_model <- train(Label~.,
                      train_all,
                      method = 'ranger',
                      importance = 'permutation',
                      trControl = trainControl(
                        method = 'cv',
                        number = 10),
                      tuneGrid = rf_tuning,
                   preProcess = c("center","scale"))

ranfor_model
```

## Measuring Model's Performance with the Testing Dataset
```{r, warning=FALSE, message=FALSE}
prediction <- predict(ranfor_model, test_all)
confusionMatrix(prediction, test_all$Label)
```
