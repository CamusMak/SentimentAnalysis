library(rvest)
library(stringr)
library(base64enc)
library(tidyverse)
library(tidytext)
library(textdata)
library(httr)
library(sentimentr)
library(transforEmotion)

results <- read.csv("AIdata.csv")

tokens_arm <- results %>%
  unnest_tokens(word, output)

p1<-tokens_arm %>%
  anti_join(stop_words, by = "word") %>%
  count(word, index, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(n * value)) %>%
  slice_max(abs(contribution), n = 12) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word)) +
  geom_col() +
  labs(x = "Frequency of word * AFINN value", y = NULL)+
  ggtitle("Armenia")

arm_tf_idf <- tokens_arm %>%
  anti_join(stop_words, by = "word") %>%
  count(index, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, index, n)

arm_tf_idf.m<- arm_tf_idf %>% 
  group_by(index)%>%
  summarise(tf.m=mean(tf), idf.m=mean(idf), tf_idf.m=mean(tf_idf))

sentiment_score_ai<-sentiment(results$output)


bert<-transformer_scores(
  text = count_ai_words$word,
  classes = c(
    "Contains fear", "excitement"
  )
)

bert_df <- data.frame(bert)
bert_df_t <- t(bert_df)
write.csv(bert_df_t, "Bert.csv", row.names = F)


bert_text<-transformer_scores(
  text = results$output,
  classes = c(
    "Negative, Neutral, Positive"
  )
)