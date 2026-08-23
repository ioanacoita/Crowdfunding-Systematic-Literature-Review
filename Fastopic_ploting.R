

library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidytext) 

# load data (output from Fastopic)
# --- # --- # --- # --- # --- # --- # --- # --- #
Crowdfunding_All <- read_csv("out/df_out_all.csv")
doc_topic_dist <- as.data.frame(read_csv("out/doc_topic_annotated.csv"))
doc_topic_dist$...1 <- NULL ; colnames(doc_topic_dist) <- c("topic_0","topic_1","topic_2","topic_3","topic_4","topic_5","topic_6",
                                                            "topic_7","topic_8","topic_9","top_1_topic","title")
doc_topic_dist$Year <- Crowdfunding_All$Year[match(tolower(doc_topic_dist$title),tolower(Crowdfunding_All$Title))]
topic_top_words <- as.data.frame(read_csv("out/topic_top_words.csv"))
# --- # --- # --- # --- # --- # --- # --- # --- #


# structure data
# --- # --- # --- # --- # --- # --- # --- # --- #
data_long <- doc_topic_dist %>% gather(key = "topic", value = "value", topic_0:topic_9)  # Gather topic columns into long format
data_long$Year <- Crowdfunding_All$Year[match(tolower(data_long$title),tolower(Crowdfunding_All$Title))]
# --- # --- # --- # --- # --- # --- # --- # --- #


# Calculate and plot median enrichment value for each topic across years and by publication type
# --- # --- # --- # --- # --- # --- # --- # --- #

# topic enrichments depending on year?
topic_mediam_year <- data_long %>% group_by(Year, topic) %>% summarise(median_value = median(value, na.rm = TRUE))  
# Visualize evoluation of topics per year
ggplot(topic_mediam_year, aes(x = Year, y = median_value, color = topic, group = topic)) + geom_smooth(se=F) +  
  labs(title = "Median Value of Topics by Year", x = "Year", y = "Median Topic Value") + theme_minimal()  
#ggsave("Fastopic/Fastopic_final_10_100/pdf_R/topic_alignment_evolution_loess.pdf",useDingbats=FALSE)

# topic enrichments by year and publication type?
data_long$doctype <- Crowdfunding_All$`Document Type`[match(tolower(data_long$title),tolower(Crowdfunding_All$Title))]
topic_mediam_year_pubtype <- data_long %>% group_by(Year ,doctype, topic) %>% summarise(median_value = median(value, na.rm = TRUE))  
# Visualize if there are publication type enrichments
ggplot(topic_mediam_year_pubtype, aes(x = Year, y = median_value, color = topic, group = topic)) + geom_smooth(se=F) +  
  labs(title = "Median Value of Topics by Year", x = "Year", y = "Median Topic Value") + facet_wrap(~doctype) + theme_minimal() 

ggplot(topic_mediam_year_pubtype, aes(x = Year, y = doctype, fill = median_value)) + geom_tile() + 
  facet_wrap(~topic) + theme_classic() + scale_fill_gradient(low="white",high="black")
#ggsave("Fastopic/Fastopic_final_10_100/pdf_R/heatmap_topic_alignment_evolution_publicationtype.pdf",useDingbats=FALSE)
# --- # --- # --- # --- # --- # --- # --- # --- #


# find 3 top topics per paper and annotate
  # SLR step
# --- # --- # --- # --- # --- # --- # --- # --- #

get_top_topics <- function(row) {
  sorted_indices <- order(row, decreasing = TRUE)
  top_topics <- names(row)[sorted_indices][1:3]
  return(top_topics)
}
# Step 2: Apply the function to each row and extract the top 3 topic names
top_topics <- t(apply(doc_topic_dist[,1:10], 1, get_top_topics))
doc_topic_dist$top_1 <- top_topics[,1]
doc_topic_dist$top_2 <- top_topics[,2]
doc_topic_dist$top_3 <- top_topics[,3]

# topic representation in sample?
table(doc_topic_dist$top_1)
topic_repr <- as.data.frame(table(doc_topic_dist$top_1,doc_topic_dist$top_2))
ggplot(topic_repr) + geom_tile(aes(Var1,Var2,fill=Freq)) + theme_classic() + scale_fill_gradient(low="white",high="black")
#ggsave("Fastopic/Fastopic_final_10_100/pdf_R/heatmap_topic_representation_first_second_toptopics.pdf",useDingbats=FALSE)

# --- # --- # --- # --- # --- # --- # --- # --- #

# weight distribution per topic for top1 assigment?
# --- # --- # --- # --- # --- # --- # --- # --- #
doc_topic_dist_melt <- reshape2::melt(doc_topic_dist[, ! colnames(doc_topic_dist) %in% c("top_1_topic","Year")])
ggplot(doc_topic_dist_melt) + geom_histogram(aes(value)) + facet_wrap(~top_1) + theme_bw()
#ggsave("Fastopic/Fastopic_final_10_100/pdf_R/histogram_topic_weights_first_toptopics.pdf",useDingbats=FALSE)
# --- # --- # --- # --- # --- # --- # --- # --- #

# explainability layer through TFIDF
# --- # --- # --- # --- # --- # --- # --- # --- #

tbl <- tibble(ID=Crowdfunding_All$Title,
              text=tolower(as.character(paste(Crowdfunding_All$Title,Crowdfunding_All$`Index Keywords`,
                                              Crowdfunding_All$`Author Keywords`,Crowdfunding_All$Abstract))))

tbl_words <- tbl %>% 
  unnest_tokens(output = "word",input = text, token = "words") %>% 
  anti_join(stop_words) %>% 
  group_by(ID) %>%
  dplyr::count(word,sort = T) 





#write_csv(doc_topic_dist,"Fastopic/Fastopic_5_1000_allenai/doc_topic_dist_annotated.csv")
# --- # --- # --- # --- # --- # --- # --- # --- #













