library(jsonlite)
library(tidytext)
library(datasets)
library(ggplot2)

### Which is the worst US state? ###
id_query <- "https://api.pushshift.io/reddit/submission/comment_ids/8822nf"
json_data <- fromJSON(paste(readLines(id_query), collapse=""))
json_data <- json_data$data


## Query comments
query <- "https://api.pushshift.io/reddit/comment/search?ids="

# Get a list with the right indices to query
index_sequence <- seq(1, length(json_data))
split_parts <- split(index_sequence, ceiling(seq_along(index_sequence) / 250))

# Query here
results <- lapply(split_parts, FUN = function(x) {
  comma_delim <- paste0(json_data[x], collapse = ",")
  comm_query <- paste0(query, comma_delim)
  comment_data <- fromJSON(paste(readLines(comm_query), collapse=""))$data
  return(comment_data)
})

# Get ALL comments
all_comms <- sapply(results, FUN = function(x) {
  return(x$body)
})
all_comms <- unlist(all_comms)
comms_df <- data.frame(comment = all_comms, stringsAsFactors = FALSE)
raw_comms <- as_tibble(unnest_tokens(comms_df, output = word, input = comment))

# Sort the data.frame
most_used <- raw_comms %>%
  mutate(word = tolower(word)) %>%
  count(word, sort = TRUE)

# find US states
most_used_states <- most_used %>%
  filter(word %in% tolower(state.name)) %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = unique(most_used_states$word)))

# GGplot
plot <- ggplot(most_used_states, aes(x = word, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "State name", y = "Number of Mentions",
       title = "What US state is the worst?",
       subtitle = "Most mentioned states in AskReddit threat 'What is the worst state in the US and why?'") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(most_used_states$word))) +
  scale_y_continuous(breaks = seq(0, 2800, by = 500)) +
  annotate("text", x = "hawaii", y = 3200, label = "twitter.com/stadlmann_",
           hjust=1.1, vjust=-1.1, col="black", cex=6,
           fontface = "bold", alpha = 0.3)
ggsave(plot, filename = "worst_US_state.png", height = 7, width = 7)
