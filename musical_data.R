library(readr)
library(ggplot2)
library(dplyr)

bb <- read_csv(file.choose())

head(bb)

bb_count <- bb %>%
  count(chord, wt = NULL, sort = TRUE)

head(bb_count, n = 20)
# you can also use bb_count[1:20,]

bb_count %>% 
  slice(1:20) %>%
  mutate(share = n/sum(n),
         chord = reorder(chord, share)) %>%
  ggplot(aes(chord, share, fill = chord)) +
  geom_col() +
  coord_flip() +
  ylab('Share of total chords') +
  xlab('chord')

bb_bigram_count <- bb %>% 
  mutate(next_chord = lead(chord),
         next_title = lead(title),
         bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>%
  count(bigram, sort = TRUE)

bb_bigram_count[1:20,]

bb_bigram_count %>% 
  slice(1:20) %>%
  mutate(share = n/sum(n),
         bigram = reorder(bigram, share)) %>%
  ggplot(aes(bigram, share, fill = bigram)) +
  geom_col() +
  coord_flip() +
  ylab('Share of total chords') +
  xlab('chord changes')








