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

#######

head(bb)
bb_30_artists <- bb %>%
  select(artist_compressed, title) %>%
  unique() %>%
  count(artist_compressed, sort = TRUE)

bb_30_artists %>%
  slice(1:30)

tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))

bb_tagged <- bb %>%
  inner_join(tags)

bb_tagged

top_20 <- bb_count$chord[1:20]

bb_tagged %>%
  filter(chord %in% top_20) %>%
  count(chord, instrument, sort = TRUE) %>%
  ggplot(aes(chord,  n, fill = chord)) +
  geom_col() +
  facet_grid(~instrument) +
  coord_flip() +
  xlab('Total chords') +
  ylab('Chord') + 
  theme(legend.position = 'none')

top_20_bigram <- bb_bigram_count$bigram[1:20]

# The top 20 most common bigrams
top_20_bigram <- bb_bigram_count$bigram[1:20]

# Creating a faceted plot comparing guitar- and piano-driven songs for bigram frequency
bb_tagged %>%
  mutate(next_chord = lead(chord),
         next_title = lead(title),
         bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>%
  count(bigram, instrument, sort = TRUE) %>%
  filter(bigram %in% top_20_bigram) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_col() +
  facet_grid(~instrument) +
  coord_flip() +
  ylab('Total bigrams') +
  xlab('Bigram') +
  theme(legend.position="none")



