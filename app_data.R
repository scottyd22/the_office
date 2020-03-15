library(tidyverse)
library(rvest)

# scrape IMDB ----
get_office_imdb_rating <- function(season) {
  
  site <- paste0('https://www.imdb.com/title/tt0386676/episodes?season=', season, '&ref_=ttep_ep_sn_nx')
  
  get <- read_html(site) %>%
    html_nodes('.zero-z-index div , .ipl-rating-star.small .ipl-rating-star__rating, #episodes_content strong a, .item_description') %>%
    html_text()
  
  matrix(unlist(get), ncol = 4, byrow = T) %>% 
    data.frame() %>%
    mutate(season = str_replace(substr(X1, 1, 3), ',', ''),
           episode = str_replace(substr(X1, 5, 12), ' ', ''),
           episode_name = X2,
           description = X4,
           rating = X3) %>%
    select(-X1, -X2, -X3, -X4) %>%
    mutate(season = ifelse(nchar(season) == 2, str_replace(season, 'S', '0'), str_replace(season, 'S', ''))) %>%
    mutate(episode = ifelse(nchar(episode) == 3, str_replace(episode, 'Ep', '0'), str_replace(episode, 'Ep', ''))) %>%
    mutate(description = str_squish(description))
  
}

# store all ratings
imdb_ratings <- lapply(1:9, get_office_imdb_rating) %>%
  bind_rows %>%
  mutate(season = str_replace(season, '0', ''),
         episode = ifelse(substr(episode, 1, 1) == '0', substr(episode, 2, 2), episode))

# characters to keep ----
characters <- schrute::theoffice %>%
  mutate(character = case_when(
    character == 'Deangelo' ~ 'DeAngelo',
    character == 'David' ~ 'David Wallace',
    character %in% c('Todd', 'Packer') ~ 'Todd Packer',
    TRUE ~ character
  )) %>%
  group_by(character) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  distinct(character, n) %>%
  arrange(desc(n)) %>%
  filter(n >= 100) %>%
  pull(character)

# correct schrute episode numbering ----
df <- schrute::theoffice %>% 
  select(season, episode, episode_name) %>%
  distinct() %>% 
  group_by(season) %>% 
  mutate(Row = row_number()) %>% 
  ungroup() %>% 
  mutate(Row = ifelse(Row < 10, paste0('0', Row), paste(Row))) %>%
  select(episode_name, Row)

office <- schrute::theoffice %>%
  mutate(character = case_when(
    character == 'Deangelo' ~ 'DeAngelo',
    character == 'David' ~ 'David Wallace',
    character %in% c('Todd', 'Packer') ~ 'Todd Packer',
    TRUE ~ character
  )) %>%
  left_join(df) %>%
  mutate(episode = Row) %>%
  select(-Row) %>%
  filter(character %in% characters) %>%
  mutate(season = str_replace(season, '0', ''),
         episode = ifelse(substr(episode, 1, 1) == '0', substr(episode, 2, 2), episode))


# did_it ---- 
# only keep cases with 4 or more words
did_it <- office %>%
  filter(substr(text_w_direction, 1, 1) == '[' & 
           substr(text_w_direction, nchar(text_w_direction), nchar(text_w_direction)) == ']' &
           text == ''
           ) %>%
  rowwise() %>%
  mutate(text_w_direction = str_replace_all(str_replace_all(text_w_direction, '\\[', ''), '\\]', '')) %>%
  mutate(text_w_direction = ifelse(str_detect(word(text_w_direction, 1), paste(characters, collapse = '|')),
                                   text_w_direction,
                                   paste0(tolower(substr(text_w_direction, 1, 1)), substr(text_w_direction, 2, 1500))
                                   )
         ) %>%
  mutate(text_w_direction = str_replace(text_w_direction ,'/b', '')) %>%
  mutate(words = str_split(text_w_direction, ' ')[1]) %>%
  mutate(n = length(words)) %>%
  ungroup() %>%
  filter(n >= 4) %>%
  select(-words, -n)


# said_it ----
# only keep cases with 8 or more words
said_it <- office %>%
  rowwise() %>%
  mutate(words = str_split(text, ' ')[1]) %>%
  mutate(n = length(words)) %>%
  ungroup() %>%
  filter(n >= 8) %>%
  select(-words, -n)


# talkative ----
talkative <- office %>%
  group_by(season, episode, episode_name, character) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(season, episode, desc(n))

# gifs ----
gifs <- data.frame(
  links = c(
    # 0-3 (5 gifs)
    '<div style="width:100%;height:0;padding-bottom:86%;position:relative;"><iframe src="https://giphy.com/embed/3t7RAFhu75Wwg" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/angry-the-office-screaming-3t7RAFhu75Wwg">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:72%;position:relative;"><iframe src="https://giphy.com/embed/4cuyucPeVWbNS" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/reddit-october-brotherinlaw-4cuyucPeVWbNS">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/anuMFPLssX6SI" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/mrw-boss-anuMFPLssX6SI">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/keuNoOuTb1D4A" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/the-office-michael-scott-keuNoOuTb1D4A">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/xiTfkNBsKADmg" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/xiTfkNBsKADmg">via GIPHY</a></p>',
    # 4-7 (8 gifs)
    '<div style="width:100%;height:0;padding-bottom:52%;position:relative;"><iframe src="https://giphy.com/embed/KBfKueAjIJV8Q" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/the-office-finger-guns-television-KBfKueAjIJV8Q">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/UjCXeFnYcI2R2" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/the-office-fist-bump-UjCXeFnYcI2R2">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:58%;position:relative;"><iframe src="https://giphy.com/embed/uIu5b0YYpTPR6" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/the-office-high-five-love-uIu5b0YYpTPR6">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:40%;position:relative;"><iframe src="https://giphy.com/embed/aDsjWMWS8PTag" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/the-office-steve-carell-michael-scott-aDsjWMWS8PTag">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:61%;position:relative;"><iframe src="https://giphy.com/embed/VS1OZuOgiuR68" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/the-office-mindy-kaling-bj-novak-VS1OZuOgiuR68">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/nGzeO4uSxRUcg" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/happy-excited-the-office-nGzeO4uSxRUcg">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:64%;position:relative;"><iframe src="https://giphy.com/embed/4KU4lNElpi6t2" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/thank-you-following-yes-gif-4KU4lNElpi6t2">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/2hoC9UkqBb60M" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/reaction-the-office-michael-scott-2hoC9UkqBb60M">via GIPHY</a></p>',
    # 8-10 (5 gifs)
    '<div style="width:100%;height:0;padding-bottom:49%;position:relative;"><iframe src="https://giphy.com/embed/mp1JYId8n0t3y" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/story-winning-naral-mp1JYId8n0t3y">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/5YTFe5djWgq0o" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/excited-the-office-5YTFe5djWgq0o">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/uMPjuulT3rpRe" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/uMPjuulT3rpRe">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/3E0IBAKbrEuFOYPd09" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/the-office-dunder-mifflin-3E0IBAKbrEuFOYPd09">via GIPHY</a></p>',
    '<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/AatW9hUhdcX5e" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/television-dance-the-office-AatW9hUhdcX5e">via GIPHY</a></p>'
  ),
  
  score = c(rep(3, 5),
            rep(7, 8),
            rep(10, 5)
            ),
  
  stringsAsFactors = F
)


# save ----
# save(imdb_ratings, said_it, did_it, lines_by_episode, file = 'D:/Google Drive/10 shiny/theoffice/app_data.RData')
save(imdb_ratings, said_it, did_it, talkative, gifs, file = 'H:/Desktop/New folder/office/app_data.RData')
