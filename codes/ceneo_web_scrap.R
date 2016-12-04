### scraper for ceneo

library(rvest)
library(dplyr)
library(stringi)

source('codes/ceneo_scrap_functions.R')

# scraping ----------------------------------------------------------------

## spacer
ceneo_spacer <- list()
n_pages <- ceneo_links(page = 1) %>%
  read_html() %>% html_nodes('div.pagination ul') %>%
  html_nodes('li a') %>% html_text() %>% as.numeric() %>% max(na.rm = T)

for (i in 1:n_pages) {
  cat('strona: ', i, 'z ', n_pages, ' \n')
  ceneo_spacer[[i]] <- get_main_page_results(ceneo_links(page = i))
}

ceneo_spacer_df <- bind_rows(ceneo_spacer)
save(ceneo_spacer_df, file = 'datasets/ceneo-podroz-i-spacer.rda')

## karmienie

ceneo_karmienie <- list()
n_pages <- ceneo_links(page = i, cat = 'karm') %>%
  read_html() %>% html_nodes('div.pagination ul') %>%
  html_nodes('li a') %>% html_text() %>% as.numeric() %>% max(na.rm = T)

for (i in 1:n_pages) {
  cat('strona: ', i, 'z ', n_pages, ' \n')
  ceneo_karmienie[[i]] <-
    get_main_page_results(ceneo_links(page = i,
                                      cat = 'karm'))
}

ceneo_karmienie_df <- bind_rows(ceneo_karmienie)
save(ceneo_karmienie_df, file = 'datasets/ceneo-karmienie.rda')

## piers

ceneo_piers <- list()
n_pages <- ceneo_links(page = i, cat = 'cik') %>%
  read_html() %>% html_nodes('div.pagination ul') %>%
  html_nodes('li a') %>% html_text() %>% as.numeric() %>% max(na.rm = T)

for (i in 1:n_pages) {
  cat('strona: ', i, 'z ', n_pages, ' \n')
  ceneo_piers[[i]] <- get_main_page_results(ceneo_links(page = i,
                                                        cat = 'cik'))
}

ceneo_piers_df <- bind_rows(ceneo_piers)
save(ceneo_piers, file = 'datasets/ceneo-piers.rda')
