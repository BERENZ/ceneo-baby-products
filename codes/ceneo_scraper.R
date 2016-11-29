### scraper for ceneo

library(rvest)
library(dplyr)
library(rdom)
library(stringi)

base_url <-
  paste0(
    'http://www.ceneo.pl/Wozki_dzieciece_i_akcesoria;0191;0020-30-0-0-',
    2,
    ';0115-1.htm'
  )

get_main_page_results <- function(url) {
  main_page <-
    url %>% read_html() %>% html_nodes('div.cat-prod-row-body')
  prod_title <-
    main_page %>% html_node('strong.cat-prod-row-name') %>%
    html_text() %>% stri_trim_both()
  prod_score <-
    main_page %>% html_node('span.product-score ') %>% html_text() %>%
    stri_replace_all_regex(replacement = '', pattern = '\r|\n|\\s+') %>%
    stri_replace_all_regex(replacement = '', pattern = '/5$') %>%
    stri_replace_all_fixed(replacement = '.', pattern = ',') %>% as.numeric()
  prod_opin <- main_page %>% html_node('a.product-reviews-link') %>%
    html_text() %>% stri_extract_all_regex('\\d+', simplify = T)
  prod_no_votes <-   prod_opin[, 1] %>% as.numeric()
  prod_no_opin <-   prod_opin[, 2] %>% as.numeric()
  prod_price_min <- main_page %>% html_node('span.price') %>%
    html_text() %>% stri_replace_all_fixed(replacement = '.', pattern = ',') %>%
    as.numeric()
  prod_shops <- main_page %>% html_node('span.shop-numb') %>%
    html_text() %>% stri_extract_all_regex('\\d+', simplify = T) %>%
    as.numeric()
  prod_category <-
    main_page %>% html_node('p.cat-prod-row-category a.dotted-link') %>% html_text()
  prod_link <-
    main_page %>% html_node('strong.cat-prod-row-name a') %>%
    html_attr('href')
  
  df <- data.frame(
    prod_title = prod_title,
    prod_score = prod_score,
    prod_no_opin = prod_no_opin,
    prod_no_votes = prod_no_votes,
    prod_price_min = prod_price_min,
    prod_shops = prod_shops,
    prod_category = prod_category,
    prod_link = prod_link
  )
  return(df)
}


res <- get_main_page_results(base_url)

