## create links for search results

## linki
# http://www.ceneo.pl/Ciaza_i_karmienie_piersia;0191;0020-30-0-0-1.htm
# http://www.ceneo.pl/Karmienie_dziecka;0191;0020-30-0-0-1.htm
# http://www.ceneo.pl/Podroz_i_spacer_z_dzieckiem;0191;0020-30-0-0-0.htm

ceneo_links <- function(cat = '', page = 1) {
  if (cat == 'cik') {
    base_url <-
      paste0(
        'http://www.ceneo.pl/Ciaza_i_karmienie_piersia;0191;0020-30-0-0-',
        page,
        '.htm'
      )
  }
  else if (cat == 'karm') {
    base_url <-
      paste0('http://www.ceneo.pl/Karmienie_dziecka;0191;0020-30-0-0-',
             page,
             '.htm')
  }
  else {
    base_url <-
      paste0(
        'http://www.ceneo.pl/Podroz_i_spacer_z_dzieckiem;0191;0020-30-0-0-',
        page,
        '.htm'
      )
  }
  
  return(base_url)
}

## main results
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
  if (ncol(prod_opin) == 2) {
    prod_no_votes <-   prod_opin[, 1] %>% as.numeric()
    prod_no_opin <-   prod_opin[, 2] %>% as.numeric()
  } else {
    prod_no_votes <- prod_no_opin <- prod_opin %>% as.numeric()
  }
  prod_price_min <- main_page %>% html_node('span.price') %>%
    html_text() %>% stri_replace_all_fixed(replacement = '.', pattern = ',') %>%
    as.numeric()
  prod_shops <- main_page %>% html_node('span.shop-numb') %>%
    html_text() %>% stri_extract_all_regex('\\d+', simplify = T) %>%
    as.numeric()
  prod_category <-
    main_page %>% html_node('p.cat-prod-row-category a.dotted-link') %>%
    html_text()
  
  prod_link <-
    main_page %>% html_node('strong.cat-prod-row-name a') %>%
    html_attr('href') %>%
    stri_paste('http://www.ceneo.pl', .)
  
  if (any(is.na(prod_link))) {
    
    Sys.sleep(5)
    prod_link <-
      url %>% read_html() %>% html_nodes('div.cat-prod-row-body') %>% 
      html_node('strong.cat-prod-row-name a') %>%
      html_attr('href') %>%
      stri_paste('http://www.ceneo.pl', .)
    
    prod_info <- lapply(prod_link, get_page_details)
    
  } else {
    prod_info <- lapply(prod_link, get_page_details)
  }
  
  
  df <- data_frame(
    prod_title = prod_title,
    prod_score = prod_score,
    prod_no_opin = prod_no_opin,
    prod_no_votes = prod_no_votes,
    prod_price_min = prod_price_min,
    prod_shops = prod_shops,
    prod_category = prod_category,
    prod_link = prod_link,
    prod_info = prod_info
  )
  
  return(df)
}

## detailed information

get_page_details <- function(url) {
  test_session_session <- html_session(url)
  ceneo_true <- stri_detect(test_session_session$url,
                            fixed = 'www.ceneo.pl')
  if (!ceneo_true) {
    prod_info <- list(
      prod_prices = NULL,
      prod_features = NULL,
      prod_details = NULL
    )
  }  else {
    main_page <- url %>% read_html()
    
    prod_prices <-
      main_page %>% html_nodes('span.price span.value') %>%
      html_text() %>% stri_paste(collapse = ', ')
    
    page_details <- main_page %>% html_node('li.page-tab a') %>%
      html_attr('href') %>%
      stri_paste('http://www.ceneo.pl', .) %>%
      read_html()
    
    tab1 <- page_details %>% html_nodes('section.full-specs th') %>%
      html_text() %>% stri_trim_both()
    tab2 <- page_details %>% html_nodes('section.full-specs ul') %>%
      html_text() %>% stri_trim_both()  %>%
      stri_replace_all(rep = ' ', regex = '\r\n') %>%
      stri_replace_all(rep = ', ', regex = '\\s{2,}')
    
    if (length(tab1) != 0 | length(tab2) != 0) {
      prod_details <- data.frame(feature = tab1,
                                 value = tab2)
    } else
    {
      prod_details <- data.frame(feature = NULL,
                                 value = NULL)
    }
    
    page_reviews_no <- main_page %>%
      html_nodes('ul.wrapper li.page-tab.reviews') %>%
      html_text() %>%
      stri_extract_all(regex = '\\d+') %>%
      unlist()
    
    if (!is.null(page_reviews_no)) {
      prod_features <- main_page %>%
        html_nodes('ul.wrapper li.page-tab.reviews a') %>%
        html_attr('href') %>%
        stri_paste('http://www.ceneo.pl', .) %>%
        read_html() %>%
        html_nodes('div.features-breakdown') %>%
        html_text() %>%
        stri_trim_both() %>%
        stri_replace_all(rep = '', regex = '\r\n') %>%
        stri_replace_all(rep = ';', regex = '\\s{2,}')
      
    }  else {
      prod_features <- character()
    }
    
    prod_info <- list(
      prod_prices = prod_prices,
      prod_features = prod_features,
      prod_details = prod_details
    )
  }
  
  
  return(prod_info)
}
