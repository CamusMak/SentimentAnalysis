pacman::p_load(rvest, stringr, base64enc, tidyverse, tidytext, textdata, httr)

news <- function(term) {
  
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-US&gl=US&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    
    Time = html_dat %>%
      html_nodes(".SVJrMe")%>% 
      html_text(),
    Link = dat$Link
  )
  
  return(news_dat)
}


AI<-news("AI")
AI

url_list <- as.character(AI$Link)

decode_urls <- function(urls) {
  extracted_urls <- list()
  
  for (url in urls) {
    #Using regex to remove unnecessery parts
    encoded_part <- sub(".\\/articles\\/(.?)\\?.*", "\\1", url)
    #"CBMiIWh0dHBzOi8vaGV0cS5hbS9lbi9hcnRpY2xlLzE1NjEyMtIBAA"
    
    decoded_part <- base64decode(encoded_part)
    #Binary format
    # [1] 08 13 22 21 68 74 74 70 73 3a 2f 2f 68 65 74 71 2e 61 6d 2f 65 6e 2f 61 72 74 69
    #[28] 63 6c 65 2f 31 35 36 31 32 32 d2 01 00
    
    decoded_part_no_null <- decoded_part[decoded_part != as.raw(0)]
    decoded_string <- rawToChar(decoded_part_no_null)
    
    #"\b\023\"!https://hetq.am/en/article/156122\xd2\001"
    
    result <- str_replace(decoded_string, '.?(http.)', '\\1')
    #"https://hetq.am/en/article/156122�\001"
    
    new_link <- strsplit(result, "�", fixed = TRUE)[[1]][1]
    # https://hetq.am/en/article/156122
    
    final_link <- sub("\\$$", "", new_link)
    extracted_urls <- c(extracted_urls, final_link)
    
  }
  
  return(extracted_urls)
}

url_list1<-unlist(decode_urls(url_list))

results <- data.frame(index = numeric(length(url_list)),
                      output = character(length(url_list)),
                      num_words = numeric(length(url_list)),
                      stringsAsFactors = FALSE)

for (i in seq_along(url_list1)) {
  url <- url_list1[i]
  
  # Try to get the web page with GET function
  response <- tryCatch(
    {
      GET(url)
    },
    error = function(e) {
      if (grepl("schannel: failed to receive handshake", e$message) || grepl("Service Unavailable \\(HTTP 503\\)", e$message)) {
        print(paste("Skipping URL due to error:", url))
        return(NULL)
      } else {
        stop(e)
      }
    }
  )
  
  # If response is NULL, skip to next iteration
  if (is.null(response)) next
  
  # Check the status code of the response
  status <- status_code(response)
  
  # If the status code is not 403, proceed with scraping
  if (status != 403) {
    page <- read_html(response)
    p_tags <- page %>%
      html_nodes("p") %>%
      html_text()
    article <- paste(p_tags, collapse = " ")
    
    # Count the number of words in the article
    num_words <- length(strsplit(article, " ")[[1]])
    
    # Store the results in the data frame
    results[i, "index"] <- i
    results[i, "output"] <- article
    results[i, "num_words"] <- num_words
    
    # If the status code is 403, skip the URL and print a message
  } else {
    print(paste("Access denied for URL:", url))
    next
  }
}

results <- results %>% filter(num_words != 0)

write_csv(resuts,'AIdata.csv')

