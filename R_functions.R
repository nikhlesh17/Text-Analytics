
## Tokenizer - takes care of unigrams and bigrams
BigramTokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 1, max = 2))
}

## Text clean up functions
clean_dtm_bigram <- function(corpus){
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  dt <- DocumentTermMatrix(corpus,
                                   control = list(tokenize = BigramTokenizer,
                                                  removePunctuation = TRUE,
                                                  tolower = TRUE,
                                                  removeNumbers = TRUE,
                                                  stripWhitespace = TRUE,
                                                  stemming = TRUE
                                                  ))
  dt1 <- tidy(dt)
  return(dt1)
}

# POS tagging
nltk = import("nltk")
pos_tagging <- function(dtm){
  temp <- nltk$pos_tag(dtm$term)
  x = data.frame(dtm,pos_tag = sapply(temp, `[[`, 2))
  return(x)
}

## Function to identify new words as compared to previous years
new_words <- function(yrval){
  if (yrval == 2006){
    t <- pos2006b %>% anti_join(pos2005b,by = "term")
    return(t) 
  }
  else if (yrval == 2007){
    t <- pos2007b %>%
              anti_join(pos2006b,by = "term") %>%
              anti_join(pos2005b,by = "term")
    return(t) 
  }
  else if (yrval == 2008){
    t <- pos2008b %>%
     anti_join(pos2007b,by = "term") %>%
     anti_join(pos2006b,by = "term") %>%
     anti_join(pos2005b,by = "term")
    return(t) 
  }
  else if (yrval == 2009){
    t <- pos2009b %>% 
     anti_join(pos2008b,by = "term") %>%
     anti_join(pos2007b,by = "term") %>%
     anti_join(pos2006b,by = "term") %>%
     anti_join(pos2005b,by = "term")
    return(t) 
  }
  else if (yrval == 2010){
    t <- pos2010b %>%
     anti_join(pos2009b,by = "term") %>%
     anti_join(pos2008b,by = "term") %>%
     anti_join(pos2007b,by = "term") %>%
     anti_join(pos2006b,by = "term") %>%
     anti_join(pos2005b,by = "term")
    return(t) 
  }
  else if (yrval == 2011){
    t <- pos2011b %>%
     anti_join(pos2010b,by = "term") %>%
     anti_join(pos2009b,by = "term") %>%
     anti_join(pos2008b,by = "term") %>%
     anti_join(pos2007b,by = "term") %>%
     anti_join(pos2006b,by = "term") %>%
     anti_join(pos2005b,by = "term")    
    return(t) 
  }
  else if (yrval == 2012){
    t <- pos2012b %>%
     anti_join(pos2011b,by = "term") %>%
     anti_join(pos2010b,by = "term") %>%
     anti_join(pos2009b,by = "term") %>%
     anti_join(pos2008b,by = "term") %>%
     anti_join(pos2007b,by = "term") %>%
     anti_join(pos2006b,by = "term") %>%
     anti_join(pos2005b,by = "term")
    return(t) 
  }
  else if (yrval == 2013){
    t <- pos2013b %>%
     anti_join(pos2012b,by = "term") %>%
     anti_join(pos2011b,by = "term") %>%
     anti_join(pos2010b,by = "term") %>%
     anti_join(pos2009b,by = "term") %>%
     anti_join(pos2008b,by = "term") %>%
     anti_join(pos2007b,by = "term") %>%
     anti_join(pos2006b,by = "term") %>%
     anti_join(pos2005b,by = "term")
    return(t) 
  }
  else if (yrval == 2014){
    t <- pos2014b %>% 
     anti_join(pos2013b,by = "term") %>%
     anti_join(pos2012b,by = "term") %>%
     anti_join(pos2011b,by = "term") %>%
     anti_join(pos2010b,by = "term") %>% 
     anti_join(pos2009b,by = "term") %>%
     anti_join(pos2008b,by = "term") %>%
     anti_join(pos2007b,by = "term") %>%
     anti_join(pos2006b,by = "term") %>%
     anti_join(pos2005b,by = "term")
    return(t) 
  }
  
}
