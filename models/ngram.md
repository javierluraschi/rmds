R Code - N-Grams
================

This R notebook makes use of N-Grams and the `ngram` package to model
the R code dataset from this repo.

We will first load the R code dataset,

``` r
library(magrittr)

code <- pins::pin_get("rmds", "https://raw.githubusercontent.com/javierluraschi/rmds/datasets/")
```

To reduce computation resources, start with a subset but skip this line
otherwise,

``` r
code <- code[1:10,]
```

First lets split between test and
validation,

``` r
index <- sample(1:2, size = nrow(code), replace = TRUE, prob = c(.8, .2))
train <- code[index == 1,]
test <- code[index == 2,]
```

We will then manually tokenize the code in a format usable by the
`ngram` package,

``` r
code_tokenize <- function(code) {
  code <- paste0(" <sof> ", paste(code, collapse = " <eof> "), " <eof> ")
  code <- gsub("\n", " <eol> ", code, fixed = TRUE)
  code <- gsub("\t", " ", code, fixed = TRUE)
  code <- gsub(",", " , ", code, fixed = TRUE)
  code <- gsub("(", " ( ", code, fixed = TRUE)
  code <- gsub(")", " ) ", code, fixed = TRUE)
  code <- gsub("[", " [ ", code, fixed = TRUE)
  code <- gsub("]", " ] ", code, fixed = TRUE)
  code <- gsub("$", " $ ", code, fixed = TRUE)
  code <- gsub("::", " :: ", code, fixed = TRUE)
  gsub("=", " = ", code, fixed = TRUE)
}

train_code <- code_tokenize(train$code)
test_code <- code_tokenize(test$code)
```

To validate this predicts properly, we used the following
check,

``` r
# train_code <- test_code <- "a b c d e f g h i j k l m n o p q r s t u v w x y z"
```

We build the 3-Gram,

``` r
ngram_size <- 3
ngram_model <- ngram::ngram(train_code, n = ngram_size, sep = " ") %>% print()
```

    ## An ngram object with 1099 3-grams

And retrieve the phrase
table,

``` r
ngram_table <- ngram::get.phrasetable(ngram_model) %>% tibble::as_tibble() %>% print()
```

    ## # A tibble: 1,099 x 3
    ##    ngrams                  freq    prop
    ##    <chr>                  <int>   <dbl>
    ##  1 ") <eol> <eol> "          77 0.0318 
    ##  2 "<eol> <eol> # "          26 0.0107 
    ##  3 ") ) <eol> "              24 0.00992
    ##  4 "<- tf $ "                21 0.00868
    ##  5 "<eol> library ( "        20 0.00826
    ##  6 "<eol> <eol> library "    19 0.00785
    ##  7 "( ) <eol> "              14 0.00579
    ##  8 "= c ( "                  12 0.00496
    ##  9 "<eol> layer_dense ( "    11 0.00455
    ## 10 "( units = "              11 0.00455
    ## # … with 1,089 more rows

Then we validate against the test dataset,

``` r
test_tokens <- strsplit(test_code, " ")[[1]]
test_tokens <- test_tokens[test_tokens != ""]

match_count <- 0
checks_count <- length(test_tokens)-ngram_size

token_incorrect <- c()
token_correct <- c()

for (i in 1:checks_count) {
  current_ngram <- test_tokens[i:(i+ngram_size-2)]
  current_text <- paste(current_ngram[1:ngram_size-1], collapse = " ")
  
  matches <- stringr::str_starts(ngram_table$ngrams, stringr::fixed(current_text))
  best <- ngram_table[matches,]$ngrams
  best <- if (length(best) == 0) "<na>" else best[[1]]
  
  correct_match <- paste0(current_text, " ", test_tokens[i+ngram_size-1], " ")
  if (best == correct_match) {
    match_count <- match_count + 1
    
    token_correct <- c(token_correct, best)
  }
  else {
    token_incorrect <- c(token_incorrect, best)
  }
}

message("Correct: ", match_count, "\n",
        "Incorrect: ", checks_count, "\n",
        "Accuracy: ", match_count / checks_count)
```

    ## Correct: 60
    ## Incorrect: 285
    ## Accuracy: 0.210526315789474

A few examples of correct completions,

``` r
tibble::tibble(correct = token_correct)
```

    ## # A tibble: 60 x 1
    ##    correct                           
    ##    <chr>                             
    ##  1 "<sof> knitr :: "                 
    ##  2 "knitr :: opts_chunk "            
    ##  3 ":: opts_chunk $ "                
    ##  4 "opts_chunk $ set "               
    ##  5 "$ set ( "                        
    ##  6 "set ( echo "                     
    ##  7 "( echo = "                       
    ##  8 "FALSE ) <eol> "                  
    ##  9 "<eof> options ( "                
    ## 10 "options ( htmltools.dir.version "
    ## # … with 50 more rows

A few examples of incorrect completions,

``` r
tibble::tibble(incorrect = token_incorrect)
```

    ## # A tibble: 225 x 1
    ##    incorrect           
    ##    <chr>               
    ##  1 "echo = TRUE "      
    ##  2 "= FALSE , "        
    ##  3 ") <eol> <eol> "    
    ##  4 "<eol> <eof> knitr "
    ##  5 "= FALSE , "        
    ##  6 "<eol> # train "    
    ##  7 <na>                
    ##  8 <na>                
    ##  9 <na>                
    ## 10 <na>                
    ## # … with 215 more rows
