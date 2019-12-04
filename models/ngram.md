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
# code <- code[1:10,]
```

First lets split between test and
validation,

``` r
index <- sample(1:2, size = nrow(code), replace = TRUE, prob = c(.90, .10))
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

    ## An ngram object with 5030518 3-grams

And retrieve the phrase
table,

``` r
ngram_table <- ngram::get.phrasetable(ngram_model) %>% tibble::as_tibble() %>% print()
```

    ## # A tibble: 5,030,518 x 3
    ##    ngrams               freq    prop
    ##    <chr>               <int>   <dbl>
    ##  1 ") <eol> <eol> "   229387 0.00902
    ##  2 ") ) <eol> "       203483 0.00801
    ##  3 "= c ( "           118805 0.00467
    ##  4 ") , <eol> "       106557 0.00419
    ##  5 ") %>% <eol> "      88105 0.00347
    ##  6 ") + <eol> "        67994 0.00267
    ##  7 "<eol> library ( "  60861 0.00239
    ##  8 "= TRUE ) "         54134 0.00213
    ##  9 "<eol> <eol> # "    50485 0.00199
    ## 10 "<eol> data ( "     49820 0.00196
    ## # … with 5,030,508 more rows

Then we validate against the test dataset,

``` r
test_tokens <- strsplit(test_code, " ")[[1]]
test_tokens <- test_tokens[test_tokens != ""]

match_count <- 0
checks_count <- length(test_tokens)-ngram_size

token_incorrect <- c()
token_correct <- c()
start_time <- Sys.time()

for (i in 1:checks_count) {
  # Cap test time to 5 minutes
  if (Sys.time() > start_time + 60 * 5) break
  
  current_ngram <- test_tokens[i:(i+ngram_size-2)]
  current_text <- paste(current_ngram[1:ngram_size-1], collapse = " ")
  
  matches <- stringr::str_starts(ngram_table$ngrams, stringr::fixed(current_text))
  best <- ngram_table[matches,]$ngrams
  best <- if (length(best) == 0) "<na>" else best[1]
  
  correct_match <- paste0(current_text, " ", test_tokens[i+ngram_size-1], " ")
  if (correct_match %in% best) {
    match_count <- match_count + 1
    
    if (length(token_correct) < 1000) token_correct <- c(token_correct, correct_match)
  }
  else {
    if (length(token_incorrect) < 1000) token_incorrect <- c(token_incorrect, best)
  }
}

message("Correct: ", match_count, "\n",
        "Incorrect: ", checks_count, "\n",
        "Accuracy: ", match_count / checks_count)
```

    ## Correct: 300
    ## Incorrect: 2874646
    ## Accuracy: 0.000104360676062374

A few examples of correct completions,

``` r
tibble::tibble(correct = token_correct)
```

    ## # A tibble: 300 x 1
    ##    correct               
    ##    <chr>                 
    ##  1 "<sof> knitr :: "     
    ##  2 "knitr :: opts_chunk "
    ##  3 ":: opts_chunk $ "    
    ##  4 "opts_chunk $ set "   
    ##  5 "$ set ( "            
    ##  6 "set ( echo "         
    ##  7 "( echo = "           
    ##  8 "echo = TRUE "        
    ##  9 ", message = "        
    ## 10 "message = FALSE "    
    ## # … with 290 more rows

A few examples of incorrect completions,

``` r
tibble::tibble(incorrect = token_incorrect)
```

    ## # A tibble: 272 x 1
    ##    incorrect       
    ##    <chr>           
    ##  1 "= TRUE ) "     
    ##  2 "TRUE , <eol> " 
    ##  3 "= FALSE ) "    
    ##  4 "FALSE , <eol> "
    ##  5 "= FALSE ) "    
    ##  6 ", <eol> data " 
    ##  7 "comment = NA " 
    ##  8 "= \"\" ) "     
    ##  9 "\"\" , <eol> " 
    ## 10 "digits = 2 "   
    ## # … with 262 more rows
