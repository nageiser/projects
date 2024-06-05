## A pair of functions replicating a simplified version of the Enigma machine -- a plugboard
## function plugboard() and an encryption function l_enigma()

plugboard <- function(){
  k <- 13
  pairs <- matrix(nrow = 2, ncol = 13)
  for(i in 1:k) {
    pair <- sample(letters, size = 2, replace = FALSE)
    pairs[,i] <- pair
    letters <- letters[!letters %in% pair]
  }
  spaces <- c(" ", " ")
  pairs <- cbind(pairs, spaces)
  return(pairs)
}

setting <- plugboard()

l_enigma <- function(text) {
  require(stringr)
  text <- strsplit(text, split = "")[[1]]
  text <- str_to_lower(text)
  setting_upper <- setting[1,]
  setting_lower <- setting[2,]
  out <- ""
  for (char in text){
    if (char %in% setting_upper) {
      out <- paste0(out, setting_lower[setting_upper == char])
    } else if (char %in% setting_lower) {
      out <- paste0(out, setting_upper[setting_lower == char])
    } else {
      out <- paste0(out, char)
    }
  }
  out <- str_to_title(out, locale = "en")
  return(out)
}
