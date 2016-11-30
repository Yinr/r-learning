guess.with.cheat <- function(n = 1000, cheat = 0.01){
  # Guess game: guess a number between 1 and n
  
  # generate random number between 1 and n
  num <- as.integer(runif(1, 1, n))
  # save guess history
  guess.log <- c()
  # input the number
  guess.input <- function(){
    guess.num <- suppressWarnings(
      as.integer(readline("guess an integer: "))
      )
    # check the input 
    while (is.na(guess.num) | guess.num < 1 | guess.num > n) {
      cat("Input Error\n", 
          "Please input an integer between 1 to", n, "!\n\n")
      guess.num <- suppressWarnings(
        as.integer(readline("Guess an integer: "))
        )
    }
    return(guess.num)
  }
  
  # Start game information
  cat("Guess Game\n\n")
  cat("Guess an integer from 1 to", n, ".\n\n")
  
  # guessing...
  guess.num <- guess.input()
  # log the guess number
  guess.log <- c(guess.log, guess.num)
  
  while (guess.num != num) {
    # do not get the right number
    if (guess.num < num) {
      cat("Too Small !")
    } else {
      cat("Too large !")
    }
    guess.num <- guess.input()
    guess.log <- c(guess.log, guess.num)
  }
  
  # get the right number !
  cat("Bingo ! You got the number", num, "in", length(guess.log), "time(s) !")
  # plot the guess log
  plot(guess.log, main = "Guess log", type = "b", 
       ylim = c(1, n), ylab = "guess number")
}

