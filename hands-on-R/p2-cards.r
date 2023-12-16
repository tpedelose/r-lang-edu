new_deck <- read.csv("hands-on-R/deck.csv")
games <- factor(c("Blackjack", "Poker", "War", "Hearts"))

setup <- function(deck_init) {
  # create a new deck
  deck <- deck_init

  shuffle <- function() {
    deck_len <- nrow(deck)
    random <- sample(1:deck_len, size = deck_len)
    # shuffle the deck
    assign("deck", deck[random, ], envir = .GlobalEnv)
  }

  deal <- function() {
    # deal a card from top of the deck
    card <- deck[1, ]
    # overwrite the global deck
    assign("deck", deck[-1, ], envir = .GlobalEnv)
    # return the card
    card
  }

  list(deal = deal, shuffle = shuffle)
}

start_game <- function(game) {
  if (game == "War") {
    ace_indices <- c(13, 26, 39, 52)
    deck$value[ace_indices, ] <- 14
  } else if (game == "Hearts") {
    deck$value <- 0
    deck$value[deck$suit == "hearts"] <- 1
    deck$value[deck$face == "queen" & deck$suit == "spades"] <- 13
  } else {
    print("Invalid game")
  }
}

cards <- setup(new_deck)
