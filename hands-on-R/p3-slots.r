wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
payouts <- c(
  "DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
  "B" = 10, "C" = 10, "0" = 0
) # this is effectively a dictionary


get_symbols <- function() {
  sample(wheel,
    size = 3, replace = TRUE,
    prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
  )
}

score <- function(symbols) {
  prize <- 0

  # 0. Count diamonds
  diamonds <- sum(symbols == "DD")
  print(diamonds)

  is_same <- diamonds - length(unique(symbols) == 1) >= 0
  is_all_bars <- all(symbols %in% c("B", "BB", "BBB"))

  # 1. Check for three of a kind
  if (is_same) {
    prize <- unname(payouts[symbols[1]])
  } else if (is_all_bars) {
    prize <- 5
  } else {
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries + 1]
  }

  # double the prize for each diamond
  prize <- prize * 2 ^ diamonds
}

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

slot_display <- function(prize) {
  # extract symbols
  symbols <- attr(prize, "symbols")

  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")

  # combine symbol with prize as a character string
  # \n is special escape sequence for a new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")

  # display character string in console without quotes
  cat(string)
}

print.slots <- function(x, ...) {
  slot_display(x)
}