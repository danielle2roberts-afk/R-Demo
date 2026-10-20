calculate <- function(first, second, operator) {
  switch(
    operator,
    "+" = first + second,
    "-" = first - second,
    "*" = first * second,
    "/" = {
      if (second == 0) {
        stop("Cannot divide by zero.", call. = FALSE)
      }
      first / second
    },
    stop("Unsupported operator. Choose +, -, *, or /.", call. = FALSE)
  )
}

read_number <- function(prompt) {
  value <- suppressWarnings(as.numeric(readline(prompt)))
  if (length(value) != 1 || is.na(value) || !is.finite(value)) {
    stop("Please enter a finite number.", call. = FALSE)
  }
  value
}

if (sys.nframe() == 0L) {
  first <- read_number("Enter the first number: ")
  operator <- trimws(readline("Choose an operation (+, -, *, /): "))
  second <- read_number("Enter the second number: ")

  result <- calculate(first, second, operator)
  cat("Result:", result, "\n")
}