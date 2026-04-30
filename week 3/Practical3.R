# Exercise 1

library(palmerpenguins)
library(dplyr)

mean_mass_by <- function(data, group_var) {
  data |>
    group_by(group_var) |>
    summarise(mean_mass = mean(body_mass_g, na.rm = TRUE))
}

mean_mass_by(penguins, species)

# solution
mean_mass_by <- function(data, group_var) {
  data |>
    group_by({{ group_var }}) |>
    summarise(mean_mass = mean(body_mass_g, na.rm = TRUE))
}
mean_mass_by(penguins, species)

# Exercise 2

summarise_species <- function(data) {
  data |>
    group_by(species) |>
    summarise(mean_mass = mean_body_mass(body_mass_g))
}

mean_body_mass <- function(x) {
  mean(x, na.rm = TREU)   
}

summarise_species(penguins)

# solution
# 
# Error in `summarise()`:
#   ℹ In argument: `mean_mass = mean_body_mass(body_mass_g)`.
# ℹ In group 1: `species = Adelie`.
# Caused by error in `mean_body_mass()`:
#   ! object 'TREU' not found
# Run `rlang::last_trace()` to see where the error occurred.
# 
# Each line adds one piece of information:
#   
#   Error in summarise() — the dplyr verb where it surfaced.
# In argument: ... — which argument of summarise() was being computed.
# In group 1: species = Adelie — which group was being processed.
# Caused by error in mean_body_mass() — your helper function.
# object 'TREU' not found — the actual R-level problem.
# Run rlang::last_trace() ... — dplyr tells you what to do next.
# 
# You already know the bug is in mean_body_mass() before running last_trace(). The backtrace just confirms the exact line.
# 
# The backtrace. rlang::last_trace() shows ~14 frames. Most are dplyr internals (summarise.grouped_df, summarise_cols, map, lapply, mask$eval_all_summarise, …) — safe to skip. The frames that matter are:
#   
#   Frame 1: summarise_species(penguins) — your top-level call.
# Frame 11: mean_body_mass(body_mass_g) — your function, innermost. This is where the bug lives.
# Frames 12–14: mean() → mean.default() → isTRUE(na.rm) — base R tries to evaluate TREU and can’t find it.
# 
# Fix: replace TREU with TRUE on the na.rm line of mean_body_mass().
# 
# Lesson: the error message usually gets you close; the backtrace pins down the line. Scan for your own function names and stop at the innermost one.

mean_body_mass <- function(x) {
  mean(x, na.rm = TRUE)   
}

summarise_species(penguins)

# Exercise 3

browser()

my_factorial <- function(n) {
  if (n == 1) return(1)
  return(n * my_factorial(n - 1))
}

my_factorial(5)   # returns 120 — correct
my_factorial(0)   # hangs / errors
my_factorial(3.5) # also wrong

# solztion

my_factorial <- function(n) {
  stopifnot(n >= 0, n == as.integer(n))
  if (n <= 1) return(1)
  return(n * my_factorial(n - 1))
}

my_factorial(5)   
my_factorial(0)   
my_factorial(3.5) 

# Exercise 4

standardise <- function(x) {
  (x - mean(x)) / sd(x)
}

standardise(c(1, 2, 3, 4, 5))   # fine
standardise(c(1, 2, NA, 4, 5))  # returns all NAs — why?

debugonce(standardise)
standardise(c(1, 2, NA, 4, 5))

# solution

standardise <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}