# Define multiple data frames
A <- data.frame(ID = "A", X = c(10, 120, 1000, 10000), Y = c(0.01, 0.05, 0.08, 0.1))
B <- data.frame(ID = "B", X = c(15), Y = c(0.03))
C <- data.frame(ID = "C", X = c(16, 100, 1010, 20000), Y = c(0.02, 0.05, 0.06, 0.3))
D <- data.frame(ID = "D", X = c(8, 100, 3000, 10001), Y = c(0.01, 0.05, 0.07, 0.4))

# Combine the data frames into one
combined_df <- rbind(A, B, C, D)

# Define the function to match the result
find_match <- function(result, id, combined_df) {
  df <- subset(combined_df, ID == id)
  match_value <- 0
  for (i in seq_len(nrow(df))) {
    if (result < df$X[i]) {
      if (i == 1) {
        match_value <- 0
      } else {
        match_value <- df$Y[i - 1]
      }
      break
    }
  }
  if (result >= tail(df$X, 1)) {
    match_value <- tail(df$Y, 1)
  }
  return(match_value)
}

# Result data frame
result_df <- data.frame(ID = c("A", "B", "C", "D"), W = c(2, 30000, 1000, 350))

# Create an empty column to store the match results
result_df$Match <- NA

# Iterate over the result_df and apply find_match for each row
for (i in 1:nrow(result_df)) {
  result <- result_df$W[i]
  id <- result_df$ID[i]
  result_df$Match[i] <- find_match(result, id, combined_df)
}

# Print the final dataframe
print(result_df)
