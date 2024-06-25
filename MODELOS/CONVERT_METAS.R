# Define the data as a string
data_string <- "10000:0.02 / 20000:0.03 / 30000:0.04 / 40000:0.05"

# Split the data into individual components
data_split <- strsplit(data_string, " / ")[[1]]

# Create a dataframe from the split data
data_frame <- do.call(rbind, lapply(data_split, function(x) {
  param_valor <- strsplit(x, ":")[[1]]
  data.frame(PARAM = as.numeric(param_valor[1]), VALOR = as.numeric(param_valor[2]))
}))

# Print the resulting dataframe
print(data_frame)
