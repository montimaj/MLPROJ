zillow.data <- subset(zillow.data, 
                      select = -c(yearBuilt: longitude,
                                  real.estate.provider:commute, latitude))
zillow.data <- na.omit(zillow.data)
# there are two rows in the na removed df where price is blank

transform_price <- function(price, sep = ',') {
  library(qdapRegex)
  
  if (price == '' || is.na(price)) {  # Fixes blank issue
    return(NA) 
  }
  price <- paste(price, '/', sep = '')
  check_plus <- ex_between(price, '$', '+')[[1]]
  check_bracket <- ex_between(price, '$', ')')[[1]]
  check_slash <- ex_between(price, '$', '/')[[1]]
  p <- NA
  if (!is.na(check_plus)) {
    p <- as.numeric(gsub(sep, "", check_plus))
  } else if (!is.na(check_bracket)) {
    p <- as.numeric(gsub(sep, "", check_bracket))
  } else if (!is.na(check_slash)) {
    p <- as.numeric(gsub(sep, "", check_slash))
  }
  return(p)
}

zillow.data$price <- sapply(zillow.data$price, transform_price)
zillow.data <- na.omit(zillow.data)
str(zillow.data)
summary(zillow.data)
write.csv(zillow.data, '../Data/zillow_cleaned.csv', row.names = FALSE)