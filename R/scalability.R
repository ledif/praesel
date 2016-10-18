library(dplyr)

#' Mutate a dataframe to add a scalability column
#' 
#' @param df The dataframe that contains time and processor count information
#' @return A new dataframe with scalability
#' @examples
#' df <- data.frame(p=c(1,2,4))
#' df$t <- runif(nrow(df), 1.0, 2.0) / df$p
#' scalability(df)
scalability <- function(df)
{
  # Empty data frames
  if (nrow(df) == 0) {
    return(df)
  }

  # Get the time on one processor
  serial_time <- filter(df, p==1)$t;

  # If this group doesn't have a one processor observation,
  # then the scalability is NA
  if (identical(serial_time, numeric(0))) {
    serial_time <- NA
  }

  # Scalability is single proc time over time
  return(mutate(df, scalability=serial_time/t))
}
