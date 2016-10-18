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
  serial_time <- filter(df, p==1)$t
  mutate(df, scalability=serial_time/t)
}
