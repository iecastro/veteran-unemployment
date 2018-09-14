library(tidyverse)
library(rvest)
library(scales)


states <- read_html("https://www.bls.gov/news.release/vet.t06A.htm") %>%
  html_node(".sub1 , #cps_vets_a06a") %>%
  html_table(header = FALSE, fill = TRUE) 

colnames(states)

bls_pct <- states %>% select(X1,X7,X8) %>%
  slice(5:56) %>% transmute("State" = X1, "count" = as.numeric(X7), "pct" = as.numeric(X8))

          
bls_pct %>% filter(State != "Total, 18 years and over") %>%
  ggplot(aes(pct,reorder(State,-pct))) + geom_point() +
  geom_segment(aes(x = 0, y = State, xend = pct, yend = State)) +
  theme_minimal() 


