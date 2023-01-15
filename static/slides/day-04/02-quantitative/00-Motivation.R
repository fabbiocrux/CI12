# Loading the Package
library(tidyverse)

# load the data set
can_lang <- read_csv("data/can_lang.csv")
# obtain the 10 most common Aboriginal languages
aboriginal_lang <- filter(can_lang, 
                          category == "Aboriginal languages")
arranged_lang <- arrange(aboriginal_lang, 
                         by = desc(mother_tongue))
ten_lang <- slice(arranged_lang, 1:10)
# create the visualization
ggplot(ten_lang, aes(x = mother_tongue,
                     y = reorder(language, mother_tongue))) +
   geom_bar(stat = "identity") +
   xlab("Mother Tongue (Number of Canadian Residents)") +
   ylab("Language")



library(rlyrics)
song <- data.frame(song_title  = c("22", "Bohemian Rhapsody"), artist = c("Taylor Swift", "Queen"))


# install.packages("devtools")
devtools::install_github("UBC-MDS/rlyrics")
