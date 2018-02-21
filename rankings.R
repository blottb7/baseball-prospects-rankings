#libraries
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

#Description:
#Read in ranking lists from prominent baseball prospects lists:
#fangraphs, baseball america, baseball prospectus
#also add mlb pipeline, but weigh it less
#second tier: mlb, razzball, katoh

#baseball america
url <- read_html('https://www.baseballamerica.com/minors/top-100-mlb-prospects-2018/')

#this is name, pos, team
bba <- html_nodes(url, 'p~ p+ p')
bba <- html_text(bba)
bba <- tibble(bba)

#baseball prospectuts
url <- read_html('https://www.baseballprospectus.com/prospects/article/37535/baseball-prospectus-top-101-prospects-2018-top-mlb-prospects-ronald-acuna-victor-robles-vladimir-guerrero-jr-eloy-jimenez/')

#this is csv name, pos, team
bbp <- html_nodes(url, '.article-content li')
bbp <- html_text(bbp)
bbp <- tibble(bbp)

#fangraphs
url <- read_html('https://www.fangraphs.com/blogs/2018-top-100-prospects/')

#reads in 100 x 7 table
fg <- html_nodes(url, '.table-wrapper td')
fg <- html_text(fg)
fg <- data.frame(matrix(fg, nrow = 100, ncol = 7, byrow = TRUE))

#name vars
names(fg) <- c("fg_rank", "name", "team_abb", "age", "fg_pos", "fv", "variance")

#clean the 3 dataframes

#1.bba

#separate into rank, name, pos, team
bba1 <- bba %>%
  separate(bba, c("name", "bba_pos", "team"), "\\|")

bba1 <- bba1 %>%
  separate(team, c("blank", "team", "extra"), "\\ ") %>%
  select(-blank, -extra)

bba1 <- bba1 %>%
  separate(name, c("bba_rank", "name"), "\\.", extra = "merge")

#trim leading and trailing white space
bba1$name <- str_trim(bba1$name)
bba1$bba_pos <- str_trim(bba1$bba_pos)

#change team names where nec.
bba1$team <- ifelse(bba1$team == "Blue", "Blue Jays", bba1$team)
bba1$team <- ifelse(bba1$team == "D'backs", "Diamondbacks", bba1$team)
bba1$team <- ifelse(bba1$team == "Red", "Red Sox", bba1$team)
bba1$team <- ifelse(bba1$team == "White", "White Sox", bba1$team)

#remove periods from names for consistent joining
bba1$name <- str_replace_all(bba1$name, "\\.", "")

#2. bbp

bbp1 <- bbp
#clean the one comma'd name
bbp1$bbp <- ifelse(bbp1$bbp == "Fernando Tatis, Jr., SS, San Diego Padres",
                   "Fernando Tatis Jr., SS, San Diego Padres",
                   bbp1$bbp)
#separate into name, pos, city_team or c(city, team)
bbp1 <- bbp1 %>%
  separate(bbp, c("name", "bbp_pos", "city_team"), "\\,")

#trim leading and trailing white space
bbp1$city_team <- str_trim(bbp1$city_team)
bbp1$bbp_pos <- str_trim(bbp1$bbp_pos)

#remove periods from names
bbp1$name <- str_replace_all(bbp1$name, "\\.", "")
#add rank by row number
bbp1$bbp_rank <- 1

for(i in 1:nrow(bbp1)) {
  bbp1$bbp_rank[i] <- i
}

#homogenize names
bbp1$name <- ifelse(bbp1$name == "Mitch White", "Mitchell White", bbp1$name)

#fangraphs

fg1 <- fg

#clean periods and commas from player names
fg1$name <- str_replace_all(fg1$name, "\\.", "")
fg1$name <- str_replace_all(fg1$name, "\\,", "")

#combine dataframes
df <- bba1 %>%
  full_join(bbp1, by = "name") %>%
  full_join(fg1, by = "name") %>%
  arrange(name)

#FIX BBP VLAD GUERR! ITS COMING UP AS UNIQUE TO THE OTHER SPELLING
  #also change Jordon to Jo Adell in bbp

#start cleaning
#convert numeric vars to correct classes
df$bba_rank <- as.numeric(df$bba_rank)
df$fg_rank <- as.numeric(as.character(df$fg_rank))

#create a single team column
df$team <- ifelse(!is.na(df$team), df$team,
                  ifelse(!is.na(df$city_team), df$city_team,
                         as.character(df$team_abb)))

df <- df %>%
  select(-city_team, -team_abb)

#create a single player position column
df$position <- ifelse(!is.na(df$bba_pos), df$bba_pos,
                      ifelse(!is.na(df$bbp_pos), df$bbp_pos,
                             as.character(df$fg_pos)))

df <- df %>%
  select(-bba_pos, -bbp_pos, -fg_pos)

#create a single ranking var
#subset ranks
df_ranks <- df %>%
  select(bba_rank, bbp_rank, fg_rank)

#take the rowMeans of rankings; take single ranking if player ranked only once
df_ranks$rank <- 0  #initialize rank var
for(i in 1:nrow(df_ranks)) {
  
  df_ranks$rank[i] <- ifelse(sum(is.na(df_ranks[i,])) <= 1, rowMeans(df_ranks[i,1:3], na.rm = TRUE),
                             ifelse(!is.na(df_ranks[i,1]), df_ranks[i,1],
                             ifelse(!is.na(df_ranks[i,2]), df_ranks[i,2],
                             df_ranks[i,3])))
  
}

#select only rank for binding with players
df_ranks_new <- df_ranks %>%
  select(rank)

#bind the new rank
df_new <- df %>%
  bind_cols(df_ranks_new)

df_new$rank <- round(as.numeric(df_new$rank))  #remove decimal players

df_new <- df_new %>%
  arrange(rank, bba_rank, bbp_rank)  #arrange by rank, with bba and bbp ranks as tie-breakers

#organize columns nicely
df_new1 <- df_new %>%
  select(mean_rank = rank, name, position, age, team, fv, variance, bba_rank, bbp_rank, fg_rank)

#TO DO
#organize by position

#FIX BBP VLAD GUERR! ITS COMING UP AS UNIQUE TO THE OTHER SPELLING
#also change Jordon to Jo Adell in bbp

write.csv(df_new1, "C:/Users/Ben/Desktop/R Projects/baseball_prospects_2018.csv")
# df <- df %>%
#   mutate(rank = rowMeans(c(bba_rank, bbp_rank, fg_rank), na.rm = TRUE))

# df_rank <- df %>%
#   select(bba_rank, bbp_rank, fg_rank) %>%
#   mutate(rank = rowsum(bba_rank, bbp_rank, fg_rank, na.rm = TRUE))
#
# #clean the one comma'd name
# copy$bbp <- ifelse(copy$bbp == "Fernando Tatis, Jr., SS, San Diego Padres",
#                    "Fernando Tatis Jr., SS, San Diego Padres",
#                    copy$bbp)
# #separate into name, pos, city_team or c(city, team)
# copy1 <- copy %>%
#   separate(bbp, c("name", "position", "city_team"), "\\,")
# 
# #trim leading and trailing white space
# copy1$city_team <- str_trim(copy1$city_team)
# copy1$position <- str_trim(copy1$position)
# 
# #remove periods from names
# copy1$name <- str_replace_all(copy1$name, "\\.", "")
# #add rank by row number
# copy1$bbp_rank <- 1
# 
# for(i in 1:nrow(copy1)) {
#   copy1$bbp_rank[i] <- i
# }
# 
# temp <- copy %>%
#         separate(bba, c("name", "position", "team"), "\\|")
# # temptwo <- temp %>%
# #         separate(team, c("teamname", "extra"), "\\ ")
# temptwo <- temp %>%
#   separate(team, c("blank", "team", "extra"), "\\ ") %>%
#   select(-blank, -extra)
# # temptwo <- temp %>%
# #   separate(team, c("teamname1", "extra"), "\\<f0>", extra = "merge")
# # temptwo <- temp %>%
# #         separate(team, c("teamname1", "teamname2", "extra"), "\\ ")
# 
# tempthree <- temptwo %>%
#   separate(name, c("bba_rank", "name"), "\\.", extra = "merge")
# 
# # tempthree[,2] <- str_trim(tempthree[,2])
# #trim leading and trailing white space
# tempthree$name <- str_trim(tempthree$name)
# tempthree$position <- str_trim(tempthree$position)
# #change team names where nec.
# tempthree$team <- ifelse(tempthree$team == "Blue", "Blue Jays", tempthree$team)
# tempthree$team <- ifelse(tempthree$team == "D'backs", "Diamondbacks", tempthree$team)
# tempthree$team <- ifelse(tempthree$team == "Red", "Red Sox", tempthree$team)
# tempthree$team <- ifelse(tempthree$team == "White", "White Sox", tempthree$team)
# #
# new <- str_split(copy[1:10,], fixed("<"), n = 2, simplify = TRUE)
# new <- 
# #
# new_copy <- lapply(split_copy, cbind)
# new_copy <- as.data.frame(new_copy)
# #
# copy <- as.matrix(bba, nrow = 100, ncol = 1)
# copy <- as.list(copy)
# 
# split_copy <- str_split(copy, pattern = "<")
# new_copy <- lapply(split_copy, cbind)
# new_copy <- t(as.data.frame(new_copy))
# #
# new <- matrix()
# for(i in 1:100) {
#   new[i,] <- str_split[copy[i,], "<", simplify = TRUE]
# }
# #new_bba <- matrix(, nrow = 100, ncol = 2)
# new_bba <- str_split(copy[,1], pattern = "<", n = 2, simplify = TRUE)
# #
# copy <- str_split(copy, pattern = "<")
# copy <- str_split(copy, pattern = ">")
# copy <- as.data.frame(copy)
# # copy <- as.data.frame(str_split(copy, pattern = "<"))
# # filter(copy, contains("U+"))
# copy %>%
#   filter(contains(as.character(copy), 'U+'))
# copy1 <- copy[contains("U+")]
# # copy <- as.data.frame(str_split(copy, pattern = ">"))
# 
# #empty_bba <- matrix(, nrow = 100, ncol = 2)
# for(i in 1:nrow(copy)) {
#   empty_bba[[i]] <- str_split(copy[i], "<", n = 2)
#   
# }
# 
# copy[1,] <- str_split(copy[1,], "<")
# #
# copy1 <- str_split(copy, pattern = "<", n = 2, simplify = TRUE)
# copy1 <- as.data.frame(copy)
# str_subset(copy[,1], "Kyle Tucker")
# str_sub(copy[,1], start = "<", end = -1) <- ""
# 
# symbols <- "<"

# copy[,1] <- str_sub(copy[,1], start="<", end=">")
# copy[,1] <- gsub("\\<*","",copy[,1])
#add wander franco at some point

