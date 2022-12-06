# Base URL example, 12/5/2022
# https://scoutingtherefs.com/2022/12/36246/todays-nhl-referees-and-linesmen-12-5-22/


scrape_refs <- function(link){
  
  #link <- "https://scoutingtherefs.com/2022/12/36270/todays-nhl-referees-and-linesmen-12-6-22/"
  
  r_list <- list()
  
  require(httr)
  require(tidyverse)
  require(rvest)
  
  r_list$r_scout <- try(unlist(strsplit(gsub("\\t", "", gsub("\\r", "", GET(link))), "\n")), TRUE)
  r_scout <- r_list$r_scout
  
  # Find and replace the percentages for easier regex capture below
  # r_scout <- gsub ("\\%","", r_scout)
  
  # Index rows in the source code
  rows_games <- grep("h1 style", r_scout) # Games
  rows_name <- grep("line-height: 95%; border: none", r_scout) #ref names
  rows_goals <- grep("Goals/Gm", r_scout) # goals per game
  rows_pp <- grep("PP/Gm", r_scout) # power plays per game
  rows_pen <- grep("Penl/Gm", r_scout) # penalties per game
  rows_pim <- grep("PIM/Gm", r_scout) # penalty minutes per game
  rows_perHome <- grep("% of Penl on Home", r_scout) # percent of penalties on home team
  rows_road <- grep("More Penl on Rd %", r_scout) # percent of time more penl on road
  
  # Based on the rows above, index the rows that have the data within them
  rows_goals_data <- sort(c(outer(rows_goals, 1:2, "+")))
  rows_pp_data <- sort(c(outer(rows_pp, 1:2, "+")))
  rows_pen_data <- sort(c(outer(rows_pen, 1:2, "+")))
  rows_pim_data <- sort(c(outer(rows_pim, 1:2, "+")))
  rows_perHome_data <- sort(c(outer(rows_perHome, 1:2, "+")))
  rows_road_data <- sort(c(outer(rows_road, 1:2, "+")))
  
  # Subset the source code by rows for each item and then extract the metric
  games_index <- r_scout[rows_games[grep(" *<h1 [s ].*", r_scout[rows_games])]]
  games_index <- gsub ("é","e", games_index) # Replace accented e
  games_value <- gsub(".*<strong>([A-Za-zÀ-ÖØ-öø-ÿ' \\.\\(\\),-]{1,})[ \t].*", "\\1", games_index)
  
  names_index <- r_scout[rows_name[grep(" *<td [s ].*", r_scout[rows_name])]]
  names_value <- gsub(".*<strong>([A-Za-zÀ-ÖØ-öø-ÿ' \\.\\(\\),-]{1,})[ \t].*", "\\1", names_index)
  
  goals_index <- r_scout[rows_goals_data[grep(" *<td [c ].*", r_scout[rows_goals_data])]]
  goals_metric <- gsub(".*<strong>([0-9]*.[0-9])</strong>.*", "\\1", goals_index)
  
  pp_index <- r_scout[rows_pp_data[grep(" *<td [c ].*", r_scout[rows_pp_data])]]
  pp_metric <- gsub(".*<strong>([0-9]*.[0-9])</strong>.*", "\\1", pp_index)
  
  pen_index <- r_scout[rows_pen_data[grep(" *<td [c ].*", r_scout[rows_pen_data])]]
  pen_metric <- gsub(".*<strong>([0-9]*.[0-9])</strong>.*", "\\1", pen_index)
  
  pim_index <- r_scout[rows_pim_data[grep(" *<td [c ].*", r_scout[rows_pim_data])]]
  pim_metric <- gsub(".*<strong>([0-9]*.[0-9])</strong>.*", "\\1", pim_index)
  
  perHome_index <- r_scout[rows_perHome_data[grep(" *<td [c ].*", r_scout[rows_perHome_data])]]
  perHome_index <- gsub ("\\%","", perHome_index)
  perHome_metric <- gsub(".*<strong>([0-9]*.[0-9])</strong>.*", "\\1", perHome_index)
  
  road_index <- r_scout[rows_road_data[grep(" *<td [c ].*", r_scout[rows_road_data])]]
  road_index <- gsub ("\\%","", road_index)
  road_metric <- gsub(".*<strong>(-?[0-9]*.[0-9])</strong>.*", "\\1", road_index)
  
  # Create a data frame for the list of today's games
  today_games <- data.frame(games_value)
  today_games <- today_games[rep(seq_len(nrow(today_games)), each = 2), ]
  
  # Column bind all the metrics into a data.frame
  w_scout <- bind_cols(names_value,
                       goals_metric, pp_metric, pen_metric, pim_metric,
                       perHome_metric, road_metric,
                       today_games
  )
  
  # Rename columns
  names(w_scout)[1] <- "referee"
  names(w_scout)[2] <- "goals_per_game"
  names(w_scout)[3] <- "pp_per_game"
  names(w_scout)[4] <- "pen_per_game"
  names(w_scout)[5] <- "pim_per_game"
  names(w_scout)[6] <- "home_penalties"
  names(w_scout)[7] <- "road_more_than_home"
  names(w_scout)[8] <- "game"
  
  # Make numeric
  w_scout <- w_scout %>% 
    mutate_at(c('goals_per_game', 'pp_per_game',
                'pen_per_game', 'pim_per_game', 
                'home_penalties', 'road_more_than_home'), 
              as.numeric) %>% 
    mutate(home_penalties = home_penalties/100,
           road_more_than_home = road_more_than_home/100)
  
  return(w_scout)
  
  
  

}


refs <- scrape_refs("https://scoutingtherefs.com/2022/12/36270/todays-nhl-referees-and-linesmen-12-6-22/")

