# web_scrapers
Repository for all the various web scraping R scripts I've built. Please scrape responsibly.

scoutingtherefs_functions.R is a r script that produces the function `scrape_refs` with the input being the URL of today's article on scoutingtherefs.com
  
  for example: scrape_refs("https://scoutingtherefs.com/2022/12/36270/todays-nhl-referees-and-linesmen-12-6-22/") will return a data frame with:
    
    referee (character)
    
    goals_per_game (numeric)
    
    pp_per_game (numeric)
   
    pen_per_game (numeric)
    
    pim_per_game (numeric)
    
    home_penalties (numeric)
    
    road_more_than_home (numeric)
    
    game (character)
