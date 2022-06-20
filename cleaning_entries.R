library(googlesheets4)
library(googledrive)
library(janitor)
library(tidyverse)
link <- "https://docs.google.com/spreadsheets/d/1U_XQYOzh8HWiTyTBZ7aYlFy4XcWMSbVXX3pmTau1CII"

pool <- read_sheet(link)
pool <- pool %>%
    clean_names() #%>%

colnames(pool) <- str_remove_all(colnames(pool), "pick_(one|two)_")

pool <- pool %>% 
    select(your_name_use_name_number_if_submitting_multiple_entries:tier_4_players_rank_name) %>%
    pivot_longer(cols = -your_name_use_name_number_if_submitting_multiple_entries,
                 names_to="tiers",
                 values_to="players") %>%
    separate_rows(players, sep=",") %>%
    rename(name=your_name_use_name_number_if_submitting_multiple_entries)

pool_f <- pool %>%
    mutate(players=str_trim(str_remove(players, '[0-9]+-')),
           tiers=str_extract(tiers, "[0-9]+"),
           entry_num=str_extract(name, "[0-9]+"),
           entry_num=case_when(
               is.na(entry_num) ~ 1,
               entry_num == 1 ~ 1,
               entry_num == 2 ~ 2,
               entry_num == 3 ~ 3,
               entry_num == 4 ~ 4,
               entry_num == 5 ~ 5,
               entry_num == 6 ~ 6
           )) 

#range_write(eg, arrange(pool_f, name))
library(tidytext)
pool_f  %>%
    count(players) %>%
    mutate(players=reorder_within(players, n, players)) %>%
    ggplot(aes(players, n)) +
    geom_col() + 
    geom_text(aes(label=round(n,2)), 
              position=position_dodge(width=0.9), hjust=-.5) +
    scale_x_reordered() +
    coord_flip() 
    
library(rvest)

espn_link <- "https://www.espn.com/golf/leaderboard"
current_scores <- espn_link %>% 
    read_html() %>%
    html_nodes("table") %>% 
    html_table() %>%
    .[[1]] %>%
    select(-2) %>%
    as_tibble() %>%
    clean_names() %>%
    rename(players=player) %>%
    filter(r1 != "Projected Cut E",
           players != "The following players failed to make the cut at E") %>%
    select(players, r1, r2, r3, r4) %>%
    mutate(across(r1:r4, function(x) as.double(x)-72),
           players=str_trim(str_remove(players, "\\(a\\)"))) 

current_scores[is.na(current_scores)] <- 8

current_scores <- espn_link %>% 
    read_html() %>%
    html_nodes("table") %>% 
    html_table() %>%
    .[[1]] %>%
    select(-2) %>%
    as_tibble() %>%
    clean_names() %>%
    rename(players=player) %>%
    filter(r1 != "Projected Cut E",
           players != "The following players failed to make the cut at E") %>%
    select(players, today, r1, r2, r3, r4) %>%
    mutate(across(r1:r3, function(x) as.double(x)-72),
           players=str_trim(str_remove(players, "\\(a\\)"))) %>%
    select(-r4) %>% 
    rename(r4=today) %>% 
    mutate(r4=ifelse(r4=="E", 0, r4),
           r4=as.double(r4)) %>%
    select(players, r1, r2, r3, r4)

current_scores[is.na(current_scores)] <- 8


current_scores %>%
    count(r1)

current_scores


# 
# current_scores <- current_scores %>% 
# mutate(r1=ifelse(str_detect(r1, "E"), 0, r1),
#            r1=as.double(str_remove(r1, "\\+")),
#            players=str_trim(str_remove(players, "\\(a\\)")))
# 
# 

# misspelled_players <- pool_f %>%
#     left_join(current_scores) %>% 
#     filter(is.na(r1)) %>% select(players) %>%
#     distinct() %>%
#     arrange(players)
    
pool_f <- pool_f %>%
    mutate(players=case_when(
        players=="Bernd Weisberger" ~ "Bernd Wiesberger",
        players== "Brooks Keopka"  ~ "Brooks Koepka",
        players== "Bryson Dechambeau" ~ "Bryson DeChambeau",
        players == "CT Pan" ~ "C.T. Pan",
        players == "Dylan Fritilli" ~ "Dylan Frittelli",
        players == "John Rahm" ~ "Jon Rahm",
        players == "JT Poston" ~ "J.T. Poston",
        players == "Matthew Wolfe" ~ "Matthew Wolff",
        players == "Rory Mcllroy"  ~ "Rory McIlroy",
        players == "Sabastian Munoz" ~  "Sebastian Munoz",
        players == "Sergio Garcia"  ~  "Sergio Garcia",
        TRUE ~ players))


day_scores <- pool_f %>% 
    left_join(current_scores) %>%
    mutate(round_total=r1+r2+r3+r4) %>%
    group_by(name) %>%
    summarize(total_score=sum(round_total[-which.max(round_total)], na.rm=TRUE)) %>%
    arrange(total_score) 

day_scores %>% 
    filter(name != "Abbas Rizvi-1")  %>%
    bind_rows({day_scores %>% 
            filter(name == "Abbas Rizvi-1") %>%
            mutate(total_score=-44)}) %>%
    mutate(rank=as.numeric(factor(rank(total_score)))) 



day_scores <- day_scores %>%
    mutate(rank=as.numeric(factor(rank(total_score)))) 


pool_f %>% 
    left_join(current_scores) %>%
    left_join(day_scores) %>%
    arrange(total_score, name) %>%
    #select(name, day1_total) %>%
    distinct() %>%
    mutate(rank=as.numeric(factor(rank(total_score))))  

library(gt)    
#install.packages('gtsummary')
library(gtsummary)
library(webshot)
pool_f %>% 
    left_join(current_scores) %>%
    left_join(day_scores) %>%
    arrange(total_score, name) %>%
    #select(name, day1_total) %>%
    distinct() %>%
    mutate(rank=as.numeric(factor(rank(total_score))),
           name=str_trim(str_replace_all(name, "[^[:alnum:]]", " ")),
           name=paste0(name, " - RANK: ", rank, "; TOTAL SCORE: ", total_score))  %>%
    group_by(name) %>%
    unite(Players, c("tiers", "players"), sep=" - ") %>%
    select(entry_num, Players, r1, r2, r3, r4) %>%
    rename(`Entry #`=entry_num,
           R1=r1,
           R2=r2,
           R3=r3,
           R4=r4) %>%
    # rename(`Entry #`=entry_num,
    #        R1=r1,
    #        R2=r2,
    #        `TOTAL SCORE`=total_score,
    #        Rank=rank) %>%
    gt() %>%
    tab_header(title="Terry's Master's Pool") %>%
    gtsave("~/Desktop/terry_tourney_rd4_comp.pdf")


