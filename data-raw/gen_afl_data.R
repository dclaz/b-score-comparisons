pacman::p_load(
    #fitzRoy,
    tidyverse,
    janitor,
    lubridate,
    fst
)

# From https://www.aussportsbetting.com/historical_data/afl.xlsx
afl_results_df_raw <- readxl::read_excel(
    "data-raw/afl.xlsx",
    skip = 1
)

afl_results_df_raw <- afl_results_df_raw %>%
    janitor::clean_names()


match_outcome <- function(margin){
    (margin + 0.5) %>%
        pmin(1) %>%
        pmax(0)
}

afl_matches_df <- afl_results_df_raw %>%
    mutate(
        date = as.Date(date),
        date_time = paste0(date, format(kick_off_local, "%H:%M:%S")) %>%
            ymd_hms(),
        margin = home_score - away_score,
        outcome = match_outcome(margin)
    ) %>%
    select(
        date,
        date_time,
        home_team,
        away_team,
        venue,
        home_score,
        away_score,
        margin,
        outcome,
        final = play_off_game,
        home_odds,
        away_odds
    )




# save datas to correct location
usethis::use_data(afl_matches_df, overwrite = TRUE)





