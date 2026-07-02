# ============================================================
# COMBINE DETAIL CHUNKS - SAFE DNB VERSION
# Combines chunk artifacts, patches missing fields, logs edits,
# and rebuilds enriched JSON.
# ============================================================

library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(jsonlite)

log_line <- function(...) {
  cat(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste(..., collapse = " "), "\n"))
  flush.console()
}

MISSING_TOKENS <- c(
  "",
  "-",
  "NA",
  "N/A",
  "NAN",
  "NULL",
  "INF",
  "DNB",
  "TDNB",
  "DID NOT BAT",
  "DID NOT BOWL",
  "ABSENT",
  "ABSENT HURT",
  "RETIRED HURT",
  "RETIRED OUT",
  "SUB",
  "SUBSTITUTE",
  "NOT REQUIRED",
  "DID NOT FIELD"
)

is_missing_token <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_squish(x)
  x_upper <- str_to_upper(x)

  is.na(x) | x_upper %in% MISSING_TOKENS
}

safe_value <- function(x) {
  x <- as.character(x)

  if (length(x) == 0 || is.na(x) || is_missing_token(x)) {
    return("NA")
  }

  x
}

parse_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_squish(x)

  x[is_missing_token(x)] <- NA_character_

  x
}

parse_number <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_replace_all(x, ",", "")
  x <- str_replace_all(x, "\\*", "")
  x <- str_squish(x)

  x[is_missing_token(x)] <- NA_character_

  suppressWarnings(as.numeric(x))
}

parse_span_start <- function(span) {
  start <- str_extract(as.character(span), "^\\d{4}")
  suppressWarnings(as.integer(start))
}

parse_span_end <- function(span) {
  end <- str_extract(as.character(span), "\\d{4}$")
  suppressWarnings(as.integer(end))
}

parse_high_score <- function(hs) {
  hs_text <- parse_text(hs)

  score <- hs_text %>%
    str_replace_all("\\*", "") %>%
    parse_number()

  not_out <- ifelse(!is.na(hs_text) & str_detect(hs_text, "\\*"), TRUE, FALSE)

  list(score = score, not_out = not_out)
}

get_value <- function(row, candidates) {
  existing <- candidates[candidates %in% names(row)]

  if (length(existing) == 0) {
    return(NA)
  }

  row[[existing[1]]][1]
}

overs_to_balls <- function(overs_value) {
  overs_text <- parse_text(overs_value)

  if (is.na(overs_text)) {
    return(NA_real_)
  }

  if (!str_detect(overs_text, "\\.")) {
    return(parse_number(overs_text) * 6)
  }

  parts <- str_split(overs_text, "\\.", simplify = TRUE)

  whole_overs <- suppressWarnings(as.numeric(parts[1]))
  balls <- suppressWarnings(as.numeric(parts[2]))

  if (is.na(whole_overs)) {
    whole_overs <- 0
  }

  if (is.na(balls)) {
    balls <- 0
  }

  whole_overs * 6 + balls
}

balls_to_overs_text <- function(balls_value) {
  balls <- parse_number(balls_value)

  if (is.na(balls)) {
    return(NA_character_)
  }

  whole <- floor(balls / 6)
  rem <- balls %% 6

  paste0(whole, ".", rem)
}

values_different <- function(old, new) {
  safe_value(old) != safe_value(new)
}

log_field_edits <- function(before_df, after_df, stat_type, fields_to_check) {
  edit_count <- 0
  rows_to_check <- min(nrow(before_df), nrow(after_df))

  for (i in seq_len(rows_to_check)) {
    player_name <- safe_value(after_df$final_player_name[i])
    player_id <- safe_value(after_df$unique_player_id[i])
    fmt <- safe_value(after_df$format[i])

    for (field in fields_to_check) {
      if (!field %in% names(before_df) || !field %in% names(after_df)) {
        next
      }

      old_value <- before_df[[field]][i]
      new_value <- after_df[[field]][i]

      if (values_different(old_value, new_value)) {
        edit_count <- edit_count + 1

        log_line(
          "EDIT",
          player_name,
          "| ID:", player_id,
          "|", fmt,
          "|", stat_type,
          "|", paste0(field, ":"),
          safe_value(old_value),
          "->",
          safe_value(new_value)
        )
      }
    }
  }

  log_line("Total edits for", stat_type, ":", edit_count)
}

dir.create("outputs", showWarnings = FALSE)

base_dirs <- list.dirs("downloaded_artifacts", recursive = TRUE, full.names = TRUE)

base_batting_path <- base_dirs[file.exists(file.path(base_dirs, "outputs/all_international_batting.csv"))][1]
base_bowling_path <- base_dirs[file.exists(file.path(base_dirs, "outputs/all_international_bowling.csv"))][1]
base_fielding_path <- base_dirs[file.exists(file.path(base_dirs, "outputs/all_international_fielding.csv"))][1]

if (is.na(base_batting_path)) {
  stop("Cannot find base all_international_batting.csv")
}

if (is.na(base_bowling_path)) {
  stop("Cannot find base all_international_bowling.csv")
}

if (is.na(base_fielding_path)) {
  stop("Cannot find base all_international_fielding.csv")
}

file.copy(file.path(base_batting_path, "outputs"), ".", recursive = TRUE, overwrite = TRUE)

all_batting <- read_csv(
  "outputs/all_international_batting.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

all_bowling <- read_csv(
  "outputs/all_international_bowling.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

all_fielding <- read_csv(
  "outputs/all_international_fielding.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

summary_files <- list.files(
  "downloaded_artifacts",
  pattern = "_summary\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

log_line("Found summary chunk files:", length(summary_files))

all_summaries <- map(
  summary_files,
  ~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = col_character()))
)

batting_summaries <- all_summaries[
  map_lgl(
    all_summaries,
    ~ all(c("DetailBallsFaced", "DetailFours", "DetailSixes") %in% names(.x))
  )
]

bowling_summaries <- all_summaries[
  map_lgl(
    all_summaries,
    ~ all(c("DetailBalls", "DetailOvers", "DetailMaidens", "DetailRunsConceded") %in% names(.x))
  )
]

if (length(batting_summaries) > 0) {
  batting_enrichment <- bind_rows(batting_summaries) %>%
    mutate(
      HasBattingDetail = as.logical(HasBattingDetail),
      DetailBallsFaced = parse_number(DetailBallsFaced),
      DetailFours = parse_number(DetailFours),
      DetailSixes = parse_number(DetailSixes)
    ) %>%
    group_by(unique_player_id, format) %>%
    summarise(
      HasBattingDetail = any(HasBattingDetail, na.rm = TRUE),
      DetailBallsFaced = sum(DetailBallsFaced, na.rm = TRUE),
      DetailFours = sum(DetailFours, na.rm = TRUE),
      DetailSixes = sum(DetailSixes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      DetailBallsFaced = ifelse(HasBattingDetail, DetailBallsFaced, NA),
      DetailFours = ifelse(HasBattingDetail, DetailFours, NA),
      DetailSixes = ifelse(HasBattingDetail, DetailSixes, NA)
    )
} else {
  batting_enrichment <- tibble(
    unique_player_id = character(),
    format = character(),
    HasBattingDetail = logical(),
    DetailBallsFaced = numeric(),
    DetailFours = numeric(),
    DetailSixes = numeric()
  )
}

if (length(bowling_summaries) > 0) {
  bowling_enrichment <- bind_rows(bowling_summaries) %>%
    mutate(
      HasBowlingDetail = as.logical(HasBowlingDetail),
      DetailBalls = parse_number(DetailBalls),
      DetailMaidens = parse_number(DetailMaidens),
      DetailRunsConceded = parse_number(DetailRunsConceded)
    ) %>%
    group_by(unique_player_id, format) %>%
    summarise(
      HasBowlingDetail = any(HasBowlingDetail, na.rm = TRUE),
      DetailBalls = sum(DetailBalls, na.rm = TRUE),
      DetailMaidens = sum(DetailMaidens, na.rm = TRUE),
      DetailRunsConceded = sum(DetailRunsConceded, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      DetailBalls = ifelse(HasBowlingDetail, DetailBalls, NA),
      DetailOvers = map_chr(DetailBalls, balls_to_overs_text),
      DetailMaidens = ifelse(HasBowlingDetail, DetailMaidens, NA),
      DetailRunsConceded = ifelse(HasBowlingDetail, DetailRunsConceded, NA)
    )
} else {
  bowling_enrichment <- tibble(
    unique_player_id = character(),
    format = character(),
    HasBowlingDetail = logical(),
    DetailBalls = numeric(),
    DetailOvers = character(),
    DetailMaidens = numeric(),
    DetailRunsConceded = numeric()
  )
}

write_csv(batting_enrichment, "outputs/batting_missing_fields_enrichment_combined.csv")
write_csv(bowling_enrichment, "outputs/bowling_missing_fields_enrichment_combined.csv")

for (needed_col in c("BF", "SR", "4s", "6s", "Runs")) {
  if (!needed_col %in% names(all_batting)) {
    all_batting[[needed_col]] <- NA_character_
  }
}

for (needed_col in c("Balls", "Overs", "Mdns", "Runs")) {
  if (!needed_col %in% names(all_bowling)) {
    all_bowling[[needed_col]] <- NA_character_
  }
}

all_batting_before <- all_batting

all_batting_enriched <- all_batting %>%
  left_join(batting_enrichment, by = c("unique_player_id", "format")) %>%
  mutate(
    BF = ifelse(is_missing_token(BF), as.character(DetailBallsFaced), as.character(BF)),
    `4s` = ifelse(is_missing_token(`4s`), as.character(DetailFours), as.character(`4s`)),
    `6s` = ifelse(is_missing_token(`6s`), as.character(DetailSixes), as.character(`6s`)),
    SR = ifelse(
      is_missing_token(SR) &
        !is.na(parse_number(Runs)) &
        !is.na(parse_number(BF)) &
        parse_number(BF) > 0,
      as.character(round(parse_number(Runs) / parse_number(BF) * 100, 2)),
      as.character(SR)
    )
  ) %>%
  select(-HasBattingDetail, -DetailBallsFaced, -DetailFours, -DetailSixes)

log_field_edits(
  all_batting_before,
  all_batting_enriched,
  "batting",
  c("BF", "SR", "4s", "6s")
)

all_bowling_before <- all_bowling

all_bowling_enriched <- all_bowling %>%
  left_join(bowling_enrichment, by = c("unique_player_id", "format")) %>%
  mutate(
    Balls = ifelse(is_missing_token(Balls), as.character(DetailBalls), as.character(Balls)),
    Overs = ifelse(is_missing_token(Overs), as.character(DetailOvers), as.character(Overs)),
    Mdns = ifelse(is_missing_token(Mdns), as.character(DetailMaidens), as.character(Mdns)),
    Runs = ifelse(is_missing_token(Runs), as.character(DetailRunsConceded), as.character(Runs))
  ) %>%
  select(-HasBowlingDetail, -DetailBalls, -DetailOvers, -DetailMaidens, -DetailRunsConceded)

log_field_edits(
  all_bowling_before,
  all_bowling_enriched,
  "bowling",
  c("Balls", "Overs", "Mdns", "Runs")
)

write_csv(all_batting_enriched, "outputs/all_international_batting_enriched.csv")
write_csv(all_bowling_enriched, "outputs/all_international_bowling_enriched.csv")

player_index <- bind_rows(
  all_batting_enriched %>% select(cricinfo_id, unique_player_id, final_player_name, source_country_text),
  all_bowling_enriched %>% select(cricinfo_id, unique_player_id, final_player_name, source_country_text),
  all_fielding %>% select(cricinfo_id, unique_player_id, final_player_name, source_country_text)
) %>%
  distinct() %>%
  arrange(final_player_name)

write_csv(player_index, "outputs/player_index_enriched.csv")

row_to_batting <- function(row) {
  hs <- parse_high_score(get_value(row, c("HS")))

  list(
    Span = parse_text(get_value(row, c("Span"))),
    Start = parse_span_start(get_value(row, c("Span"))),
    End = parse_span_end(get_value(row, c("Span"))),
    Matches = parse_number(get_value(row, c("Mat"))),
    Innings = parse_number(get_value(row, c("Inns"))),
    NotOuts = parse_number(get_value(row, c("NO"))),
    Runs = parse_number(get_value(row, c("Runs"))),
    HighScore = hs$score,
    HighScoreNotOut = hs$not_out,
    Average = parse_number(get_value(row, c("Ave"))),
    BallsFaced = parse_number(get_value(row, c("BF"))),
    StrikeRate = parse_number(get_value(row, c("SR"))),
    Hundreds = parse_number(get_value(row, c("100"))),
    Fifties = parse_number(get_value(row, c("50"))),
    Ducks = parse_number(get_value(row, c("0"))),
    Fours = parse_number(get_value(row, c("4s"))),
    Sixes = parse_number(get_value(row, c("6s")))
  )
}

row_to_bowling <- function(row) {
  raw_balls <- parse_number(get_value(row, c("Balls")))
  raw_overs <- parse_text(get_value(row, c("Overs")))

  calculated_balls <- ifelse(is.na(raw_balls), overs_to_balls(raw_overs), raw_balls)
  calculated_overs <- ifelse(is.na(raw_overs), balls_to_overs_text(calculated_balls), raw_overs)

  list(
    Span = parse_text(get_value(row, c("Span"))),
    Start = parse_span_start(get_value(row, c("Span"))),
    End = parse_span_end(get_value(row, c("Span"))),
    Matches = parse_number(get_value(row, c("Mat"))),
    Innings = parse_number(get_value(row, c("Inns"))),
    Balls = calculated_balls,
    Overs = calculated_overs,
    Maidens = parse_number(get_value(row, c("Mdns"))),
    RunsConceded = parse_number(get_value(row, c("Runs"))),
    Wickets = parse_number(get_value(row, c("Wkts"))),
    BestBowlingInnings = parse_text(get_value(row, c("BBI"))),
    BestBowlingMatch = parse_text(get_value(row, c("BBM"))),
    Average = parse_number(get_value(row, c("Ave"))),
    Economy = parse_number(get_value(row, c("Econ"))),
    StrikeRate = parse_number(get_value(row, c("SR"))),
    FourWickets = parse_number(get_value(row, c("4"))),
    FiveWickets = parse_number(get_value(row, c("5"))),
    TenWickets = parse_number(get_value(row, c("10")))
  )
}

row_to_fielding <- function(row) {
  list(
    Span = parse_text(get_value(row, c("Span"))),
    Start = parse_span_start(get_value(row, c("Span"))),
    End = parse_span_end(get_value(row, c("Span"))),
    Matches = parse_number(get_value(row, c("Mat"))),
    Innings = parse_number(get_value(row, c("Inns"))),
    Dismissals = parse_number(get_value(row, c("Dis"))),
    Caught = parse_number(get_value(row, c("Ct"))),
    Stumped = parse_number(get_value(row, c("St"))),
    CaughtBehind = parse_number(get_value(row, c("Ct Wk"))),
    CaughtFielder = parse_number(get_value(row, c("Ct Fi"))),
    MaxDismissalsInnings = parse_number(get_value(row, c("MD"))),
    DismissalsPerInnings = parse_number(get_value(row, c("D/I")))
  )
}

empty_batting_stats <- function() {
  list(
    Span = NA_character_,
    Start = NA_integer_,
    End = NA_integer_,
    Matches = NA_real_,
    Innings = NA_real_,
    NotOuts = NA_real_,
    Runs = NA_real_,
    HighScore = NA_real_,
    HighScoreNotOut = NA,
    Average = NA_real_,
    BallsFaced = NA_real_,
    StrikeRate = NA_real_,
    Hundreds = NA_real_,
    Fifties = NA_real_,
    Ducks = NA_real_,
    Fours = NA_real_,
    Sixes = NA_real_
  )
}

empty_bowling_stats <- function() {
  list(
    Span = NA_character_,
    Start = NA_integer_,
    End = NA_integer_,
    Matches = NA_real_,
    Innings = NA_real_,
    Balls = NA_real_,
    Overs = NA_character_,
    Maidens = NA_real_,
    RunsConceded = NA_real_,
    Wickets = NA_real_,
    BestBowlingInnings = NA_character_,
    BestBowlingMatch = NA_character_,
    Average = NA_real_,
    Economy = NA_real_,
    StrikeRate = NA_real_,
    FourWickets = NA_real_,
    FiveWickets = NA_real_,
    TenWickets = NA_real_
  )
}

empty_fielding_stats <- function() {
  list(
    Span = NA_character_,
    Start = NA_integer_,
    End = NA_integer_,
    Matches = NA_real_,
    Innings = NA_real_,
    Dismissals = NA_real_,
    Caught = NA_real_,
    Stumped = NA_real_,
    CaughtBehind = NA_real_,
    CaughtFielder = NA_real_,
    MaxDismissalsInnings = NA_real_,
    DismissalsPerInnings = NA_real_
  )
}

empty_format_group <- function(type) {
  if (type == "batting") {
    return(list(test = empty_batting_stats(), odi = empty_batting_stats(), t20i = empty_batting_stats()))
  }

  if (type == "bowling") {
    return(list(test = empty_bowling_stats(), odi = empty_bowling_stats(), t20i = empty_bowling_stats()))
  }

  if (type == "fielding") {
    return(list(test = empty_fielding_stats(), odi = empty_fielding_stats(), t20i = empty_fielding_stats()))
  }
}

all_player_ids <- player_index %>%
  filter(!is.na(unique_player_id), unique_player_id != "") %>%
  pull(unique_player_id) %>%
  unique() %>%
  sort()

nested_stats <- list()

for (pid in all_player_ids) {
  player_meta <- player_index %>%
    filter(unique_player_id == pid) %>%
    slice(1)

  player_obj <- list(
    player_info = list(
      unique_player_id = player_meta$unique_player_id,
      cricinfo_id = player_meta$cricinfo_id,
      final_player_name = player_meta$final_player_name,
      final_country = player_meta$source_country_text,
      source = "espncricinfo_statsguru_enriched"
    ),
    batting = empty_format_group("batting"),
    bowling = empty_format_group("bowling"),
    fielding = empty_format_group("fielding")
  )

  for (fmt in c("test", "odi", "t20i")) {
    batting_row <- all_batting_enriched %>%
      filter(unique_player_id == pid, format == fmt) %>%
      slice(1)

    if (nrow(batting_row) == 1) {
      player_obj$batting[[fmt]] <- row_to_batting(batting_row)
    }

    bowling_row <- all_bowling_enriched %>%
      filter(unique_player_id == pid, format == fmt) %>%
      slice(1)

    if (nrow(bowling_row) == 1) {
      player_obj$bowling[[fmt]] <- row_to_bowling(bowling_row)
    }

    fielding_row <- all_fielding %>%
      filter(unique_player_id == pid, format == fmt) %>%
      slice(1)

    if (nrow(fielding_row) == 1) {
      player_obj$fielding[[fmt]] <- row_to_fielding(fielding_row)
    }
  }

  nested_stats[[pid]] <- player_obj
}

write_json(
  nested_stats,
  "outputs/all_international_stats_enriched.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

log_line("Saved JSON:", "outputs/all_international_stats_enriched.json")