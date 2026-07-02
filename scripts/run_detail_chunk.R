# ============================================================
# RUN DETAIL CHUNK - SAFE DNB VERSION
# Scrapes one target + page range for GitHub Actions
#
# Env vars:
# DETAIL_TARGET = test_batting / odi_batting / test_bowling / odi_bowling
# PAGE_START = 1
# PAGE_END = 100
# CHUNK_LABEL = chunk_001_100
# ============================================================

library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(jsonlite)
library(httr)

BASE_URL <- "https://stats.espncricinfo.com/ci/engine/stats/index.html"

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/detail_chunks", showWarnings = FALSE)

log_line <- function(...) {
  cat(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste(..., collapse = " "), "\n"))
  flush.console()
}

safe_env <- function(name, default = NA_character_) {
  value <- Sys.getenv(name)
  if (is.na(value) || value == "") default else value
}

DETAIL_TARGET <- safe_env("DETAIL_TARGET")
PAGE_START <- as.integer(safe_env("PAGE_START", "1"))
PAGE_END <- as.integer(safe_env("PAGE_END", "100"))
CHUNK_LABEL <- safe_env("CHUNK_LABEL", paste0("chunk_", PAGE_START, "_", PAGE_END))

if (is.na(DETAIL_TARGET) || DETAIL_TARGET == "") {
  stop("Missing DETAIL_TARGET")
}

target_info <- tribble(
  ~target,        ~format, ~class_id, ~stat_type, ~view,      ~orderby,
  "test_batting", "test",  1,         "batting",  "innings", "balls_faced",
  "odi_batting",  "odi",   2,         "batting",  "innings", "balls_faced",
  "test_bowling", "test",  1,         "bowling",  "innings", "overs",
  "odi_bowling",  "odi",   2,         "bowling",  "innings", "overs"
)

target_row <- target_info %>%
  filter(target == DETAIL_TARGET) %>%
  slice(1)

if (nrow(target_row) != 1) {
  stop(paste("Unknown DETAIL_TARGET:", DETAIL_TARGET))
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

fetch_html <- function(url) {
  response <- httr::GET(
    url,
    httr::user_agent("Mozilla/5.0 AppleWebKit/605.1.15 Safari/605.1.15")
  )

  status <- httr::status_code(response)

  if (status >= 400) {
    stop(paste("Request failed:", status, url))
  }

  read_html(httr::content(response, as = "text", encoding = "UTF-8"))
}

clean_colnames <- function(df) {
  nms <- names(df)

  nms <- nms %>%
    str_replace_all("\u00a0", " ") %>%
    str_squish()

  blank_names <- is.na(nms) | nms == ""

  if (any(blank_names)) {
    nms[blank_names] <- paste0("blank_col_", which(blank_names))
  }

  names(df) <- make.unique(nms, sep = "_")
  df
}

clean_player_name <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\u00a0", " ") %>%
    str_squish() %>%
    str_remove("\\s*\\([^)]*\\)$") %>%
    str_squish()
}

extract_country_text <- function(x) {
  country <- str_match(as.character(x), "\\(([^)]*)\\)\\s*$")[, 2]
  ifelse(is.na(country), NA_character_, country)
}

extract_cricinfo_id_from_href <- function(href) {
  id1 <- str_extract(href, "(?<=/player/)\\d+(?=\\.html)")
  id2 <- str_extract(href, "(?<=-)\\d+$")
  ifelse(!is.na(id1), id1, id2)
}

make_slug <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

has_next_page <- function(html) {
  link_texts <- html %>%
    html_elements("a") %>%
    html_text2()

  any(str_detect(str_to_lower(link_texts), "^next$|next"))
}

build_detail_url <- function(class_id, stat_type, view, orderby, page = 1) {
  paste0(
    BASE_URL,
    "?class=", class_id,
    ";filter=advanced",
    ";orderby=", orderby,
    ";page=", page,
    ";size=200",
    ";template=results",
    ";type=", stat_type,
    ";view=", view
  )
}

read_detail_page <- function(url) {
  html <- fetch_html(url)

  table_nodes <- html %>% html_elements("table.engineTable")

  if (length(table_nodes) == 0) {
    return(list(data = tibble(), has_next = FALSE))
  }

  tables <- table_nodes %>%
    map(~ html_table(.x, fill = TRUE))

  table_index <- which(
    map_lgl(
      tables,
      function(tbl) {
        tbl <- as.data.frame(tbl, check.names = FALSE)
        tbl <- clean_colnames(tbl)
        "Player" %in% names(tbl)
      }
    )
  )[1]

  if (is.na(table_index)) {
    return(list(data = tibble(), has_next = has_next_page(html)))
  }

  df <- tables[[table_index]] %>%
    as.data.frame(check.names = FALSE) %>%
    clean_colnames() %>%
    as_tibble(.name_repair = "unique") %>%
    filter(!is.na(Player), Player != "", Player != "Player")

  row_nodes <- table_nodes[[table_index]] %>%
    html_elements("tr")

  data_rows <- row_nodes[
    map_int(row_nodes, ~ length(html_elements(.x, "td"))) > 0
  ]

  player_texts <- data_rows %>%
    html_element("td:nth-child(1)") %>%
    html_text2()

  hrefs <- data_rows %>%
    html_element("td:nth-child(1) a") %>%
    html_attr("href")

  valid_rows <- !is.na(player_texts) &
    player_texts != "" &
    player_texts != "Player"

  hrefs <- hrefs[valid_rows]
  cricinfo_ids <- extract_cricinfo_id_from_href(hrefs)

  if (length(cricinfo_ids) >= nrow(df)) {
    cricinfo_ids <- cricinfo_ids[seq_len(nrow(df))]
  } else {
    cricinfo_ids <- c(cricinfo_ids, rep(NA_character_, nrow(df) - length(cricinfo_ids)))
  }

  df <- df %>%
    mutate(
      cricinfo_id = cricinfo_ids,
      unique_player_id = ifelse(
        !is.na(cricinfo_id) & cricinfo_id != "",
        cricinfo_id,
        paste0(
          "missing_",
          make_slug(clean_player_name(Player)),
          "_",
          make_slug(extract_country_text(Player))
        )
      ),
      final_player_name = clean_player_name(Player),
      source_country_text = extract_country_text(Player),
      .before = Player
    ) %>%
    mutate(across(everything(), as.character))

  list(data = df, has_next = has_next_page(html))
}

all_pages <- list()

log_line("TARGET:", DETAIL_TARGET)
log_line("FORMAT:", target_row$format)
log_line("STAT_TYPE:", target_row$stat_type)
log_line("PAGES:", PAGE_START, "to", PAGE_END)

for (page in PAGE_START:PAGE_END) {
  url <- build_detail_url(
    class_id = target_row$class_id,
    stat_type = target_row$stat_type,
    view = target_row$view,
    orderby = target_row$orderby,
    page = page
  )

  log_line("Reading:", DETAIL_TARGET, "page", page)

  page_result <- read_detail_page(url)
  df <- page_result$data

  if (nrow(df) == 0) {
    log_line("No rows at page", page, "- stopping this chunk.")
    break
  }

  df <- df %>%
    mutate(
      format = target_row$format,
      stat_type = target_row$stat_type,
      detail_view = target_row$view,
      page = as.character(page),
      source_url = url
    )

  all_pages[[as.character(page)]] <- df

  if (!page_result$has_next) {
    log_line("No next page at", page, "- stopping this chunk.")
    break
  }

  Sys.sleep(1.5)
}

raw <- bind_rows(all_pages)

raw_file <- paste0("outputs/detail_chunks/", DETAIL_TARGET, "_", CHUNK_LABEL, "_raw.csv")
write_csv(raw, raw_file)

log_line("Saved raw:", raw_file, "| Rows:", nrow(raw))

if (nrow(raw) == 0) {
  if (target_row$stat_type == "batting") {
    empty_summary <- tibble(
      unique_player_id = character(),
      format = character(),
      HasBattingDetail = logical(),
      DetailBallsFaced = numeric(),
      DetailFours = numeric(),
      DetailSixes = numeric()
    )
  } else {
    empty_summary <- tibble(
      unique_player_id = character(),
      format = character(),
      HasBowlingDetail = logical(),
      DetailBalls = numeric(),
      DetailOvers = character(),
      DetailMaidens = numeric(),
      DetailRunsConceded = numeric()
    )
  }

  summary_file <- paste0("outputs/detail_chunks/", DETAIL_TARGET, "_", CHUNK_LABEL, "_summary.csv")
  write_csv(empty_summary, summary_file)
  log_line("Saved empty summary:", summary_file)
  quit(save = "no", status = 0)
}

if (target_row$stat_type == "batting") {
  for (needed_col in c("BF", "4s", "6s")) {
    if (!needed_col %in% names(raw)) {
      raw[[needed_col]] <- NA_character_
    }
  }

  summary <- raw %>%
    mutate(
      ParsedBF = parse_number(BF),
      ParsedFours = parse_number(`4s`),
      ParsedSixes = parse_number(`6s`)
    ) %>%
    group_by(unique_player_id, format) %>%
    summarise(
      HasBattingDetail = any(!is.na(ParsedBF) | !is.na(ParsedFours) | !is.na(ParsedSixes)),
      DetailBallsFaced = sum(ParsedBF, na.rm = TRUE),
      DetailFours = sum(ParsedFours, na.rm = TRUE),
      DetailSixes = sum(ParsedSixes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      DetailBallsFaced = ifelse(HasBattingDetail, DetailBallsFaced, NA),
      DetailFours = ifelse(HasBattingDetail, DetailFours, NA),
      DetailSixes = ifelse(HasBattingDetail, DetailSixes, NA)
    )
}

if (target_row$stat_type == "bowling") {
  for (needed_col in c("Overs", "Mdns", "Runs")) {
    if (!needed_col %in% names(raw)) {
      raw[[needed_col]] <- NA_character_
    }
  }

  summary <- raw %>%
    mutate(
      ParsedOvers = parse_text(Overs),
      ParsedBalls = map_dbl(ParsedOvers, overs_to_balls),
      ParsedMdns = parse_number(Mdns),
      ParsedRuns = parse_number(Runs)
    ) %>%
    group_by(unique_player_id, format) %>%
    summarise(
      HasBowlingDetail = any(!is.na(ParsedBalls) | !is.na(ParsedMdns) | !is.na(ParsedRuns)),
      DetailBalls = sum(ParsedBalls, na.rm = TRUE),
      DetailMaidens = sum(ParsedMdns, na.rm = TRUE),
      DetailRunsConceded = sum(ParsedRuns, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      DetailBalls = ifelse(HasBowlingDetail, DetailBalls, NA),
      DetailOvers = map_chr(DetailBalls, balls_to_overs_text),
      DetailMaidens = ifelse(HasBowlingDetail, DetailMaidens, NA),
      DetailRunsConceded = ifelse(HasBowlingDetail, DetailRunsConceded, NA)
    )
}

summary_file <- paste0("outputs/detail_chunks/", DETAIL_TARGET, "_", CHUNK_LABEL, "_summary.csv")
write_csv(summary, summary_file)

log_line("Saved summary:", summary_file, "| Rows:", nrow(summary))
log_line("DONE:", DETAIL_TARGET, CHUNK_LABEL)