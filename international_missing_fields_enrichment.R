# ============================================================
# INTERNATIONAL MISSING FIELDS ENRICHMENT - WITH EDIT LOGGING
# Source: ESPNcricinfo Statsguru
#
# Run after:
# Rscript international_stats_pipeline.R
#
# Purpose:
# - Fill missing batting fields:
#   Test: BF, SR, 4s, 6s
#   ODI: 4s, 6s
#
# - Fill missing bowling fields:
#   Test: Overs, Mdns if possible
#   ODI: Overs, Mdns if possible
#
# - Terminal shows actual field edits:
#   EDIT Sachin Tendulkar | ID: 35320 | test | batting | BF: NA -> 29437
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "rvest",
  "dplyr",
  "purrr",
  "stringr",
  "tibble",
  "readr",
  "jsonlite",
  "httr"
)

installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!pkg %in% installed) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(jsonlite)
library(httr)


# ============================================================
# 1. TERMINAL LOGGING SETTINGS
# ============================================================

LOG_EVERY_EDIT <- TRUE

# Set to Inf for every edit.
# Set to 500 or 1000 if terminal gets too crowded.
MAX_EDIT_LOG <- Inf

BASE_URL <- "https://stats.espncricinfo.com/ci/engine/stats/index.html"

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/enrichment_logs", showWarnings = FALSE)

log_line <- function(...) {
  cat(
    paste0(
      "[",
      format(Sys.time(), "%H:%M:%S"),
      "] ",
      paste(..., collapse = " "),
      "\n"
    )
  )
  flush.console()
}

log_section <- function(title) {
  cat("\n============================================================\n")
  cat(title, "\n")
  cat("============================================================\n")
  flush.console()
}

safe_value <- function(x) {
  x <- as.character(x)

  if (length(x) == 0 || is.na(x) || x == "" || x == "NA" || x == "null") {
    return("NA")
  }

  x
}

values_different <- function(old, new) {
  old <- safe_value(old)
  new <- safe_value(new)

  old != new
}

log_field_edits <- function(before_df, after_df, stat_type, fields_to_check) {
  if (!LOG_EVERY_EDIT) {
    return(invisible(NULL))
  }

  edit_count <- 0

  if (nrow(before_df) != nrow(after_df)) {
    log_line(
      "WARNING:",
      stat_type,
      "before/after row counts differ:",
      nrow(before_df),
      "vs",
      nrow(after_df)
    )
  }

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

        if (edit_count <= MAX_EDIT_LOG) {
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
  }

  if (edit_count > MAX_EDIT_LOG) {
    log_line(
      "Edit log capped for",
      stat_type,
      "| Displayed:",
      MAX_EDIT_LOG,
      "| Total edits:",
      edit_count
    )
  } else {
    log_line("Total edits logged for", stat_type, ":", edit_count)
  }

  invisible(edit_count)
}


# ============================================================
# 2. FORMATS
# ============================================================

formats <- tribble(
  ~format_key, ~class_id,
  "test",      1,
  "odi",       2,
  "t20i",      3
)


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

fetch_html <- function(url) {
  response <- httr::GET(
    url,
    httr::user_agent(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 Safari/605.1.15"
    )
  )

  status <- httr::status_code(response)

  if (status >= 400) {
    stop(paste("Request failed:", status, url))
  }

  html_text <- httr::content(response, as = "text", encoding = "UTF-8")

  read_html(html_text)
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


parse_number <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, ",", "")
  x <- str_replace_all(x, "\\*", "")
  x <- str_squish(x)

  x[x %in% c("", "-", "NA", "NaN", "Inf", "null")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


parse_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_squish(x)

  x[x %in% c("", "-", "NA", "null")] <- NA_character_

  x
}


parse_span_start <- function(span) {
  span <- as.character(span)
  start <- str_extract(span, "^\\d{4}")
  suppressWarnings(as.integer(start))
}


parse_span_end <- function(span) {
  span <- as.character(span)
  end <- str_extract(span, "\\d{4}$")
  suppressWarnings(as.integer(end))
}


parse_high_score <- function(hs) {
  hs_text <- parse_text(hs)

  score <- hs_text %>%
    str_replace_all("\\*", "") %>%
    parse_number()

  not_out <- ifelse(
    !is.na(hs_text) & str_detect(hs_text, "\\*"),
    TRUE,
    FALSE
  )

  list(
    score = score,
    not_out = not_out
  )
}


get_value <- function(row, candidates) {
  existing <- candidates[candidates %in% names(row)]

  if (length(existing) == 0) {
    return(NA)
  }

  row[[existing[1]]][1]
}


has_next_page <- function(html) {
  link_texts <- html %>%
    html_elements("a") %>%
    html_text2()

  any(str_detect(str_to_lower(link_texts), "^next$|next"))
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


# ============================================================
# 4. READ ONE DETAIL PAGE
# ============================================================

read_detail_page <- function(url) {
  html <- fetch_html(url)

  table_nodes <- html %>% html_elements("table.engineTable")

  if (length(table_nodes) == 0) {
    return(list(
      data = tibble(),
      has_next = FALSE
    ))
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
    return(list(
      data = tibble(),
      has_next = has_next_page(html)
    ))
  }

  df <- tables[[table_index]] %>%
    as.data.frame(check.names = FALSE) %>%
    clean_colnames() %>%
    as_tibble(.name_repair = "unique")

  df <- df %>%
    filter(
      !is.na(Player),
      Player != "",
      Player != "Player"
    )

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
    cricinfo_ids <- c(
      cricinfo_ids,
      rep(NA_character_, nrow(df) - length(cricinfo_ids))
    )
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

  list(
    data = df,
    has_next = has_next_page(html)
  )
}


# ============================================================
# 5. READ ALL DETAIL PAGES
# ============================================================

get_all_detail_pages <- function(
  format_key,
  class_id,
  stat_type,
  view,
  orderby,
  max_pages = 575
) {
  all_pages <- list()
  seen_signatures <- character()

  for (page in 1:max_pages) {
    url <- build_detail_url(
      class_id = class_id,
      stat_type = stat_type,
      view = view,
      orderby = orderby,
      page = page
    )

    log_line("Reading detail:", format_key, stat_type, view, "page", page)

    page_result <- read_detail_page(url)
    df <- page_result$data

    if (nrow(df) == 0) {
      log_line("No rows. Stopping:", format_key, stat_type, view)
      break
    }

    df <- df %>%
      mutate(
        format = format_key,
        stat_type = stat_type,
        detail_view = view,
        source_url = url
      )

    # Prevent repeated-page loops.
    signature <- paste(
      paste(head(df$unique_player_id, 10), collapse = "_"),
      nrow(df),
      sep = "|"
    )

    if (page > 1 && signature %in% seen_signatures) {
      log_line("Repeated page detected. Stopping:", format_key, stat_type, view)
      break
    }

    seen_signatures <- unique(c(seen_signatures, signature))
    all_pages[[page]] <- df

    if (!page_result$has_next) {
      log_line("No next page. Finished:", format_key, stat_type, view)
      break
    }

    Sys.sleep(1.5)
  }

  bind_rows(all_pages)
}


# ============================================================
# 6. BATTING DETAIL ENRICHMENT
# ============================================================

log_section("BATTING DETAIL ENRICHMENT")

batting_detail_all <- list()

for (i in seq_len(nrow(formats))) {
  fmt <- formats$format_key[i]
  class_id <- formats$class_id[i]

  # Needed:
  # Test: BF, SR, 4s, 6s
  # ODI: 4s, 6s
  # T20I already has these in summary, so skip for speed.
  if (!fmt %in% c("test", "odi")) {
    next
  }

  batting_detail_all[[fmt]] <- get_all_detail_pages(
    format_key = fmt,
    class_id = class_id,
    stat_type = "batting",
    view = "innings",
    orderby = "balls_faced"
  )
}

batting_detail <- bind_rows(batting_detail_all)

write_csv(
  batting_detail,
  "outputs/batting_innings_detail_raw.csv"
)

log_line(
  "Saved batting detail raw:",
  "outputs/batting_innings_detail_raw.csv",
  "| Rows:",
  nrow(batting_detail)
)

# Make sure missing detail fields exist before aggregation.
for (needed_col in c("BF", "4s", "6s", "Runs")) {
  if (!needed_col %in% names(batting_detail)) {
    batting_detail[[needed_col]] <- NA_character_
  }
}

batting_enrichment <- batting_detail %>%
  group_by(unique_player_id, format) %>%
  summarise(
    DetailBallsFaced = sum(parse_number(BF), na.rm = TRUE),
    DetailFours = sum(parse_number(`4s`), na.rm = TRUE),
    DetailSixes = sum(parse_number(`6s`), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DetailBallsFaced = ifelse(DetailBallsFaced == 0, NA, DetailBallsFaced),
    DetailFours = ifelse(DetailFours == 0, NA, DetailFours),
    DetailSixes = ifelse(DetailSixes == 0, NA, DetailSixes)
  )

write_csv(
  batting_enrichment,
  "outputs/batting_missing_fields_enrichment.csv"
)

log_line(
  "Saved batting enrichment:",
  "outputs/batting_missing_fields_enrichment.csv",
  "| Rows:",
  nrow(batting_enrichment)
)


# ============================================================
# 7. BOWLING DETAIL ENRICHMENT
# ============================================================

log_section("BOWLING DETAIL ENRICHMENT")

bowling_detail_all <- list()

for (i in seq_len(nrow(formats))) {
  fmt <- formats$format_key[i]
  class_id <- formats$class_id[i]

  # Needed:
  # Test: Mdns
  # ODI: Mdns
  # T20I has Overs/Mdns in summary.
  if (!fmt %in% c("test", "odi")) {
    next
  }

  bowling_detail_all[[fmt]] <- get_all_detail_pages(
    format_key = fmt,
    class_id = class_id,
    stat_type = "bowling",
    view = "innings",
    orderby = "overs"
  )
}

bowling_detail <- bind_rows(bowling_detail_all)

write_csv(
  bowling_detail,
  "outputs/bowling_innings_detail_raw.csv"
)

log_line(
  "Saved bowling detail raw:",
  "outputs/bowling_innings_detail_raw.csv",
  "| Rows:",
  nrow(bowling_detail)
)

# Make sure missing detail fields exist before aggregation.
for (needed_col in c("Overs", "Mdns", "Runs")) {
  if (!needed_col %in% names(bowling_detail)) {
    bowling_detail[[needed_col]] <- NA_character_
  }
}

bowling_enrichment <- bowling_detail %>%
  mutate(
    DetailOversText = Overs,
    DetailBalls = map_dbl(DetailOversText, overs_to_balls),
    DetailMaidens = parse_number(Mdns),
    DetailRunsConceded = parse_number(Runs)
  ) %>%
  group_by(unique_player_id, format) %>%
  summarise(
    DetailBalls = sum(DetailBalls, na.rm = TRUE),
    DetailMaidens = sum(DetailMaidens, na.rm = TRUE),
    DetailRunsConceded = sum(DetailRunsConceded, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DetailBalls = ifelse(DetailBalls == 0, NA, DetailBalls),
    DetailOvers = map_chr(DetailBalls, balls_to_overs_text),
    DetailMaidens = ifelse(DetailMaidens == 0, NA, DetailMaidens),
    DetailRunsConceded = ifelse(DetailRunsConceded == 0, NA, DetailRunsConceded)
  )

write_csv(
  bowling_enrichment,
  "outputs/bowling_missing_fields_enrichment.csv"
)

log_line(
  "Saved bowling enrichment:",
  "outputs/bowling_missing_fields_enrichment.csv",
  "| Rows:",
  nrow(bowling_enrichment)
)


# ============================================================
# 8. PATCH EXISTING CSV FILES
# ============================================================

log_section("PATCHING EXISTING CSV FILES")

if (!file.exists("outputs/all_international_batting.csv")) {
  stop("Missing outputs/all_international_batting.csv. Run international_stats_pipeline.R first.")
}

if (!file.exists("outputs/all_international_bowling.csv")) {
  stop("Missing outputs/all_international_bowling.csv. Run international_stats_pipeline.R first.")
}

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

# Ensure expected fields exist.
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


# ============================================================
# 9. PATCH BATTING AND LOG EACH EDIT
# ============================================================

log_section("PATCHING BATTING FIELDS")

all_batting_before <- all_batting

all_batting_enriched <- all_batting %>%
  left_join(
    batting_enrichment,
    by = c("unique_player_id", "format")
  ) %>%
  mutate(
    BF = ifelse(
      is.na(BF) | BF == "",
      as.character(DetailBallsFaced),
      as.character(BF)
    ),
    `4s` = ifelse(
      is.na(`4s`) | `4s` == "",
      as.character(DetailFours),
      as.character(`4s`)
    ),
    `6s` = ifelse(
      is.na(`6s`) | `6s` == "",
      as.character(DetailSixes),
      as.character(`6s`)
    ),
    SR = ifelse(
      (is.na(SR) | SR == "") &
        !is.na(parse_number(Runs)) &
        !is.na(parse_number(BF)) &
        parse_number(BF) > 0,
      as.character(round(parse_number(Runs) / parse_number(BF) * 100, 2)),
      as.character(SR)
    )
  ) %>%
  select(
    -DetailBallsFaced,
    -DetailFours,
    -DetailSixes
  )

log_field_edits(
  before_df = all_batting_before,
  after_df = all_batting_enriched,
  stat_type = "batting",
  fields_to_check = c("BF", "SR", "4s", "6s")
)

write_csv(
  all_batting_enriched,
  "outputs/all_international_batting_enriched.csv"
)

log_line(
  "Saved batting enriched:",
  "outputs/all_international_batting_enriched.csv",
  "| Rows:",
  nrow(all_batting_enriched)
)


# ============================================================
# 10. PATCH BOWLING AND LOG EACH EDIT
# ============================================================

log_section("PATCHING BOWLING FIELDS")

all_bowling_before <- all_bowling

all_bowling_enriched <- all_bowling %>%
  left_join(
    bowling_enrichment,
    by = c("unique_player_id", "format")
  ) %>%
  mutate(
    Balls = ifelse(
      is.na(Balls) | Balls == "",
      as.character(DetailBalls),
      as.character(Balls)
    ),
    Overs = ifelse(
      is.na(Overs) | Overs == "",
      as.character(DetailOvers),
      as.character(Overs)
    ),
    Mdns = ifelse(
      is.na(Mdns) | Mdns == "",
      as.character(DetailMaidens),
      as.character(Mdns)
    ),
    Runs = ifelse(
      is.na(Runs) | Runs == "",
      as.character(DetailRunsConceded),
      as.character(Runs)
    )
  ) %>%
  select(
    -DetailBalls,
    -DetailOvers,
    -DetailMaidens,
    -DetailRunsConceded
  )

log_field_edits(
  before_df = all_bowling_before,
  after_df = all_bowling_enriched,
  stat_type = "bowling",
  fields_to_check = c("Balls", "Overs", "Mdns", "Runs")
)

write_csv(
  all_bowling_enriched,
  "outputs/all_international_bowling_enriched.csv"
)

log_line(
  "Saved bowling enriched:",
  "outputs/all_international_bowling_enriched.csv",
  "| Rows:",
  nrow(all_bowling_enriched)
)


# ============================================================
# 11. OPTIONAL: REBUILD JSON FROM ENRICHED CSVs
# ============================================================

log_section("REBUILDING JSON FROM ENRICHED CSV FILES")

all_fielding <- read_csv(
  "outputs/all_international_fielding.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

player_index <- bind_rows(
  all_batting_enriched %>%
    select(cricinfo_id, unique_player_id, final_player_name, source_country_text),
  all_bowling_enriched %>%
    select(cricinfo_id, unique_player_id, final_player_name, source_country_text),
  all_fielding %>%
    select(cricinfo_id, unique_player_id, final_player_name, source_country_text)
) %>%
  distinct() %>%
  arrange(final_player_name)

write_csv(
  player_index,
  "outputs/player_index_enriched.csv"
)

missing_cricinfo_ids <- player_index %>%
  filter(is.na(cricinfo_id) | cricinfo_id == "")

write_csv(
  missing_cricinfo_ids,
  "outputs/missing_cricinfo_ids_enriched.csv"
)


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

  calculated_balls <- ifelse(
    is.na(raw_balls),
    overs_to_balls(raw_overs),
    raw_balls
  )

  calculated_overs <- ifelse(
    is.na(raw_overs),
    balls_to_overs_text(calculated_balls),
    raw_overs
  )

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
    return(list(
      test = empty_batting_stats(),
      odi = empty_batting_stats(),
      t20i = empty_batting_stats()
    ))
  }

  if (type == "bowling") {
    return(list(
      test = empty_bowling_stats(),
      odi = empty_bowling_stats(),
      t20i = empty_bowling_stats()
    ))
  }

  if (type == "fielding") {
    return(list(
      test = empty_fielding_stats(),
      odi = empty_fielding_stats(),
      t20i = empty_fielding_stats()
    ))
  }
}


all_player_ids <- player_index %>%
  filter(!is.na(unique_player_id), unique_player_id != "") %>%
  pull(unique_player_id) %>%
  unique() %>%
  sort()

nested_stats <- list()

for (idx in seq_along(all_player_ids)) {
  pid <- all_player_ids[[idx]]

  if (idx == 1 || idx %% 250 == 0 || idx == length(all_player_ids)) {
    log_line(
      "Building JSON player",
      idx,
      "of",
      length(all_player_ids),
      "| ID:",
      pid
    )
  }

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

jsonlite::write_json(
  nested_stats,
  "outputs/all_international_stats_enriched.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

log_line(
  "Saved enriched JSON:",
  "outputs/all_international_stats_enriched.json",
  "| Total unique players:",
  length(all_player_ids)
)


# ============================================================
# 12. SUMMARY
# ============================================================

summary_enrichment <- tibble(
  file = c(
    "batting_innings_detail_raw.csv",
    "bowling_innings_detail_raw.csv",
    "batting_missing_fields_enrichment.csv",
    "bowling_missing_fields_enrichment.csv",
    "all_international_batting_enriched.csv",
    "all_international_bowling_enriched.csv",
    "all_international_stats_enriched.json",
    "player_index_enriched.csv",
    "missing_cricinfo_ids_enriched.csv"
  ),
  path = c(
    "outputs/batting_innings_detail_raw.csv",
    "outputs/bowling_innings_detail_raw.csv",
    "outputs/batting_missing_fields_enrichment.csv",
    "outputs/bowling_missing_fields_enrichment.csv",
    "outputs/all_international_batting_enriched.csv",
    "outputs/all_international_bowling_enriched.csv",
    "outputs/all_international_stats_enriched.json",
    "outputs/player_index_enriched.csv",
    "outputs/missing_cricinfo_ids_enriched.csv"
  )
)

write_csv(
  summary_enrichment,
  "outputs/summary_enrichment.csv"
)

log_section("DONE ENRICHMENT")
log_line("Batting detail raw:", "outputs/batting_innings_detail_raw.csv")
log_line("Bowling detail raw:", "outputs/bowling_innings_detail_raw.csv")
log_line("Batting enriched CSV:", "outputs/all_international_batting_enriched.csv")
log_line("Bowling enriched CSV:", "outputs/all_international_bowling_enriched.csv")
log_line("Enriched JSON:", "outputs/all_international_stats_enriched.json")
log_line("Summary:", "outputs/summary_enrichment.csv")