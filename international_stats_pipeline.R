# ============================================================
# INTERNATIONAL CRICKET STATS PIPELINE
# Source: ESPNcricinfo Statsguru
#
# Pulls:
# - Test batting, bowling, fielding
# - ODI batting, bowling, fielding
# - T20I batting, bowling, fielding
#
# Outputs:
# - outputs/raw/test_batting_raw.csv, etc.
# - outputs/all_international_summary_flat.csv
# - outputs/field_names_by_table.csv
# - outputs/player_index.csv
# - outputs/missing_cricinfo_ids.csv
# - outputs/all_international_stats.json
# ============================================================


# ============================================================
# 0. INSTALL PACKAGES
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
    install.packages(pkg)
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
# 1. BASIC SETTINGS
# ============================================================

BASE_URL <- "https://stats.espncricinfo.com/ci/engine/stats/index.html"

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/raw", showWarnings = FALSE)

formats <- tribble(
  ~format_key, ~class_id,
  "test",      1,
  "odi",       2,
  "t20i",      3
)

stat_types <- tribble(
  ~stat_type, ~orderby,
  "batting",  "runs",
  "bowling",  "wickets",
  "fielding", "dismissals"
)


# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

build_statsguru_url <- function(class_id, stat_type, orderby, page = 1) {
  paste0(
    BASE_URL,
    "?class=", class_id,
    ";filter=advanced",
    ";orderby=", orderby,
    ";page=", page,
    ";size=200",
    ";template=results",
    ";type=", stat_type
  )
}


fetch_html <- function(url) {
  response <- httr::GET(
    url,
    httr::user_agent(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 Safari/605.1.15"
    )
  )

  status <- httr::status_code(response)

  if (status >= 400) {
    stop(paste("Request failed with status:", status, "URL:", url))
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
  # Old Statsguru links often look like:
  # /ci/content/player/253802.html
  #
  # Newer profile links may look like:
  # /cricketers/virat-kohli-253802
  #
  # This handles both styles.

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


# ============================================================
# 3. READ ONE STATSGURU PAGE
# ============================================================

read_statsguru_page <- function(url) {
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
        "Player" %in% names(clean_colnames(tbl))
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

  # Extract player links from first column of the same table.
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
    )

  list(
    data = df,
    has_next = has_next_page(html)
  )
}


# ============================================================
# 4. READ ALL PAGES FOR ONE FORMAT + STAT TYPE
# ============================================================

get_all_pages <- function(format_key, class_id, stat_type, orderby, max_pages = 200) {
  all_pages <- list()
  seen_ids <- character()

  for (page in 1:max_pages) {
    url <- build_statsguru_url(
      class_id = class_id,
      stat_type = stat_type,
      orderby = orderby,
      page = page
    )

    cat("Reading:", format_key, stat_type, "page", page, "\n")

    page_result <- read_statsguru_page(url)
    df <- page_result$data

    if (nrow(df) == 0) {
      cat("No rows found. Stopping:", format_key, stat_type, "\n")
      break
    }

    df <- df %>%
      mutate(
        across(everything(), as.character),
        format = as.character(format_key),
        stat_type = as.character(stat_type),
        source = "espncricinfo_statsguru",
        source_url = as.character(url)
      )

    current_ids <- df$unique_player_id

    # Safety check: if ESPN repeats a page, stop.
    if (page > 1 && all(current_ids %in% seen_ids)) {
      cat("Repeated page detected. Stopping:", format_key, stat_type, "\n")
      break
    }

    seen_ids <- unique(c(seen_ids, current_ids))
    all_pages[[page]] <- df

    if (!page_result$has_next) {
      cat("No next page. Finished:", format_key, stat_type, "\n")
      break
    }

    Sys.sleep(1.5)
  }

  bind_rows(all_pages)
}


# ============================================================
# 5. SCRAPE ALL INTERNATIONAL SUMMARY TABLES
# ============================================================

all_tables <- list()
field_names_rows <- list()

for (i in seq_len(nrow(formats))) {
  for (j in seq_len(nrow(stat_types))) {

    format_key <- formats$format_key[i]
    class_id <- formats$class_id[i]

    stat_type <- stat_types$stat_type[j]
    orderby <- stat_types$orderby[j]

    table_key <- paste(format_key, stat_type, sep = "_")

    df <- get_all_pages(
      format_key = format_key,
      class_id = class_id,
      stat_type = stat_type,
      orderby = orderby
    )

    all_tables[[table_key]] <- df

    raw_file <- paste0("outputs/raw/", table_key, "_raw.csv")

    write_csv(df, raw_file)

    field_names_rows[[table_key]] <- tibble(
      table_key = table_key,
      format = format_key,
      stat_type = stat_type,
      field_names = paste(names(df), collapse = " | ")
    )

    cat("Saved:", raw_file, "Rows:", nrow(df), "\n\n")
  }
}

field_names_by_table <- bind_rows(field_names_rows)

write_csv(
  field_names_by_table,
  "outputs/field_names_by_table.csv"
)

all_summary_flat <- all_tables %>%
  map(~ mutate(.x, across(everything(), as.character))) %>%
  bind_rows()


write_csv(
  all_summary_flat,
  "outputs/all_international_summary_flat.csv"
)

cat("Saved flat summary:", nrow(all_summary_flat), "rows\n")


# ============================================================
# 6. CREATE PLAYER INDEX
# ============================================================

player_index <- all_summary_flat %>%
  select(
    unique_player_id,
    cricinfo_id,
    final_player_name,
    source_country_text
  ) %>%
  distinct() %>%
  arrange(final_player_name)

write_csv(
  player_index,
  "outputs/player_index.csv"
)

missing_cricinfo_ids <- player_index %>%
  filter(is.na(cricinfo_id) | cricinfo_id == "")

write_csv(
  missing_cricinfo_ids,
  "outputs/missing_cricinfo_ids.csv"
)


# ============================================================
# 7. CONVERT RAW ROWS INTO CLEAN NESTED STATS
# ============================================================

row_to_batting <- function(row) {
  hs <- parse_high_score(get_value(row, c("HS", "High Score", "HighScore")))

  list(
    Span = parse_text(get_value(row, c("Span"))),
    Start = parse_span_start(get_value(row, c("Span"))),
    End = parse_span_end(get_value(row, c("Span"))),

    Matches = parse_number(get_value(row, c("Mat", "Matches"))),
    Innings = parse_number(get_value(row, c("Inns", "Innings"))),
    NotOuts = parse_number(get_value(row, c("NO", "Not Outs", "NotOuts"))),

    Runs = parse_number(get_value(row, c("Runs"))),
    HighScore = hs$score,
    HighScoreNotOut = hs$not_out,

    Average = parse_number(get_value(row, c("Ave", "Avg", "Average"))),
    BallsFaced = parse_number(get_value(row, c("BF", "Balls Faced", "BallsFaced"))),
    StrikeRate = parse_number(get_value(row, c("SR", "Strike Rate", "StrikeRate"))),

    Hundreds = parse_number(get_value(row, c("100", "100s", "X100", "Hundreds"))),
    Fifties = parse_number(get_value(row, c("50", "50s", "X50", "Fifties"))),
    Ducks = parse_number(get_value(row, c("0", "0s", "X0", "Ducks"))),

    Fours = parse_number(get_value(row, c("4s", "Fours"))),
    Sixes = parse_number(get_value(row, c("6s", "Sixes")))
  )
}


row_to_bowling <- function(row) {
  list(
    Span = parse_text(get_value(row, c("Span"))),
    Start = parse_span_start(get_value(row, c("Span"))),
    End = parse_span_end(get_value(row, c("Span"))),

    Matches = parse_number(get_value(row, c("Mat", "Matches"))),
    Innings = parse_number(get_value(row, c("Inns", "Innings"))),

    Balls = parse_number(get_value(row, c("Balls", "Ball"))),
    Overs = parse_number(get_value(row, c("Overs", "O"))),
    Maidens = parse_number(get_value(row, c("Mdns", "Maidens"))),

    RunsConceded = parse_number(get_value(row, c("Runs"))),
    Wickets = parse_number(get_value(row, c("Wkts", "Wickets"))),

    BestBowlingInnings = parse_text(get_value(row, c("BBI"))),
    BestBowlingMatch = parse_text(get_value(row, c("BBM"))),

    Average = parse_number(get_value(row, c("Ave", "Avg", "Average"))),
    Economy = parse_number(get_value(row, c("Econ", "Economy"))),
    StrikeRate = parse_number(get_value(row, c("SR", "Strike Rate", "StrikeRate"))),

    FourWickets = parse_number(get_value(row, c("4", "4w", "X4", "FourWickets"))),
    FiveWickets = parse_number(get_value(row, c("5", "5w", "X5", "FiveWickets"))),
    TenWickets = parse_number(get_value(row, c("10", "10w", "X10", "TenWickets")))
  )
}


row_to_fielding <- function(row) {
  list(
    Span = parse_text(get_value(row, c("Span"))),
    Start = parse_span_start(get_value(row, c("Span"))),
    End = parse_span_end(get_value(row, c("Span"))),

    Matches = parse_number(get_value(row, c("Mat", "Matches"))),
    Innings = parse_number(get_value(row, c("Inns", "Innings"))),

    Dismissals = parse_number(get_value(row, c("Dis", "Dismissals"))),
    Caught = parse_number(get_value(row, c("Ct", "Catches", "Caught"))),
    Stumped = parse_number(get_value(row, c("St", "Stumped"))),

    CaughtBehind = parse_number(get_value(row, c("Ct Wk", "CtWk", "Caught Wicketkeeper"))),
    CaughtFielder = parse_number(get_value(row, c("Ct Fi", "CtFi", "Caught Fielder"))),

    MaxDismissalsInnings = parse_number(get_value(row, c("MD", "Max Dismissals")))
  )
}


empty_format_stats <- function() {
  list(
    test = NULL,
    odi = NULL,
    t20i = NULL
  )
}


# ============================================================
# 8. BUILD NESTED JSON BY UNIQUE PLAYER ID
# ============================================================

all_player_ids <- all_summary_flat %>%
  filter(!is.na(unique_player_id), unique_player_id != "") %>%
  pull(unique_player_id) %>%
  unique()

all_player_ids <- sort(all_player_ids)

nested_stats <- list()

for (pid in all_player_ids) {

  player_rows <- all_summary_flat %>%
    filter(unique_player_id == pid)

  player_meta <- player_rows %>%
    select(
      unique_player_id,
      cricinfo_id,
      final_player_name,
      source_country_text
    ) %>%
    distinct() %>%
    slice(1)

  player_obj <- list(
    player_info = list(
      unique_player_id = player_meta$unique_player_id,
      cricinfo_id = player_meta$cricinfo_id,
      final_player_name = player_meta$final_player_name,
      final_country = player_meta$source_country_text,
      source = "espncricinfo_statsguru"
    ),
    batting = empty_format_stats(),
    bowling = empty_format_stats(),
    fielding = empty_format_stats()
  )

  for (fmt in c("test", "odi", "t20i")) {

    batting_row <- player_rows %>%
      filter(format == fmt, stat_type == "batting") %>%
      slice(1)

    if (nrow(batting_row) == 1) {
      player_obj$batting[[fmt]] <- row_to_batting(batting_row)
    }

    bowling_row <- player_rows %>%
      filter(format == fmt, stat_type == "bowling") %>%
      slice(1)

    if (nrow(bowling_row) == 1) {
      player_obj$bowling[[fmt]] <- row_to_bowling(bowling_row)
    }

    fielding_row <- player_rows %>%
      filter(format == fmt, stat_type == "fielding") %>%
      slice(1)

    if (nrow(fielding_row) == 1) {
      player_obj$fielding[[fmt]] <- row_to_fielding(fielding_row)
    }
  }

  nested_stats[[pid]] <- player_obj
}


jsonlite::write_json(
  nested_stats,
  "outputs/all_international_stats.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat("\n============================================================\n")
cat("DONE\n")
cat("============================================================\n")
cat("Flat CSV: outputs/all_international_summary_flat.csv\n")
cat("Nested JSON: outputs/all_international_stats.json\n")
cat("Player index: outputs/player_index.csv\n")
cat("Field names: outputs/field_names_by_table.csv\n")
cat("Missing IDs: outputs/missing_cricinfo_ids.csv\n")
cat("Total unique players:", length(all_player_ids), "\n")
cat("============================================================\n")