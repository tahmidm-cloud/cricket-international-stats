# ============================================================
# INTERNATIONAL CRICKET STATS PIPELINE - CLEAN VERSION
# Source: ESPNcricinfo Statsguru
#
# Fix:
# - Batting tables only keep batting columns
# - Bowling tables only keep bowling columns
# - Fielding tables only keep fielding columns
# - No giant combined batting/bowling/fielding raw schema
# - Every player keeps unique_player_id / cricinfo_id
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
# 1. SETTINGS
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
# 2. SEPARATE RAW SCHEMAS
# ============================================================

PLAYER_COLUMNS <- c(
  "cricinfo_id",
  "unique_player_id",
  "final_player_name",
  "source_country_text",
  "Player"
)

BATTING_COLUMNS <- c(
  PLAYER_COLUMNS,
  "Span",
  "Mat",
  "Inns",
  "NO",
  "Runs",
  "HS",
  "Ave",
  "BF",
  "SR",
  "100",
  "50",
  "0",
  "4s",
  "6s"
)

# I kept Runs/Ave/SR here because for bowling:
# Runs = Runs conceded
# Ave = Bowling average
# SR = Bowling strike rate
# Removing them would lose important bowling data.
BOWLING_COLUMNS <- c(
  PLAYER_COLUMNS,
  "Span",
  "Mat",
  "Inns",
  "Balls",
  "Overs",
  "Mdns",
  "Runs",
  "Wkts",
  "BBI",
  "BBM",
  "Ave",
  "Econ",
  "SR",
  "4",
  "5",
  "10"
)

FIELDING_COLUMNS <- c(
  PLAYER_COLUMNS,
  "Span",
  "Mat",
  "Inns",
  "Dis",
  "Ct",
  "St",
  "Ct Wk",
  "Ct Fi",
  "MD",
  "D/I"
)


get_schema_for_type <- function(stat_type) {
  if (stat_type == "batting") {
    return(BATTING_COLUMNS)
  }

  if (stat_type == "bowling") {
    return(BOWLING_COLUMNS)
  }

  if (stat_type == "fielding") {
    return(FIELDING_COLUMNS)
  }

  stop(paste("Unknown stat_type:", stat_type))
}


apply_schema <- function(df, stat_type) {
  schema <- get_schema_for_type(stat_type)

  missing_cols <- setdiff(schema, names(df))

  for (col in missing_cols) {
    df[[col]] <- NA_character_
  }

  df <- df[, schema]

  df
}


# ============================================================
# 3. HELPERS
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


# ============================================================
# 4. READ ONE STATSGURU PAGE
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
    )

  list(
    data = df,
    has_next = has_next_page(html)
  )
}


# ============================================================
# 5. READ ALL PAGES FOR ONE TABLE
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
      mutate(across(everything(), as.character)) %>%
      apply_schema(stat_type)

    current_ids <- df$unique_player_id

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

  all_pages %>%
    map(~ mutate(.x, across(everything(), as.character))) %>%
    map(~ apply_schema(.x, stat_type)) %>%
    bind_rows()
}


# ============================================================
# 6. SCRAPE TABLES
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


# ============================================================
# 7. CREATE SEPARATE COMBINED FILES BY STAT TYPE
# ============================================================

all_batting <- bind_rows(
  all_tables$test_batting %>% mutate(format = "test"),
  all_tables$odi_batting %>% mutate(format = "odi"),
  all_tables$t20i_batting %>% mutate(format = "t20i")
)

all_bowling <- bind_rows(
  all_tables$test_bowling %>% mutate(format = "test"),
  all_tables$odi_bowling %>% mutate(format = "odi"),
  all_tables$t20i_bowling %>% mutate(format = "t20i")
)

all_fielding <- bind_rows(
  all_tables$test_fielding %>% mutate(format = "test"),
  all_tables$odi_fielding %>% mutate(format = "odi"),
  all_tables$t20i_fielding %>% mutate(format = "t20i")
)

write_csv(all_batting, "outputs/all_international_batting.csv")
write_csv(all_bowling, "outputs/all_international_bowling.csv")
write_csv(all_fielding, "outputs/all_international_fielding.csv")


# ============================================================
# 8. PLAYER INDEX
# ============================================================

player_index <- bind_rows(
  all_batting %>% select(cricinfo_id, unique_player_id, final_player_name, source_country_text),
  all_bowling %>% select(cricinfo_id, unique_player_id, final_player_name, source_country_text),
  all_fielding %>% select(cricinfo_id, unique_player_id, final_player_name, source_country_text)
) %>%
  distinct() %>%
  arrange(final_player_name)

write_csv(player_index, "outputs/player_index.csv")

missing_cricinfo_ids <- player_index %>%
  filter(is.na(cricinfo_id) | cricinfo_id == "")

write_csv(missing_cricinfo_ids, "outputs/missing_cricinfo_ids.csv")


# ============================================================
# 9. JSON CONVERSION
# ============================================================

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


# ============================================================
# 10. BUILD NESTED JSON
# ============================================================

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
      source = "espncricinfo_statsguru"
    ),
    batting = empty_format_group("batting"),
    bowling = empty_format_group("bowling"),
    fielding = empty_format_group("fielding")
  )

  for (fmt in c("test", "odi", "t20i")) {

    batting_row <- all_batting %>%
      filter(unique_player_id == pid, format == fmt) %>%
      slice(1)

    if (nrow(batting_row) == 1) {
      player_obj$batting[[fmt]] <- row_to_batting(batting_row)
    }

    bowling_row <- all_bowling %>%
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
  "outputs/all_international_stats.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)


# ============================================================
# 11. SUMMARY FILES
# ============================================================

summary_by_table <- tibble(
  table = names(all_tables),
  rows = map_int(all_tables, nrow),
  fields = map_chr(all_tables, ~ paste(names(.x), collapse = " | "))
)

write_csv(summary_by_table, "outputs/summary_by_table.csv")

cat("\n============================================================\n")
cat("DONE\n")
cat("============================================================\n")
cat("Raw files: outputs/raw\n")
cat("Batting CSV: outputs/all_international_batting.csv\n")
cat("Bowling CSV: outputs/all_international_bowling.csv\n")
cat("Fielding CSV: outputs/all_international_fielding.csv\n")
cat("Nested JSON: outputs/all_international_stats.json\n")
cat("Player index: outputs/player_index.csv\n")
cat("Missing IDs: outputs/missing_cricinfo_ids.csv\n")
cat("Field names: outputs/field_names_by_table.csv\n")
cat("Total unique players:", length(all_player_ids), "\n")
cat("============================================================\n")