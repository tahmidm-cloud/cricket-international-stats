# ============================================================
# SCRAPE DOMESTIC PROFILES CHUNK
#
# Separate output only. Does NOT edit international JSON.
#
# Reads:
# - outputs/player_index_enriched.csv
# - fallback outputs/player_index.csv
# - fallback outputs/all_international_stats_enriched.json
#
# Writes:
# - outputs/domestic_profile_chunks/<chunk>_profiles.csv
# - outputs/domestic_profile_chunks/<chunk>_domestic_stats.csv
# - outputs/domestic_profile_chunks/<chunk>_errors.csv
#
# Prints after each player:
# - profile result
# - domestic/career stat rows
# - missing fields
# - progress saved count
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

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/domestic_profile_chunks", showWarnings = FALSE)
dir.create("outputs/domestic_profile_cache", showWarnings = FALSE)
dir.create("outputs/domestic_profile_cache/profile_pages", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/domestic_profile_cache/stats_pages", showWarnings = FALSE, recursive = TRUE)

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

safe_env <- function(name, default = NA_character_) {
  value <- Sys.getenv(name)

  if (is.na(value) || value == "") {
    return(default)
  }

  value
}

CHUNK_LABEL <- safe_env("CHUNK_LABEL", "local_chunk")
ROW_START <- as.integer(safe_env("ROW_START", "1"))
ROW_END <- as.integer(safe_env("ROW_END", "50"))
SLEEP_MIN <- as.numeric(safe_env("SLEEP_MIN", "8"))
SLEEP_MAX <- as.numeric(safe_env("SLEEP_MAX", "18"))

if (is.na(ROW_START)) ROW_START <- 1
if (is.na(ROW_END)) ROW_END <- 50
if (is.na(SLEEP_MIN)) SLEEP_MIN <- 8
if (is.na(SLEEP_MAX)) SLEEP_MAX <- 18

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

safe_value <- function(x) {
  x <- parse_text(x)

  if (length(x) == 0 || is.na(x)) {
    return("—")
  }

  as.character(x)
}

clean_player_name <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\u00a0", " ") %>%
    str_squish()
}

make_slug <- function(x) {
  x %>%
    clean_player_name() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("^-+|-+$", "")
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

build_profile_url <- function(player_name, cricinfo_id) {
  slug <- make_slug(player_name)
  paste0("https://www.espncricinfo.com/cricketers/", slug, "-", cricinfo_id)
}

build_stats_url <- function(player_name, cricinfo_id) {
  paste0(build_profile_url(player_name, cricinfo_id), "/bowling-batting-stats")
}

fetch_url_text <- function(url, cache_file, label, max_attempts = 3) {
  if (file.exists(cache_file) && file.info(cache_file)$size > 0) {
    log_line("CACHE HIT", label, "|", basename(cache_file))

    txt <- readLines(
      cache_file,
      warn = FALSE,
      encoding = "UTF-8"
    ) %>%
      paste(collapse = "\n")

    return(list(
      text = txt,
      from_cache = TRUE,
      status_code = NA_integer_
    ))
  }

  last_error <- NA_character_

  for (attempt in seq_len(max_attempts)) {
    log_line("FETCH", label, "| Attempt", attempt, "of", max_attempts)

    result <- tryCatch({
      response <- httr::GET(
        url,
        httr::user_agent("Mozilla/5.0 AppleWebKit/605.1.15 Safari/605.1.15"),
        httr::timeout(30)
      )

      status <- httr::status_code(response)

      if (status >= 400) {
        stop(paste("Request failed:", status, url))
      }

      txt <- httr::content(response, as = "text", encoding = "UTF-8")

      writeLines(txt, cache_file, useBytes = TRUE)

      list(
        text = txt,
        from_cache = FALSE,
        status_code = status
      )
    }, error = function(e) {
      last_error <<- e$message
      NULL
    })

    if (!is.null(result)) {
      log_line("FETCH OK", label, "| Status:", result$status_code)
      return(result)
    }

    wait_time <- 10 * attempt
    log_line("FETCH ERROR", label, "|", last_error, "| Waiting", wait_time, "seconds")
    Sys.sleep(wait_time)
  }

  stop(paste("Failed after retries:", label, "|", last_error))
}

flatten_text <- function(html) {
  html %>%
    html_elements("body") %>%
    html_text2() %>%
    paste(collapse = "\n") %>%
    str_replace_all("\u00a0", " ") %>%
    str_squish()
}

extract_label_value_from_text <- function(page_text, labels) {
  for (label in labels) {
    pattern <- paste0(label, "\\s+([^|\\n]{2,180})")

    m <- str_match(
      page_text,
      regex(pattern, ignore_case = TRUE)
    )[, 2]

    if (!is.na(m)) {
      m <- str_squish(m)

      m <- str_replace(
        m,
        "\\s+(Age|Batting Style|Batting style|Bowling Style|Bowling style|Playing Role|Playing role|Born|Birth Place|Birthplace|Place of Birth|Teams|Major teams|Current teams)\\s+.*$",
        ""
      )

      return(parse_text(m))
    }
  }

  NA_character_
}

extract_teams_from_text <- function(page_text) {
  extract_label_value_from_text(
    page_text,
    c(
      "Current teams",
      "Current Teams",
      "Teams",
      "Major teams",
      "Major Teams"
    )
  )
}

extract_profile_from_page <- function(player_row, html_text) {
  html <- read_html(html_text)
  page_text <- flatten_text(html)

  title <- html %>%
    html_element("title") %>%
    html_text2()

  h1 <- html %>%
    html_elements("h1") %>%
    html_text2() %>%
    .[1]

  display_name <- parse_text(h1)

  if (length(display_name) == 0 || is.na(display_name)) {
    display_name <- title %>%
      str_replace("\\s*Profile.*$", "") %>%
      str_replace("\\s*Cricket Player.*$", "") %>%
      str_squish() %>%
      parse_text()
  }

  full_name <- extract_label_value_from_text(
    page_text,
    c("Full Name", "Full name")
  )

  dob <- extract_label_value_from_text(
    page_text,
    c("Born", "Date of Birth", "Date of birth")
  )

  place_of_birth <- extract_label_value_from_text(
    page_text,
    c("Birth Place", "Birthplace", "Place of Birth", "Place of birth")
  )

  batting_style <- extract_label_value_from_text(
    page_text,
    c("Batting Style", "Batting style")
  )

  bowling_style <- extract_label_value_from_text(
    page_text,
    c("Bowling Style", "Bowling style")
  )

  playing_role <- extract_label_value_from_text(
    page_text,
    c("Playing Role", "Playing role", "Role")
  )

  teams <- extract_teams_from_text(page_text)

  national_team <- player_row$source_country_text

  tibble(
    cricinfo_id = player_row$cricinfo_id,
    unique_player_id = player_row$unique_player_id,
    source_name = player_row$final_player_name,

    cricinfo_display_name = display_name,
    full_name = full_name,
    dob_raw = dob,
    place_of_birth = place_of_birth,
    batting_style = batting_style,
    bowling_style = bowling_style,
    playing_role = playing_role,
    national_team = national_team,
    current_team = teams,
    teams_raw = teams,

    profile_url = player_row$profile_url,
    stats_url = player_row$stats_url,
    profile_source = "espncricinfo_profile_page"
  )
}

looks_like_career_stats_table <- function(df) {
  if (ncol(df) < 3) {
    return(FALSE)
  }

  nms <- names(df) %>% str_to_lower()

  has_format_or_matches <- any(str_detect(nms, "^format$|^mat$|^matches$|^span$"))
  has_stat <- any(str_detect(nms, "^runs$|^wkts$|^wickets$|^ave$|^avg$|^sr$|^econ$|^bf$|^balls$|^overs$"))

  has_format_or_matches && has_stat
}

extract_domestic_stats_from_page <- function(player_row, html_text) {
  html <- read_html(html_text)

  tables <- html %>%
    html_elements("table") %>%
    html_table(fill = TRUE)

  if (length(tables) == 0) {
    return(tibble())
  }

  out <- list()

  for (idx in seq_along(tables)) {
    df <- tables[[idx]] %>%
      as.data.frame(check.names = FALSE) %>%
      clean_colnames() %>%
      as_tibble(.name_repair = "unique")

    if (nrow(df) == 0) {
      next
    }

    names(df)[1] <- "Format"

    if (!looks_like_career_stats_table(df)) {
      next
    }

    df <- df %>%
      mutate(across(everything(), as.character)) %>%
      filter(
        !is.na(Format),
        Format != "",
        Format != "Format"
      )

    if (nrow(df) == 0) {
      next
    }

    df <- df %>%
      mutate(
        cricinfo_id = player_row$cricinfo_id,
        unique_player_id = player_row$unique_player_id,
        final_player_name = player_row$final_player_name,
        national_team = player_row$source_country_text,
        table_index = as.character(idx),
        stats_url = player_row$stats_url,
        .before = 1
      )

    out[[length(out) + 1]] <- df
  }

  bind_rows(out)
}

get_row_value <- function(row, candidates) {
  existing <- candidates[candidates %in% names(row)]

  if (length(existing) == 0) {
    return("—")
  }

  safe_value(row[[existing[1]]][1])
}

print_profile_result <- function(profile_row) {
  log_section("PROFILE RESULT")

  log_line("Cricinfo ID:", safe_value(profile_row$cricinfo_id))
  log_line("Source name:", safe_value(profile_row$source_name))
  log_line("Display name:", safe_value(profile_row$cricinfo_display_name))
  log_line("Full name:", safe_value(profile_row$full_name))
  log_line("DOB:", safe_value(profile_row$dob_raw))
  log_line("Place of birth:", safe_value(profile_row$place_of_birth))
  log_line("Batting style:", safe_value(profile_row$batting_style))
  log_line("Bowling style:", safe_value(profile_row$bowling_style))
  log_line("Playing role:", safe_value(profile_row$playing_role))
  log_line("National team:", safe_value(profile_row$national_team))
  log_line("Current team/teams:", safe_value(profile_row$current_team))
}

print_domestic_result <- function(stats_rows) {
  log_section("DOMESTIC / CAREER STATS RESULT")

  if (nrow(stats_rows) == 0) {
    log_line("No domestic/career stat rows found.")
    return(invisible(NULL))
  }

  log_line("Rows found:", nrow(stats_rows))
  log_line("Columns:", paste(names(stats_rows), collapse = " | "))

  max_rows_to_print <- min(nrow(stats_rows), 20)

  for (i in seq_len(max_rows_to_print)) {
    row <- stats_rows[i, ]

    format_value <- get_row_value(row, c("Format"))
    mat_value <- get_row_value(row, c("Mat", "Matches"))
    inns_value <- get_row_value(row, c("Inns", "Innings"))
    runs_value <- get_row_value(row, c("Runs"))
    avg_value <- get_row_value(row, c("Ave", "Average", "Avg"))
    sr_value <- get_row_value(row, c("SR", "StrikeRate", "Strike Rate"))
    wkts_value <- get_row_value(row, c("Wkts", "Wickets"))
    econ_value <- get_row_value(row, c("Econ", "Economy"))

    log_line(
      "STAT ROW",
      i,
      "| Format:", format_value,
      "| Mat:", mat_value,
      "| Inns:", inns_value,
      "| Runs:", runs_value,
      "| Avg:", avg_value,
      "| SR:", sr_value,
      "| Wkts:", wkts_value,
      "| Econ:", econ_value
    )
  }

  if (nrow(stats_rows) > max_rows_to_print) {
    log_line("Additional rows not printed:", nrow(stats_rows) - max_rows_to_print)
  }
}

print_scrape_summary <- function(player_row, profile_row, stats_rows, profile_fetch, stats_fetch) {
  log_section("SCRAPE SUMMARY")

  log_line("Player:", safe_value(player_row$final_player_name))
  log_line("ID:", safe_value(player_row$cricinfo_id))
  log_line("Profile fetch:", ifelse(profile_fetch$from_cache, "cache", "request"))
  log_line("Stats fetch:", ifelse(stats_fetch$from_cache, "cache", "request"))
  log_line("Profile URL:", safe_value(player_row$profile_url))
  log_line("Stats URL:", safe_value(player_row$stats_url))
  log_line("Domestic stat rows:", nrow(stats_rows))

  missing_fields <- c()

  if (is.na(profile_row$cricinfo_display_name)) missing_fields <- c(missing_fields, "display_name")
  if (is.na(profile_row$full_name)) missing_fields <- c(missing_fields, "full_name")
  if (is.na(profile_row$dob_raw)) missing_fields <- c(missing_fields, "dob")
  if (is.na(profile_row$place_of_birth)) missing_fields <- c(missing_fields, "place_of_birth")
  if (is.na(profile_row$batting_style)) missing_fields <- c(missing_fields, "batting_style")
  if (is.na(profile_row$bowling_style)) missing_fields <- c(missing_fields, "bowling_style")
  if (is.na(profile_row$playing_role)) missing_fields <- c(missing_fields, "playing_role")
  if (is.na(profile_row$current_team)) missing_fields <- c(missing_fields, "current_team")

  if (length(missing_fields) == 0) {
    log_line("Missing profile fields: none")
  } else {
    log_line("Missing profile fields:", paste(missing_fields, collapse = ", "))
  }
}

write_progress_files <- function(profiles, domestic_stats, errors) {
  profiles_df <- bind_rows(profiles)
  domestic_stats_df <- bind_rows(domestic_stats)
  errors_df <- bind_rows(errors)

  profile_file <- paste0("outputs/domestic_profile_chunks/", CHUNK_LABEL, "_profiles.csv")
  stats_file <- paste0("outputs/domestic_profile_chunks/", CHUNK_LABEL, "_domestic_stats.csv")
  errors_file <- paste0("outputs/domestic_profile_chunks/", CHUNK_LABEL, "_errors.csv")

  write_csv(profiles_df, profile_file)
  write_csv(domestic_stats_df, stats_file)
  write_csv(errors_df, errors_file)

  log_line("PROGRESS SAVED profiles:", nrow(profiles_df), "| stats rows:", nrow(domestic_stats_df), "| errors:", nrow(errors_df))
}

load_player_index <- function() {
  if (file.exists("outputs/player_index_enriched.csv")) {
    log_line("Using outputs/player_index_enriched.csv")

    return(
      read_csv(
        "outputs/player_index_enriched.csv",
        show_col_types = FALSE,
        col_types = cols(.default = col_character())
      )
    )
  }

  if (file.exists("outputs/player_index.csv")) {
    log_line("Using outputs/player_index.csv")

    return(
      read_csv(
        "outputs/player_index.csv",
        show_col_types = FALSE,
        col_types = cols(.default = col_character())
      )
    )
  }

  if (file.exists("outputs/all_international_stats_enriched.json")) {
    log_line("Building player index from outputs/all_international_stats_enriched.json")

    data <- jsonlite::fromJSON(
      "outputs/all_international_stats_enriched.json",
      simplifyVector = FALSE
    )

    rows <- imap(data, function(player, pid) {
      info <- player$player_info

      tibble(
        cricinfo_id = as.character(info$cricinfo_id),
        unique_player_id = as.character(info$unique_player_id),
        final_player_name = as.character(info$final_player_name),
        source_country_text = as.character(info$final_country)
      )
    })

    return(bind_rows(rows))
  }

  stop("No player index found. Need outputs/player_index_enriched.csv or outputs/player_index.csv")
}

players <- load_player_index() %>%
  mutate(
    cricinfo_id = as.character(cricinfo_id),
    unique_player_id = as.character(unique_player_id),
    final_player_name = as.character(final_player_name),
    source_country_text = as.character(source_country_text)
  ) %>%
  filter(
    !is.na(cricinfo_id),
    cricinfo_id != "",
    !str_detect(cricinfo_id, "^missing_")
  ) %>%
  distinct(cricinfo_id, .keep_all = TRUE) %>%
  arrange(final_player_name)

total_players <- nrow(players)

chunk_players <- players %>%
  mutate(row_number_all = row_number()) %>%
  filter(
    row_number_all >= ROW_START,
    row_number_all <= ROW_END
  ) %>%
  mutate(
    profile_url = map2_chr(final_player_name, cricinfo_id, build_profile_url),
    stats_url = map2_chr(final_player_name, cricinfo_id, build_stats_url)
  )

log_section("CHUNK START")
log_line("Chunk:", CHUNK_LABEL)
log_line("Rows:", ROW_START, "to", ROW_END)
log_line("Total valid Cricinfo players:", total_players)
log_line("Players in this chunk:", nrow(chunk_players))
log_line("Sleep range:", SLEEP_MIN, "to", SLEEP_MAX, "seconds")

if (nrow(chunk_players) == 0) {
  write_csv(tibble(), paste0("outputs/domestic_profile_chunks/", CHUNK_LABEL, "_profiles.csv"))
  write_csv(tibble(), paste0("outputs/domestic_profile_chunks/", CHUNK_LABEL, "_domestic_stats.csv"))
  write_csv(tibble(), paste0("outputs/domestic_profile_chunks/", CHUNK_LABEL, "_errors.csv"))

  log_line("No players in this chunk. Exiting.")
  quit(save = "no", status = 0)
}

profiles <- list()
domestic_stats <- list()
errors <- list()

for (i in seq_len(nrow(chunk_players))) {
  player_row <- chunk_players[i, ]

  log_section(
    paste0(
      "PLAYER ",
      i,
      " OF ",
      nrow(chunk_players),
      " | ",
      player_row$final_player_name,
      " | ID ",
      player_row$cricinfo_id
    )
  )

  profile_cache <- file.path(
    "outputs/domestic_profile_cache/profile_pages",
    paste0(player_row$cricinfo_id, ".html")
  )

  stats_cache <- file.path(
    "outputs/domestic_profile_cache/stats_pages",
    paste0(player_row$cricinfo_id, ".html")
  )

  tryCatch({
    profile_fetch <- fetch_url_text(
      url = player_row$profile_url,
      cache_file = profile_cache,
      label = paste0(player_row$final_player_name, " profile")
    )

    inner_delay <- runif(1, min = 2, max = 5)
    log_line("Inner sleep before stats page:", round(inner_delay, 1), "seconds")
    Sys.sleep(inner_delay)

    stats_fetch <- fetch_url_text(
      url = player_row$stats_url,
      cache_file = stats_cache,
      label = paste0(player_row$final_player_name, " stats")
    )

    profile_row <- extract_profile_from_page(
      player_row = player_row,
      html_text = profile_fetch$text
    )

    stats_rows <- extract_domestic_stats_from_page(
      player_row = player_row,
      html_text = stats_fetch$text
    )

    profiles[[length(profiles) + 1]] <- profile_row

    if (nrow(stats_rows) > 0) {
      domestic_stats[[length(domestic_stats) + 1]] <- stats_rows
    }

    print_profile_result(profile_row)
    print_domestic_result(stats_rows)
    print_scrape_summary(player_row, profile_row, stats_rows, profile_fetch, stats_fetch)

    log_line("OK", player_row$final_player_name, "| profile saved | stat rows:", nrow(stats_rows))

  }, error = function(e) {
    log_line("ERROR", player_row$final_player_name, "|", e$message)

    errors[[length(errors) + 1]] <<- tibble(
      cricinfo_id = player_row$cricinfo_id,
      unique_player_id = player_row$unique_player_id,
      final_player_name = player_row$final_player_name,
      profile_url = player_row$profile_url,
      stats_url = player_row$stats_url,
      error_message = e$message
    )
  })

  write_progress_files(profiles, domestic_stats, errors)

  if (i < nrow(chunk_players)) {
    delay <- runif(1, min = SLEEP_MIN, max = SLEEP_MAX)
    log_line("Sleeping before next player:", round(delay, 1), "seconds")
    Sys.sleep(delay)
  }
}

log_section("FINAL CHUNK SAVE")

write_progress_files(profiles, domestic_stats, errors)

profiles_df <- bind_rows(profiles)
domestic_stats_df <- bind_rows(domestic_stats)
errors_df <- bind_rows(errors)

log_line("Final profiles rows:", nrow(profiles_df))
log_line("Final domestic stat rows:", nrow(domestic_stats_df))
log_line("Final errors:", nrow(errors_df))
log_line("DONE CHUNK:", CHUNK_LABEL)