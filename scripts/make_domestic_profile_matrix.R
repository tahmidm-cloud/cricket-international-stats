# ============================================================
# MAKE DOMESTIC PROFILE MATRIX
#
# Counts valid Cricinfo players and creates GitHub Actions matrix.
# Works for 8,500+ players.
# ============================================================

library(dplyr)
library(readr)
library(jsonlite)
library(stringr)

dir.create("outputs", showWarnings = FALSE)

chunk_size <- as.integer(Sys.getenv("CHUNK_SIZE", "50"))

if (is.na(chunk_size) || chunk_size <= 0) {
  chunk_size <- 50
}

load_player_index <- function() {
  if (file.exists("outputs/player_index_enriched.csv")) {
    return(read_csv(
      "outputs/player_index_enriched.csv",
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    ))
  }

  if (file.exists("outputs/player_index.csv")) {
    return(read_csv(
      "outputs/player_index.csv",
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    ))
  }

  if (file.exists("outputs/all_international_stats_enriched.json")) {
    data <- jsonlite::fromJSON(
      "outputs/all_international_stats_enriched.json",
      simplifyVector = FALSE
    )

    rows <- lapply(data, function(player) {
      info <- player$player_info

      data.frame(
        cricinfo_id = as.character(info$cricinfo_id),
        unique_player_id = as.character(info$unique_player_id),
        final_player_name = as.character(info$final_player_name),
        source_country_text = as.character(info$final_country),
        stringsAsFactors = FALSE
      )
    })

    return(bind_rows(rows))
  }

  stop("No player index found.")
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

if (total_players == 0) {
  matrix <- list(include = list())
} else {
  starts <- seq(1, total_players, by = chunk_size)
  ends <- pmin(starts + chunk_size - 1, total_players)

  include <- lapply(seq_along(starts), function(i) {
    start <- starts[i]
    end <- ends[i]

    list(
      label = sprintf("chunk_%04d_%04d", start, end),
      start = start,
      end = end
    )
  })

  matrix <- list(include = include)
}

chunk_count <- length(matrix$include)

if (chunk_count > 256) {
  stop(
    paste0(
      "Too many chunks for one GitHub Actions matrix: ",
      chunk_count,
      ". Increase CHUNK_SIZE. Current CHUNK_SIZE=",
      chunk_size
    )
  )
}

write_json(
  matrix,
  "outputs/domestic_profile_matrix.json",
  auto_unbox = TRUE,
  pretty = FALSE
)

summary <- data.frame(
  total_players = total_players,
  chunk_size = chunk_size,
  chunk_count = chunk_count
)

write_csv(summary, "outputs/domestic_profile_matrix_summary.csv")

cat("Total valid Cricinfo players:", total_players, "\n")
cat("Chunk size:", chunk_size, "\n")
cat("Chunk count:", chunk_count, "\n")
cat("Saved outputs/domestic_profile_matrix.json\n")