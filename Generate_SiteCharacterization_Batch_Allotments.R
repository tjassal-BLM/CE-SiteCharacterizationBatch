# Title: Generate_SiteCharacterization_Batch_Allotments.R
# 10 April 2026
# Author: Tim Assal (adapted from Eric Jensen)
# generate multiple Climate Engine Site Characterization reports for a set of polygons
# similar to the reports found at: https://support.climateengine.org/article/148-using-site-characterization

#!! Please take note of the following: 
# 1. You will need a Climate Engine API key (see ~line 46) to run this script
# 2. You will need to define the ownership see ~line 58 (mask_ownership_default <- 'None')
# choices are: None, BIA, BLM, DOD, FWS,NPS, USFS)
# 3. Please change the title of your report (Line ~57)
# 4. Please input your email address (Line ~174)
# 5. You might need to pause and/or rerun the block of code from Line 221 to the end to allow time for the reports to be generated
# 6. Update location of output directory (Line ~283)
# 7. If the reports did not generated, open csv output file at end of code to check status of reports. 
# If the 'status' column states 'in progress' and 'download_success' is 'false, then wait a period of time for the reports to regenerate; then
# rerun the code from lines 221 to the end again. Check the status again. 

# Example input data
# The .shp included contains 5 records (3 legit allotments with a mix of BLM and private land, so 3 reports will be returned); 
# 1 record in US with no BLM land (will run with no land tenure mask); 1 record outside of US (no RAP coverage, therefore no report will be generated))
# this code is set to run using two input fields: 'ALLOT_NO' and 'ALLOT_NAME' (see lines 167-173)

# Questions? tassal@blm.gov


## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries-------------------------------------------------------------------------------------------------------------------------------------------------
# Load core libraries for HTTP requests, data wrangling,
# spatial operations, and JSON serialization
library(httr2)
library(tidyverse)
library(sf)
library(jsonlite)


## ----parameters------------------------------------------------------------------------------------------------------------------------------------------------
# Base URL & personal API token for Climate Engine
# (replace `key_here` with your own)
# ------------------------- Define parameters for Climate Engine API -------------------------------------

# Define root url for Climate Engine API
root_url <- 'https://api.climateengine.org/'

# Define Climate Engine API key
# Request a free API key from Climate Engine at https://support.climateengine.org/article/36-requesting-an-authorization-key-token
key = 'INSERT KEY HERE'

## ----generate_links--------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------- Generate report links for WY allotments -------------------------------------

# Read allotment polygons and transform to WGS84 lon/lat for API coordinates
allotments_path <- "SourceData/WY-test-allotments.shp"
endpoint <- 'reports/site_characterization/coordinates'
site_description_default <- 'Report for WY allotment'
mask_ownership_default <- 'None'
mask_landcover_default <- 'True'
batch_mode <- 'True'

allotments_to_run <- st_read(allotments_path, quiet = TRUE) |>
  st_transform(4326) |>
  st_make_valid()

pick_first <- function(...) {
  for (x in list(...)) if (!is.null(x) && length(x) > 0) return(x)
  NULL
}

as_single_text <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_character_)
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  value <- as.character(value)
  value <- value[!is.na(value) & nzchar(trimws(value))]
  if (length(value) == 0) NA_character_ else value[1]
}

normalize_text <- function(value) {
  txt <- as_single_text(value)
  if (is.na(txt) || !nzchar(trimws(txt))) NA_character_ else txt
}

is_error_like_message <- function(text) {
  text <- normalize_text(text)
  if (is.na(text)) return(FALSE)
  msg <- tolower(text)
  if (str_detect(msg, "generated successfully|success|report initiated|initiated|in progress|queued|submitted")) return(FALSE)
  str_detect(
    msg,
    "\\b4\\d\\d\\b|\\b5\\d\\d\\b|error|exception|invalid|bad request|forbidden|unauthorized|not found|failed|does not intersect|does not have valid pixels|no valid pixels|outside the continental us|outside conus|unable to|cannot\\b|missing required|is required"
  )
}

extract_status_from_text <- function(text) {
  text <- normalize_text(text)
  if (is.na(text)) return(NA_integer_)
  patterns <- c("(?i)\\b([45]\\d\\d)\\s+client\\s+error\\b", "(?i)\\bhttp\\s*[:=]?\\s*([45]\\d\\d)\\b", "(?i)\\bstatus\\s*[:=]?\\s*([45]\\d\\d)\\b", "^\\s*([45]\\d\\d)\\b")
  for (pattern in patterns) {
    match <- str_match(text, pattern)[, 2]
    if (!is.na(match) && nzchar(match)) return(as.integer(match))
  }
  NA_integer_
}

parse_response_body <- function(response, response_text) {
  body <- tryCatch(resp_body_json(response), error = function(e) NULL)
  if (!is.null(body)) return(body)
  if (nzchar(response_text) && jsonlite::validate(response_text)) {
    return(tryCatch(jsonlite::fromJSON(response_text, simplifyVector = FALSE), error = function(e) list()))
  }
  list()
}

extract_api_error <- function(response_body, response_text, http_status) {
  err <- normalize_text(pick_first(response_body$Error, response_body$error, response_body$Data$Error, response_body$Data$error))
  if (!is.na(err)) return(err)
  msg <- normalize_text(pick_first(response_body$message, response_body$Message, response_body$Data$message, response_body$Data$Message))
  if (is_error_like_message(msg)) return(msg)
  json_error <- str_match(response_text, '"Error"\\s*:\\s*"([^"]+)"')[, 2]
  if (!is.na(json_error) && nzchar(json_error)) return(json_error)
  if (http_status >= 400) return(normalize_text(str_sub(str_squish(response_text), 1, 500)))
  NA_character_
}

normalize_error_message <- function(error_message, message) {
  err <- normalize_text(error_message)
  msg <- normalize_text(message)
  if (is.na(err) && !is.na(msg) && is_error_like_message(msg)) msg else err
}

extract_http_status <- function(wrapper_status, response_body, response_text, message, error_message) {
  for (candidate in list(response_body$status_code, response_body$status, response_body$code, response_body$Data$status_code, response_body$Data$status, response_body$Data$code)) {
    parsed <- suppressWarnings(as.integer(str_match(as_single_text(candidate), "(\\d{3})")[, 2]))
    if (!is.na(parsed) && parsed >= 100 && parsed <= 599) return(parsed)
  }
  for (candidate in list(error_message, message, response_text)) {
    parsed <- extract_status_from_text(candidate)
    if (!is.na(parsed)) return(parsed)
  }
  wrapper_status <- as.integer(wrapper_status)
  if ((is.na(wrapper_status) || wrapper_status < 400) &&
      ((!is.na(error_message) && nzchar(error_message)) || is_error_like_message(message))) {
    return(400L)
  }
  wrapper_status
}

ring_to_coordinate_list <- function(ring_matrix) {
  apply(ring_matrix[, 1:2, drop = FALSE], 1, function(coord_pair) unname(as.numeric(coord_pair)), simplify = FALSE)
}

geometry_to_coordinates_json <- function(geom) {
  geom <- st_zm(geom, drop = TRUE, what = "ZM")
  if (inherits(geom, "POLYGON")) {
    coords <- lapply(geom, ring_to_coordinate_list)
  } else if (inherits(geom, "MULTIPOLYGON")) {
    coords <- lapply(geom, function(poly) lapply(poly, ring_to_coordinate_list))
  } else {
    stop("Unsupported geometry type: ", class(geom)[1])
  }
  toJSON(coords, auto_unbox = TRUE)
}

generate_report <- function(allotment_feature) {
  allot_id <- as.character(unlist(allotment_feature$ALLOT_NO))
  allot_name <- as.character(unlist(allotment_feature$ALLOT_NAME))
  site_name <- str_sub(allot_name, 1, 35)
  site_type <- str_sub(paste('Allotment', allot_id), 1, 50)
  coordinates_json <- geometry_to_coordinates_json(st_geometry(allotment_feature)[[1]])
  print(paste("Making request for allotment:", allot_name, "(", allot_id, ")"))

  params <- list(
    user_email = 'tassal@blm.edu',
    site_name = site_name,
    site_type = site_type,
    site_description = site_description_default,
    mask_ownership = mask_ownership_default,
    mask_landcover = mask_landcover_default,
    coordinates = coordinates_json,
    batch = batch_mode
  )

  response <- request(base_url = paste0(root_url, endpoint)) |>
    req_url_query(query = !!!params) |>
    req_headers(Authorization = key) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  wrapper_status <- resp_status(response)
  response_text <- tryCatch(resp_body_string(response), error = function(e) "")
  response_body <- parse_response_body(response, response_text)
  data_raw <- pick_first(response_body$Data, list())

  message <- normalize_text(pick_first(data_raw$Message, response_body$Message, response_body$message, NA_character_))
  report_link <- normalize_text(pick_first(data_raw$`Report link`, response_body$`Report link`, NA_character_))
  site_description_resp <- normalize_text(pick_first(data_raw$`Site description`, response_body$`Site description`, site_description_default))
  site_name_resp <- normalize_text(pick_first(data_raw$`Site name`, response_body$`Site name`, site_name))
  site_type_resp <- normalize_text(pick_first(data_raw$`Site type`, response_body$`Site type`, site_type))
  error_message <- normalize_error_message(extract_api_error(response_body, response_text, wrapper_status), message)

  list(
    Http_status = extract_http_status(wrapper_status, response_body, response_text, message, error_message),
    Data = list(
      Message = if (is.na(message) && is.na(error_message)) 'Report initiated' else message,
      `Report link` = report_link,
      `Site description` = site_description_resp,
      `Site name` = site_name_resp,
      `Site type` = site_type_resp
    ),
    Error = error_message,
    Raw_response = str_sub(str_squish(response_text), 1, 500)
  )
}

# Make requests for each allotment feature in the shapefile
allotment_features <- split(allotments_to_run, seq_len(nrow(allotments_to_run)))
response_list <- map(allotment_features, generate_report)


## ----write_unzip-----------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------- Write out and unzip the reports ----------------------------------------

status_from_response <- function(message, error_message, http_status) {
  msg <- tolower(coalesce(message, ""))
  if (!is.na(error_message) && nzchar(error_message)) return("failed")
  if (!is.na(http_status) && http_status >= 400) return("failed")
  if (is_error_like_message(msg)) return("failed")
  if (str_detect(msg, "generated successfully|success")) return("completed")
  if (str_detect(msg, "initiated|in progress|queued|submitted")) return("in progress")
  "in progress"
}

download_report_file <- function(report_link, destination_path) {
  warning_text <- NULL
  result_code <- withCallingHandlers(
    tryCatch(
      download.file(report_link, destfile = destination_path, mode = "wb", quiet = TRUE),
      error = function(e) e
    ),
    warning = function(w) {
      warning_text <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  if (inherits(result_code, "error")) return(list(success = FALSE, note = conditionMessage(result_code)))
  if (!file.exists(destination_path)) return(list(success = FALSE, note = "file was not created"))
  file_size <- file.info(destination_path)$size
  is_valid <- isTRUE(result_code == 0) && is.finite(file_size) && file_size > 0 && is.null(warning_text)
  if (!is_valid) {
    suppressWarnings(file.remove(destination_path))
    note <- pick_first(warning_text, "download incomplete or report not ready")
    return(list(success = FALSE, note = as.character(note)))
  }
  list(success = TRUE, note = NA_character_)
}

# Convert nested list to dataframe
response_df <- map_dfr(response_list, function(item) {
  data <- pick_first(item$Data, list())
  message <- normalize_text(pick_first(data$Message, NA_character_))
  report_link <- normalize_text(pick_first(data$`Report link`, NA_character_))
  site_description <- normalize_text(pick_first(data$`Site description`, NA_character_))
  site_name <- normalize_text(pick_first(data$`Site name`, NA_character_))
  site_type <- normalize_text(pick_first(data$`Site type`, NA_character_))
  error_message <- normalize_error_message(pick_first(item$Error, NA_character_), message)
  http_status <- as.integer(pick_first(item$Http_status, NA_integer_))
  raw_response <- normalize_text(pick_first(item$Raw_response, NA_character_))

  tibble(
    message = message,
    report_link = report_link,
    site_description = site_description,
    site_name = site_name,
    site_type = site_type,
    http_status = http_status,
    error_message = error_message,
    raw_response = raw_response,
    status = status_from_response(message, error_message, http_status)
  )
})

download_dir <- "Reports_Out/"
dir.create(download_dir, showWarnings = FALSE)
response_df <- response_df %>%
  rowwise() %>%
  mutate(
    local_path = if (!is.na(report_link) && nzchar(report_link)) file.path(download_dir, basename(report_link)) else NA_character_,
    download_result = list({
      if (status == "failed") {
        list(success = FALSE, note = "skipped due to API error")
      } else if (is.na(local_path)) {
        list(success = FALSE, note = "no report link returned")
      } else {
        download_report_file(report_link, local_path)
      }
    }),
    download_success = download_result$success,
    download_note = download_result$note
  ) %>%
  ungroup() %>%
  select(-download_result) %>%
  mutate(
    status = case_when(
      status == "failed" ~ "failed",
      download_success ~ "completed",
      TRUE ~ "in progress"
    )
  )
zip_files <- response_df %>%
  filter(download_success, !is.na(local_path), file.exists(local_path)) %>%
  pull(local_path) %>%
  unique()

for (zip_file in zip_files) {
  print(zip_file |> str_replace('.zip', ''))
  dir.create(zip_file |> str_replace('.zip', ''), showWarnings = FALSE)
# Unzip each successfully downloaded archive into its own sub‑folder
  tryCatch(
    unzip(zip_file, exdir = zip_file |> str_replace('.zip', '')),
    error = function(e) {
      message(paste("Failed to unzip", zip_file, ":", conditionMessage(e)))
    }
  )
}

# Write row-level status summary for reruns/debugging
write_csv(response_df, file.path(download_dir, "report_status_summary.csv"), na = "")

