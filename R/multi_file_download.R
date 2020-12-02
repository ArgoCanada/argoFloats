
multi_file_download <- function(url, dest, quiet = FALSE) {
  stopifnot(
    is.character(url),
    is.character(dest)
  )

  # recycle url along dest (could want to query one url many times)
  url <- rep_len(url, length(dest))

  if (length(url) == 0) {
    return(invisible(character(0)))
  }

  # download the files!
  error_message <- vapply(seq_along(url), function(i) {
    if (!quiet) message(glue("'{ url[i] }' => '{ dest[i] }'"))
    if (!dir.exists(dirname(dest[i]))) {
      dir.create(dirname(dest[i]), recursive = TRUE, showWarnings = FALSE)
    }

    tryCatch({
      curl::curl_download(url[i], dest[i])
      NA_character_
    }, error = function(e) {
      # If this is a user interrupt, stop completely
      if (identical(e$message, "Operation was aborted by an application callback")) {
        abort(e$message) # nocov
      }

      paste0(e, collapse = "\n")
    })
  }, character(1))

  # check that all were downloaded
  has_error <- !is.na(error_message)
  if (any(has_error)) {
    missing_paths <- dest[has_error]
    missing_paths_lab <- paste0(
      "'", utils::head(missing_paths, 10), "': ", error_message[has_error],
      collapse = "\n"
    )
    files <- if (length(missing_paths) != 1) "files" else "file"
    abort(
      glue(
        "{ length(missing_paths) } { files } failed to download:\n{missing_paths_lab}"
      )
    )
  }

  invisible(dest)
}

multi_file_download_async <- function(url, dest, quiet = FALSE) {
  stopifnot(
    is.character(url),
    is.character(dest)
  )

  # recycle url along dest (could want to query one url many times)
  url <- rep_len(url, length(dest))

  # create a mutable object that keeps track of success/failure
  results <- new.env(parent = emptyenv())

  if (length(url) == 0) {
    return(invisible(character(0)))
  }

  pool <- curl::new_pool(total_con = 6, host_con = 6)

  for (i in seq_along(url)) {
    if (!quiet) message(glue::glue("Fetching '{ url[i] }'"))
    results[[paste(url[i], dest[i])]] <- FALSE
    curl::curl_fetch_multi(
      url[i],
      multi_download_async_success(url[i], dest[i], results, quiet = quiet),
      multi_download_async_failure(url[i], dest[i], results, quiet = quiet),
      pool = pool
    )
  }

  # this will block as long as files are being downloaded
  curl::multi_run(pool = pool)

  n_error <- sum(!unlist(as.list(results)))

  if (n_error > 0) {
    files <- if (n_error != 1) "files" else "file"
    abort(glue("{ n_error }/{ length(url) } { files } failed to download."))
  }

  invisible(dest)
}

multi_download_async_success <- function(url, dest, results, quiet = FALSE) {
  force(url)
  force(dest)
  force(quiet)
  force(results)

  function(res) {
    if (res$status_code >= 300) {
      if (!quiet) message(glue("Fetching '{ url }' failed with status { res$status_code }"))
      results[[paste(url, dest)]] <- FALSE
      return()
    }

    if (!quiet) message(glue("Writing '{ dest }'"))

    if (!dir.exists(dirname(dest))) dir.create(dirname(dest), recursive = TRUE)
    con <- file(dest, "wb")
    on.exit(close(con))

    writeBin(res$content, con)
    results[[paste(url, dest)]] <- TRUE
  }
}

multi_download_async_failure <- function(url, dest, results, quiet = FALSE) {
  force(url)
  force(dest)
  force(quiet)
  force(results)

  function(msg) {
    if (!quiet) message(glue::glue("Fetching '{ url }' failed ({ msg })"))
    results[[paste(url, dest)]] <- FALSE
  }
}
