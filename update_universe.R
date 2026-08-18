reformat <- function(issue) {
  text <- issue$body
  lines <- strsplit(text, "\n")[[1]]
  repo_url_line <- grep("<!--repourl-->", lines, value = TRUE)
  repo_url <- trimws(sub(
    "<!--repourl-->",
    "",
    sub("<!--end-repourl-->", "", sub(".*http", "http", repo_url_line))
  ))

  pkg_line <- grep("^Package: ", lines, value = TRUE)
  pkgname <- sub("^Package: ", "", pkg_line)

  info <- list(
    package = pkgname,
    url = repo_url,
    metadata = list(
      review = list(
        organization = "rOpenSci Software Review",
        url = issue$html_url
      )
    )
  )

  # https://github.com/ropensci/software-review/issues/775#issuecomment-4845685249
  subdir_line <- grep("^Sub-directory: ", lines, value = TRUE)
  if (length(subdir_line) != 0) {
    info$subdir <- sub("^Sub-directory: ", "", subdir_line)
  }

  info
}

gh::gh(
  "/repos/ropensci/software-review/issues",
  .limit = Inf,
  label = "1/editor-checks,2/seeking-reviewer(s),3/reviewer(s)-assigned,4/review(s)-in-awaiting-changes,5/awaiting-reviewer(s)-response"
) |>
  purrr::keep(\(x) is.null(x$pull_request)) |>
  purrr::map(reformat) |>
  purrr::compact() |>
  jsonlite::write_json("packages.json", auto_unbox = TRUE, pretty = TRUE)
