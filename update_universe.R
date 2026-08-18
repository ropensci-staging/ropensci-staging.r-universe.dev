add_url <- function(entry) {
  review_url <- sprintf(
    "/repos/ropensci/software-review/issues/%s",
    entry$iss_no
  )
  text <- gh::gh(review_url)$body
  lines <- strsplit(text, "\n")[[1]]
  repo_url_line <- grep("<!--repourl-->", lines, value = TRUE)
  repo_url <- trimws(sub(
    "<!--repourl-->",
    "",
    sub("<!--end-repourl-->", "", sub("Repository:", "", repo_url_line))
  ))
  list(
    package = entry$pkgname,
    url = repo_url,
    metadata = list(
      review = list(
        organization = "rOpenSci Software Review",
        url = sprintf(
          "https://github.com/ropensci/software-review/issues/%s",
          entry$iss_no
        )
      )
    )
  )
}


jsonlite::read_json(
  "https://raw.githubusercontent.com/ropensci-org/badges/refs/heads/gh-pages/json/onboarded.json"
) |>
  purrr::keep(\(x) x$status == "pending") |>
  purrr::map(add_url) |>
  jsonlite::write_json("packages.json", auto_unbox = TRUE, pretty = TRUE)
