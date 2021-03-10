
#' The scalade logo, using ASCII or Unicode characters
#'
#' Use [crayon::strip_style()] to get rid of the colors.
#'
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#'   on UTF-8 platforms.
#'
#' @md
#' @export
#' @examples
#' scalade_logo()
scalade_logo_text <- function(unicode = l10n_info()$`UTF-8`) {
    logo <- c(
        "------+-----+-----+-----+-\\_\\-+-----+-----+-----+-----+------",
        "|     |  2 |     |     |  \\_\\|     |     |  6  |     |     |",
        "|     |     |     |     |   \\_\\     |     |     |     |     |",
        "------+-----+-----+-----+----\\_\\----+-----+-----+/_/--+------",
        "| 3  |     |     |     |     \\_\\   |     |     /_/   |     |",
        "|     |     |     |     |     |\\_\\  |     |    /_/    |     |",
        "------+-----+-----+/_/--+-----+-\\_\\-+-----+---/_/-----+------",
        "|     |  1  |     /_/   |  9 |  \\_\\|     |  /_/|     |     |",
        "|     |     |    /_/    |     |   \\_\\     | / / |     |     |",
        "------+-----+---/_/-----+----_|----\\_\\----+-----+-----+------",
        "|              /| |         | |     \\_\\   |  5 |     |     |",
        "|  ___  ___ __/_| | __ _  __| | ___  \\_\\  |     |     |     |",
        "| / __|/ __/ _` | |/ _` |/ _` |/ _ \\+-----+-----+-----+------",
        "| \\__ \\ (_| (_| | | (_| | (_| |  __/|     |     |     |     |",
        "| |___/\\___\\__,_|_|\\__,_|\\__,_|\\___||     |     |     |     |",
        "+-----+-----+-----+-----+-----+-----+-----+-----+-----+------"
    )

    hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
    if (unicode) hexa <- c("*" = " \u2b22", "o" = " \u2b21", "." = ".")[hexa]

    cols <- c(
        "red", "yellow", "green", "magenta", "cyan",
        "yellow", "green", "white", "magenta", "cyan"
    )

    col_hexa <- purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

    for (i in 0:9) {
        pat <- paste0("\\b", i, "\\b")
        logo <- sub(pat, col_hexa[[i + 1]], logo)
    }

    structure(crayon::blue(logo), class = "tidyverse_logo")
}

#' @export
scalade_logo <- function(x = scalade_logo_text(), ...) {
    cat(x, ..., sep = " \n")
    invisible(x)
}