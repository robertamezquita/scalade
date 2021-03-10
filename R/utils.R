msg <- function(..., startup = FALSE) {
    if (startup) {
        if (!isTRUE(getOption("scalade.quiet"))) {
            packageStartupMessage(text_col(...))
        }
    } else {
        message(text_col(...))
    }
}

text_col <- function(x) {
    # If RStudio not available, messages already printed in black
    if (!rstudioapi::isAvailable()) {
        return(x)
    }

    if (!rstudioapi::hasFun("getThemeInfo")) {
        return(x)
    }

    theme <- rstudioapi::getThemeInfo()

    if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
}

#' List all packages in the scalade
#'
#' @param include_self Include scalade in the list?
#' @export
#' @examples
#' scalade_packages()
scalade_packages <- function(include_self = TRUE) {
    raw <- utils::packageDescription("scalade")$Imports
    imports <- strsplit(raw, ",")[[1]]
    parsed <- gsub("^\\s+|\\s+$", "", imports)
    names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

    if (include_self) {
        names <- c(names, "scalade")
    }

    names
}

invert <- function(x) {
    if (length(x) == 0) {
        return()
    }
    stacked <- utils::stack(x)
    tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
    crayon::style(
        paste0(...),
        crayon::make_style(grDevices::grey(level), grey = TRUE)
    )
}