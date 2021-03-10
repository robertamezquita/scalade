# #' Update scalade packages
# #'
# #' This will check to see if all scalade packages (and optionally, their
# #' dependencies) are up-to-date, and will install after an interactive
# #' confirmation.
# #'
# #' @inheritParams scalade_deps
# #' @export
# #' @examples
# #' \dontrun{
# #' scalade_update()
# #' }
# scalade_update <- function(recursive = FALSE, repos = getOption("repos")) {
#     deps <- scalade_deps(recursive, repos)
#     behind <- dplyr::filter(deps, behind)

#     if (nrow(behind) == 0) {
#         cli::cat_line("All scalade packages up-to-date")
#         return(invisible())
#     }

#     cli::cat_line("The following packages are out of date:")
#     cli::cat_line()
#     cli::cat_bullet(format(behind$package), " (", behind$local, " -> ", behind$cran, ")")

#     cli::cat_line()
#     cli::cat_line("Start a clean R session then run:")

#     pkg_str <- paste0(deparse(behind$package), collapse = "\n")
#     cli::cat_line("install.packages(", pkg_str, ")")

#     invisible()
# }

packageVersion <- function(pkg) {
    if (rlang::is_installed(pkg)) {
        utils::packageVersion(pkg)
    } else {
        0
    }
}