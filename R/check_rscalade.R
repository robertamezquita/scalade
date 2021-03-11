#' Helper to check for .Rscalade in project, user home, or specified
#'
#' Alerts regarding if .Rscalade location was inferred
#'
#' Only allows for the alert to happen once in a session to prevent yelling.
#' And furthermore only in interactive mode.
.not_alerted <- function() {
    # Skip messages if running as a script
    if (!interactive()) {
        return(FALSE)
    }
    opt <- "scalade.not_alerted"
    if (is.null(getOption(opt))) {
        options("scalade.not_alerted" = FALSE)
        return(TRUE) # not alerted to inference
    }
    if (!is.null(getOption(opt)) & !getOption(opt)) {
        return(FALSE) # already has been alerted to inference
    }
}

#' Mega helper to suss out the rscalade location
#'
#' Checks for a ladder definition. See details.
#'
#' @details
#' This function looks for a .Rscalade file as follows:
#'
#' * first checks the `.scalade` path if provided
#'   * note this does NOT set `options(scalade.rscalade)` to allow for scalade switching;
#'     use `scalade::affix()` for that
#' * if .scalade is not provided, then the following occur in order to infer the location:
#'   * checks the global options for a preset rscalade path, `getOption("scalade.rscalade")`
#'   * checks for a project .Rscalade (if found, this sets `options(scalade.rscalade = .local_scalade)`)
#'   * checks for a home .Rscalade (if found, this sets `options(scalade.rscalade = .home_scalade)`)
#'
#' @param .scalade Path to an existing .Rscalade definition. If not provided, walks through inference
#'   steps.
#'
#' @name check
#' @export
.check_rscalade <- function(.scalade = NULL) {
    # If provided, first check for that; abort if not found ---
    if (!is.null(.scalade)) {
        .provided_scalade_exists <- fs::file_exists(.scalade)
        if (!.provided_scalade_exists) {
            cli::cli_alert_danger(
                c(".Rscalade was not found at: ", crayon::red(.scalade))
            )
            return(invisible())
        }
        if (.provided_scalade_exists) {
            if (.not_alerted()) {
                cli::cli_alert_success("Setting .Rscalade for this session to: ", crayon::green(.scalade))
            }
            # options(scalade.rscalade = .scalade) # .scalade may change within a sesh; only set as opt explicitly
            # or with inference
            return(.scalade)
        }
    }

    # Automagic inference of .Rscalade ----
    if (is.null(.scalade)) {
        # Check if its already been set previously as an opt ..
        .opt_scalade <- getOption("scalade.rscalade")
        if (!is.null(.opt_scalade)) {
            if (.not_alerted()) {
                cli::cli_alert_success(
                    c(
                        "Using preset .Rscalade from ",
                        crayon::green("`options(scalade.rscalade)`"), ":\n",
                        crayon::green(c("  ", .opt_scalade))
                    )
                )
            }
            return(.opt_scalade)
        }

        # Next check the local project ..
        .local_scalade <- here::here(".Rscalade")
        .local_scalade_exists <- fs::file_exists(.local_scalade)
        if (.local_scalade_exists) {
            if (.not_alerted()) {
                cli::cli_alert_success(
                    c(
                        "Using local project .Rscalade found in: ",
                        crayon::green(here::here())
                    )
                )
            }
            options(scalade.rscalade = .local_scalade)
            return(.local_scalade)
        }

        # Last check the user home dir (~/)
        if (!.local_scalade_exists) {
            .home_scalade <- fs::path_expand("~/.Rscalade")
            .home_scalade_exists <- fs::file_exists(.home_scalade)

            # It exists! Use the homedir .Rscalade
            if (.home_scalade_exists) {
                if (.not_alerted()) {
                    cli::cli_alert_success(
                        c(
                            "No local project .Rscalade found; using home directory .Rscalade: ",
                            crayon::green(.home_scalade)
                        )
                    )
                }
                options(scalade.rscalade = .home_scalade)
                return(.home_scalade)
            }

            # No setting, no opt, no local, no home scalade .. abort and alert!
            if (!.home_scalade_exists) {
                rlang::abort(message = c(
                    cli::cli_alert_danger(
                        c(".Rscalade was not specified, preset via `options()`, nor was it found in the local project or home directory.")
                    ),
                    cli::cli_alert_warning(
                        c(
                            "Project directory is currently set to:\n    ",
                            crayon::red("  ", here::here())
                        )
                    ),
                    cli::cli_alert_warning(
                        c(
                            "Home directory is set to:\n    ",
                            crayon::red("  ", fs::path_expand("~"))
                        )
                    ),
                    cli::cli_alert_info(
                        cat(
                            "To fix this, do one of the following: \n",
                            "* provide a path to an existing .Rscalade file via the ", crayon::green("`.scalade`"), " argument\n",
                            "* provide a path to an existing .Rscalade by setting ", crayon::green("`options(scalade.rscalade_global = '/path/to/.Rscalade')`"), "\n",
                            "* automagically create a new one via ", crayon::green("`scalade::setup()`")
                        )
                    )
                ))
            }
        }
    }
}