#' @title Creating a fresh .Rscalade definition file
#'
#' @description
#' This function should be the first one run out of the box, as it creates a
#' baseline definition file that contains predefined ladders.
#'
#' @details
#' This package relies on having an available ladder definition file
#' (.Rscalade). Thus, to use the package, a starting version must be created
#' via `scalade::fresh()`.
#'
#' @param mode One of three options. "home" will create a .Rscalade in your user home
#'   directory. "local" will create a project specific .Rscalade (where your project
#'   location is defined by `here::here()`). "manual" will use the provided .scalade
#'   argument.
#' @param overwrite If there is an existing .Rscalade file, whether to overwrite it
#'   with the predefined ladder definitions from this package.
#' @param .scalade Manually provided desired path for the fresh .Rscalade.
#'
#' @return None; called for side-effect of writing ~/.Rscalade
#'
#' @name setup
#' @export
setup <- function(mode = "home", overwrite = FALSE, .scalade = NULL) {
    if (mode == "home") {
        .scalade <- fs::path_expand("~/.Rscalade")
    }
    if (mode == "local") {
        .scalade <- here::here()
    }
    scalade_exists <- fs::file_exists(.scalade)

    if (scalade_exists & overwrite == FALSE) {
        cli::cli_alert_danger(
            c(
                "A .Rscalade file already exists at:\n ",
                crayon::red("  `", .scalade, "`\n "),
                " Set ", crayon::red("`overwrite = TRUE`"),
                " to replace with the base prebuilt spec from this package, which can be seen via:\n",
                crayon::green("  `data(scalade_prebuilt)`")
            )
        )
        return(invisible())
    }

    data(scalade_prebuilt)
    readr::write_tsv(scalade_prebuilt, .scalade,
        col_names = TRUE, append = FALSE
    )
    return(invisible())
}

#' Affix the session .Rscalade file to use via options()
#'
#' A little helper that does the hard part of writing out
#' `options(scalade.rscalade = "/path/to/.Rscalade")` so that all
#' functions know where to look for the .Rscalade. Overwrites any previously
#' set .Rscalade in options.
#'
#' @param .scalade Path to existing .Rscalade file.
#'
#' @return None; called for the side effect of setting the location of an existing .Rscalade
#'   file via `options(scalade.rscalade = "/path/to/.Rscalade")`.
#'
#' @name affix
#' @export
affix <- function(.scalade) {
    scalade_exists <- fs::file_exists(.scalade)
    if (scalade_exists) {
        if (interactive()) {
            cli::cli_alert_success(
                c(
                    "Setting .Rscalade to existing ladder definition:\n",
                    crayon::green("   ", .scalade)
                )
            )
        }
        options(scalade.rscalade = .scalade)
    }
    if (!scalade_exists) {
        rlang::abort(message = c(
            cli::cli_alert_danger(
                c("No .Rscalade file exists at:\n", crayon::red("  ", .scalade))
            ),
            cli::cli_alert_info(
                c("Create a new .Rscalade file via ", crayon::green("`scalade::fresh()`"))
            )
        ))
    }
}



#' @title Edit existing ladder definitions interactively
#'
#' @description
#' Add, remove, and edit existing ladder definitions interactively. `scalade::liveedit()` will
#' pop open an interactive editor (as set by $EDITOR).
#'
#' @details Repos
#' Note that for packages that are stored in remote (Github) repositories (`repo = "Remote"`)
#' the package name must be in a format compatible with `remotes::install_remote`,
#' e.g. `<gh-username>/<repo>`.
#'
#' @param .scalade A .Rscalade ladder definition file to live edit.
#' @param repo
#'
#' @return None; called for the side effect of editing the ~/.Rscalade file.
#'
#' @name editlive
#' @export
liveedit <- function(.scalade = NULL) {
    .scalade <- .check_rscalade(.scalade)
    usethis::edit_file(.scalade)
}

#' @title Edit existing ladder definitions via construction and destruction
#'
#' @description
#' Add, remove, and edit existing ladder definitions programmatically. `scalade::build()` will
#' take arguments to build a new ladder to append to an existing .Rscalade file (or create a new one
#' if it doesn't exist). `scalade::burn()` will destroy a ladder from an existing .Rscalade file.
#'
#' @param id.spec The ID to assign to the ladder.
#' @param repo The repo where the packages live (one of "CRAN", "Bioconductor", or "Remote" (Github))
#' @param packages The list of packages to associate with the new ladder.
#' @param .scalade The path to an existing or new .Rscalade file.
#'
#' @rdname edit
#' @export
construct <- function(id.spec, repo, packages, .scalade = NULL) {
    .scalade <- .check_rscalade(.scalade)

    if (!(repo %in% c("CRAN", "Bioconductor", "Remote"))) {
        cli::cli_alert_warning(
            paste0(
                crayon::red("`repo` "), "must be one of ",
                crayon::blue("`CRAN`, `Bioconductor`"), " or ",
                crayon::blue("`Remote`")
            )
        )
        return(invisible())
    }

    new <- tibble::tibble(
        id = id.spec, repo = repo, package = packages
    )

    readr::write_tsv(new, .scalade,
        col_names = TRUE, append = TRUE
    )

    return(invisible())
}

#' @name edit
#' @export
burn <- function(id.spec = NULL, overwrite = FALSE, .scalade = NULL) {
    .scalade <- .check_rscalade(.scalade)
    ladders_exist <- fs::file_exists(.scalade)
    if (ladders_exist & overwrite == FALSE) {
        cli::cli_alert_warning(
            c(
                crayon::red("`~/.Rscalade`"),
                " ladder spec will be changed.",
                " Set ", crayon::red("`overwrite = TRUE`"),
                " to remove specified ladders in ", crayon::red("`id.spec`.")
            )
        )
        invisible()
    }

    scalade <- readr::read_tsv(.scalade, col_types = "ccc")
    if (!is.null(id.spec)) {
        scalade <- scalade %>%
            dplyr::filter(!(id %in% id.spec))
    }

    readr::write_tsv(scalade, .scalade,
        col_names = TRUE, append = FALSE
    )
}

#' @title View defined ladders in a pretty-printed manner
#'
#' @description
#' This will pretty-print ladders (all by default, or specific ones)
#' that have already been defined in ~/.Rscalade.
#'
#' @param id.spec Specified ladder ID to inspect. If not specified shows
#'   all available ladders with their associated packages.
#' @param .scalade Location of the ladder definition file. Default is NULL.
#'   If NULL (e.g. not set), it first searches for a .Rscalade file in the
#'   project root (as defined by there `here` package), and failing that, will
#'   search the user's home directory ("~/.Rscalade").
#'
#' @return Invisibly returns the tibble of ladder definitions. In the
#'   printed output, a checkmark notes that the package is already installed
#'   on your host computer, while a cross signifies the package is not
#'   currently available.
#'
#' @name view
NULL

#' @rdname view
#' @export
inspect <- function(id.spec = NULL, .scalade = NULL) {
    .scalade <- .check_rscalade(.scalade)
    scalade <- readr::read_tsv(.scalade, col_types = "ccc") %>%
        dplyr::group_by(id, repo) %>%
        tidyr::nest() %>%
        dplyr::ungroup()

    if (!all(id.spec %in% scalade$id)) {
        id.spec = id.spec[id.spec %in% scalade$id]
        id.missing = id.spec[!(id.spec %in% scalade$id)]
        if (length(id.spec) == 0) {
            rlang::abort(c(
                "None of the provided ladder IDs were found in the .Rscalade:\n",
                crayon::red(.scalade))
            )
        }
        cli::cli_alert_warning("The following ladder IDs were not found: ",
            crayon::red(paste(id.missing, sep = ", ")))
    }

    .try_pkgver <- function(p) {
        tryCatch(package_version(p), error = function(e) "NA")
    }
    .pretty_print <- function(id, repo, package_col) {
        msg(cli::rule(
            left = crayon::bold(id),
            right = paste0(repo)
        ))
        to_load <- package_col$package
        versions <- vapply(to_load, .try_pkgver, character(1))
        packages <- paste0(
            ifelse(
                versions == "NA",
                crayon::red(cli::symbol$cross),
                crayon::green(cli::symbol$tick)
            ), " ", crayon::blue(format(to_load)), " ",
            crayon::col_align(versions, max(crayon::col_nchar(versions)))
        )
        if (length(packages) %% 2 == 1) {
            packages <- append(packages, "")
        }
        col1 <- seq_len(length(packages) / 2)
        info <- paste0(packages[col1], "     ", packages[-col1])
        msg(paste(info, collapse = "\n"))
        msg("\n")
    }

    if (!is.null(id.spec)) {
        scalade <- scalade %>%
            dplyr::filter(id %in% id.spec)
    }

    purrr::pwalk(
        list(id = scalade$id, repo = scalade$repo, package_col = scalade$data),
        .pretty_print
    )

    invisible(scalade)
}


#' @title Attaches packages specified by ladders
#'
#' @description
#' Loads up the family of packages associated with the designated
#' ladders.
#'
#' @inheritParams inspect
#' @param report_conflicts Whether to show the function conflicts that
#'   arise from the loaded packages. Currently set by default to FALSE
#'   as it can be quite noisy.
#'
#' @name load
NULL

#' @rdname load
#' @export
climb <- function(id.spec, report_conflicts = FALSE, .scalade = NULL) {
    .scalade <- .check_rscalade(.scalade)
    scalade <- readr::read_tsv(.scalade, col_types = "ccc") %>%
        dplyr::filter(id %in% id.spec)

    if (nrow(scalade) == 0) {
        cli::cli_alert_warning(
            paste0(
                crayon::red("`id.spec` "), "must be one of ",
                "the id values present in the ladder definitions file ",
                crayon::red("`~/.Rscalade`")
            )
        )
        return(invisible())
    }

    id.out <- paste0(id.spec, collapse = ", ")
    to_attach(id.out, scalade$package, report_conflicts = report_conflicts)
}


#' @title Install & upgrade packages from ladders
#'
#' @description
#' Keep your ladder up to date by moving from a source repo the latest
#' version of a package to your computer (hence, transport).
#'
#' @details Repos
#' Note that at the moment this does not take into account if a package
#' is in two or more repos and will install first from Bioconductor, then CRAN,
#' then Remote (Github) repositories.
#'
#' @inheritParams inspect
#' @param ... Additional arguments to `remotes::install_*`.
#'
#' @name install
#' @export
transport <- function(id.spec = NULL, .scalade = NULL, ...) {
    .scalade <- .check_rscalade(.scalade)

    scalade <- readr::read_tsv(.scalade, col_types = "ccc") %>%
        dplyr::filter(id %in% id.spec) %>%
        dplyr::arrange(package, repo) %>%
        dplyr::filter(!duplicated(package)) %>%
        dplyr::select(-id) %>%
        dplyr::group_by(repo) %>%
        tidyr::nest()

    .install <- function(repo, package_col) {
        package_vctr <- package_col$package
        if (repo == "CRAN") {
            remotes::install_cran(package_vctr, ...)
        }
        if (repo == "Bioconductor") {
            remotes::install_bioc(package_vctr, ...)
        }
        if (repo == "Remote") {
            remotes::install_remote(package_vctr, ...)
        }
    }

    purrr::walk2(scalade$repo, scalade$data, .install)
}