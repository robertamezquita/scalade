#' @title Getting started - creating a ~/.Rscalade definition file
#'
#' @description
#' This function should be the first one run out of the box, as it creates a
#' baseline definition file that contains predefined ladders.
#'
#' @details Definitions
#' This package relies on a centrally cached definitions file that is created
#' upon first setting up the package with `setup_ladders()` .
#'
#' @param overwrite Whether to overwrite an existing ~/.Rscalade file with
#'   the original base predefined set of ladders from this package.
#'
#' @return None; called for side-effect of writing ~/.Rscalade
#'
#' @name setup_ladders
NULL

#' @rdname setup_ladders
#' @export
setup_ladders <- function(overwrite = FALSE, .scalade = NULL) {
    if (is.null(.scalade)) {
        .scalade <- fs::path_expand("~/.Rscalade")
    }
    ladders_exist <- fs::file_exists(.scalade)

    if (ladders_exist & overwrite == FALSE) {
        cli::cli_alert_warning(
            c(
                crayon::red("`~/.Rscalade`"),
                " ladder spec file already exists.",
                " Set ", crayon::red("`overwrite = TRUE`"),
                " to replace with the base spec."
            )
        )
        invisible()
    }

    readr::write_tsv(scalade_prebuilt_ladders, .scalade,
        col_names = TRUE, append = FALSE
    )
}

#' Editing of ladders ---------------------------------------------------------

#' @title Edit the ladder definitions
#'
#' @description
#' Add, remove, and edit existing ladder definitions.
#' 
#' @details Repos
#' Note that for packages that are stored in remote (Github) repositories,
#' the package name must be in a format compatible with `remotes::install_remote`,
#' e.g. `<gh-username>/<repo>`.
#'
#' @param id.spec Specified ladder ID to work with.
#'
#' @return None; called for the side effect of editing the ~/.Rscalade file.
#'
#' @name edit_ladders
NULL

#' @rdname edit_ladders
#' @export
edit_ladders <- function(.scalade = NULL) {
        if (is.null(.scalade)) {
        .scalade <- fs::path_expand("~/.Rscalade")
    }
    usethis::edit_file(.scalade)
}

#' @rdname edit_ladders
#' @export
build_ladder <- function(id, repo, packages, .scalade = NULL) {
    if (is.null(.scalade)) {
        .scalade <- fs::path_expand("~/.Rscalade")
    }

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

    new_ladder <- tibble::tibble(
        id = id, repo = repo, package = packages
    )

    readr::write_tsv(new_ladder, .scalade,
        col_names = TRUE, append = TRUE
    )

    return(invisible())
}

#' @name edit_ladders
#' @export
burn_ladder <- function(id.spec = NULL, overwrite = FALSE, .scalade = NULL) {
    if (is.null(.scalade)) {
        .scalade <- fs::path_expand("~/.Rscalade")
    }
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

#' Pretty-printing ---------------------------------------------------------

#' @title View defined ladders in a pretty-printed manner
#'
#' @description
#' This will pretty-print ladders (all by default, or specific ones)
#' that have already been defined in ~/.Rscalade.
#'
#' @param id.spec Specified ladder ID to inspect. If not specified shows
#'   all available ladders with their associated packages.
#'
#' @return Invisibly returns the tibble of ladder definitions. In the
#'   printed output, a checkmark notes that the package is already installed
#'   on your host computer, while a cross signifies the package is not
#'   currently available.
#'
#' @name view_ladders
NULL

#' @rdname view_ladders
#' @export
inspect_ladders <- function(id.spec = NULL, .scalade = NULL) {
    if (is.null(.scalade)) {
        .scalade <- fs::path_expand("~/.Rscalade")
    }
    scalade <- readr::read_tsv(.scalade, col_types = "ccc") %>%
        dplyr::group_by(id, repo) %>%
        tidyr::nest() %>%
        dplyr::ungroup()

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


#' Attach ---------------------------------------------------------------------

#' @title Attaches packages specified by ladders
#'
#' @description
#' Loads up the family of packages associated with the designated
#' ladders.
#'
#' @param id.spec A string or character vector with the ladder IDs
#'   from which to get the packages to load based on the definitions in
#'   ~/.Rscalade.
#' @param report_conflicts Whether to show the function conflicts that
#'   arise from the loaded packages. Currently set by default to FALSE
#'   as it can be quite noisy.
#'
#' @name load_ladders
NULL

#' @rdname load_ladders
#' @export
climb_ladder <- function(id.spec, report_conflicts = FALSE, .scalade = NULL) {
    if (is.null(.scalade)) {
        .scalade <- fs::path_expand("~/.Rscalade")
    }
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


#' Upgrades ------------------------------------------------------------------

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
#' @param id.spec Ladders for which to install/upgrade.
#' @param ... Additional arguments to `remotes::install_*`.
#'
#' @name install_ladders
#' @export
transport_ladder <- function(id.spec = NULL, .scalade = NULL, ...) {
    if (is.null(.scalade)) {
        .scalade <- fs::path_expand("~/.Rscalade")
    }

    scalade <- readr::read_tsv(.scalade, col_types = "ccc") %>%
        dplyr::filter(id %in% id.spec) %>%
        dplyr::arrange(package, repo) %>%
        dplyr::filter(!duplicated(package)) %>%
        dplyr::select(-id) %>%
        dplyr::group_by(repo) %>%
        tidyr::nest()
    
    .install = function(repo, package_col) {
        package_vctr = package_col$package
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
