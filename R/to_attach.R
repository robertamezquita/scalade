# core <- ""

core_unloaded <- function(core) {
    search <- paste0("package:", core)
    core[!search %in% search()]
}

# Attach the package from the same package library it was
same_library <- function(pkg) {
    loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
    do.call(
        "library",
        list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
    )
}

scalade_attach <- function(id, core) {
    to_load <- core_unloaded(core)
    if (length(to_load) == 0) {
        return(invisible())
    }

    msg(
        cli::rule(
            left = crayon::bold("Attaching packages"),
            right = id # paste0("scalade ", package_version("scalade"))
        ),
        startup = TRUE
    )

    versions <- vapply(to_load, package_version, character(1))
    packages <- paste0(
        crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
        crayon::col_align(versions, max(crayon::col_nchar(versions)))
    )

    if (length(packages) %% 2 == 1) {
        packages <- append(packages, "")
    }
    col1 <- seq_len(length(packages) / 2)
    info <- paste0(packages[col1], "     ", packages[-col1])

    msg(paste(info, collapse = "\n"), startup = TRUE)

    suppressPackageStartupMessages(
        lapply(to_load, same_library)
    )

    invisible()
}

package_version <- function(x) {
    version <- as.character(unclass(utils::packageVersion(x))[[1]])

    if (length(version) > 3) {
        version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
    }
    paste0(version, collapse = ".")
}

to_attach <- function(id, core, report_conflicts = FALSE) {
    needed <- core[!is_attached(core)]
    if (length(needed) == 0) {
        return()
    }

    crayon::num_colors(TRUE)
    scalade_attach(id, core)

    if (report_conflicts) {
        if (!"package:conflicted" %in% search()) {
            x <- scalade_conflicts(core)
            msg(scalade_conflict_message(x), startup = TRUE)
        }
    }
}

is_attached <- function(x) {
    paste0("package:", x) %in% search()
}