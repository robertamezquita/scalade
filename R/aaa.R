# Create a predefined set of ladders for common use cases

# CRAN -----------------------------------------------------------------------
CRAN_pkgs <- list(
    # Misc required by pkg ---
    pretty_prompt = c(
        "cli", # prettier command line
        "crayon", # colorization
        "pillar" # aligning columns for tabular display
    ),

    # Developer ---
    development = c(
        "devtools", # developer toolkit
        # "pak", # modern package manager - not ready for primetime
        "usethis", # automates package setup
        "remotes" # manages installations
    ),

    # Core tidyverse programming utils ---
    tidypkgs = c(
        "rlang", # core tidyeval and core-r rewritten
        "dplyr", # data swiss army knife
        "tidyr", # pivots of data
        "hms", # prettified time of day
        "purrr", # functional programming
        "tibble", # data frame alternate
        "stringr", # string manipulation
        "forcats", # factor manipulation
        "fs", # file system management
        "glue", # a better paste
        "readr", # reading of tables directly into memory
        "vroom", # faster reading by lazily reading & indexing,
        "dbplyr" # database backend for dplyr
    ),

    # Networks ---
    networks = c(
        "tidygraph", # network analysis/composition
        "ggraph", # network plotting
        "particles" # verbiage for force-directed layouts (d3-force.js based)
    ),

    # Static Visualization ---
    viz_static = c(
        "ggplot2", # core plotting lib
        "httpgd", # browser-based graphics device
        "ragg", # graphics device
        "patchwork", # puts together multiple plots into a single layout
        "ggforce", # a bunch of ggplot2 extras (facet zoom, convex hulls, etc)
        "scico", # continuous colour schema
        "hierarchicalSets" # tree-like visualization akin to upSet plots
    ),

    # Dynamic Visualization ---
    viz_dynamic = c(
        "gganimate", # core animation engine
        "transformr" # interpolation between states
    ),

    # Generative art ---
    viz_generative = c(
        "ambient"
    )
)

# Bioconductor ---------------------------------------------------------------
Bioc_pkgs <- list(
    # single-cell methods ---
    bioc_sc_methods = c(
        "SingleCellExperiment", # core data struct
        "scuttle", # swiss army knife of tooling
        "scran", # normalization
        "DropletUtils", # droplet processing
        "batchelor", # batch correction/integration
        "zellkonverter", # converts python annData to sce
        "scDblFinder", # methods for doublet identification
        "SingleR" # annotation engine
    ),

    # single-cell data ---
    bioc_sc_data = c(
        "celldex", # reference datasets for annotations
        "scRNAseq", # many example datasets
        "TENxBrainData" # brain scrnaseq
    ),

    # generalized developer facing tooling ---
    bioc_general_methods = c(
        "BiocNeighbors", # nearest neighbor algorithms (KNN, Annoy)
        "BiocSingular", # singular value decomposition and related
        "bluster", # clustering methods
        "metapod" # meta-analyses - combining p-values
    ),

    # bioconductor developer ---
    bioc_development = c(
        "BiocGenerics",
        "S4Vectors",
        "GenomicRanges",
        "SummarizedExperiment"
    )
)

# Remote (github) projects ---
remote_pkgs <- list(
    viz_generative = c(
        "djnavarro/jasmines"
    )
    # Extra tidyverse utils only on Github ---
    # tidypkgs_gh = c(
    # "r-lib/clock" # date/time management (future replacement for lubridate)
    # )
)

# ---
pkg_compendium <- list(
    CRAN = CRAN_pkgs,
    Bioconductor = Bioc_pkgs,
    Remote = remote_pkgs
)

scalade_prebuilt_ladders <- tibble::tibble(
    repo = names(pkg_compendium),
    v = purrr::map(pkg_compendium, ~.)
) %>%
    tidyr::unnest(cols = v) %>%
    dplyr::mutate(id = names(v)) %>%
    tidyr::unnest(cols = v) %>%
    dplyr::relocate(id, .before = 1) %>%
    dplyr::relocate(repo, .after = id) %>%
    dplyr::rename(package = v)