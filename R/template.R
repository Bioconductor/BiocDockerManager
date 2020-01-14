#' Get rocker/rstudio image version
#'
#' @details `get_rocker_rstudio_version()` gives the devel and release
#'     R version number of the latest release and devel images. These
#'     versions are taken from "inst/extdata/config".
#'
#' @param version `character(1)` "release" or "devel" version.
#'
#' @return `character(1)` version number or name (devel).
#' 
get_rocker_rstudio_version <-
    function(version = c("devel", "release"))
{
    version <- match.arg(version)
    CONFIG <- system.file("extdata", "config", package="BiocDockerManager")
    versions = yaml::read_yaml(CONFIG)
    versions[[version]]
}


#' Get dockerfile template
#'
#' @details The template is annotated with whisker templating tags
#' 
.dockerfile_template <-
    function()
{
    readLines(
        system.file("extdata", "Dockerfile", package="BiocDockerManager")
    )
}


#' Fill in the template with a payload.
#'
#' 
.create_payload <- function(devel) {
    ## Argument validitity check
    stopifnot(is.logical(devel))

    build_vars <- readLines(
        system.file("extdata", "build_vars", package="BiocDockerManager")
    )

    ## Create payload
    payload <- list(
        rocker_rstudio_version = if (devel) {get_rocker_rstudio_version("devel")}
                                 else {get_rocker_rstudio_version("release")}
        devel_build_variables = if(devel) build_vars
    )
    payload
}


#' Render the template with the 
#'
#' @importFrom whisker whisker.render
#' @export
render_dockerfile <- function(devel, path) {
    tmpl <- .dockerfile_template()

    if (devel)
        payload <- .create_payload(devel = TRUE)
    else
        payload <- .create_payload(devel = FALSE)
    ## Render devel
    writeLines(whisker.render(tmpl, payload), path)
}


