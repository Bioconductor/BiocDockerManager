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
.get_rocker_rstudio_version <- 
    function(version = c("devel", "release")) 
{
    version <- match.arg(version)
    CONFIG <- system.file("extdata", "config", package="BiocDockerManager")
    versions = yaml::read_yaml(CONFIG)
    versions[[version]]
}


#' Get dockerfile template
#'
#' @details Get the Dockerfile template which is annotated 
#'     with whisker templating tags.
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
#' @details `create_payload()` fills in the template with
#'     the appropriate payload. The payload can be either
#'     for release or devel.
#'     
#' @param `character(1)` version for Bioconductor is 'devel' or 'release'
#' 
.create_payload <- function(version = c("devel", "release")) {
    
    version = match.arg(version)
    
    ## TODO: The build_vars && symbols don't get read properly. Fix this.
    build_vars <- readLines(
        system.file("extdata", "build_vars", package="BiocDockerManager"),
        encoding = "UTF-8"
    )
    
    ## Create payload
    payload <- list(
        rocker_rstudio_version = .get_rocker_rstudio_version(version),
        devel_build_variables = if(version == "devel") build_vars
    )
    payload
}


#' Render the template with the 
#'
#' @details `render_dockerfile()` will render the 
#'     dockerfile from the template with the appropriate
#'     information for release or devel. 
#'
#' @param `character(1)` version for Bioconductor is 'devel' or 'release'
#' 
#' @param `character(1)` path to where the rendered file is written. 
#' 
#' @importFrom whisker whisker.render
#' @export
render_dockerfile <- 
    function(version=c("devel", "release"), path)
{
    tmpl <- .dockerfile_template()
    payload <- .create_payload(version = version)
    ## Render devel
    writeLines(whisker.render(tmpl, payload), path)
}

