release_url <- "https://raw.githubusercontent.com/rocker-org/rocker-versioned/master/rstudio/3.6.1.Dockerfile"

## Get rocker release version to avoid hardcoding
get_rocker_rstudio_release_version <- function(url) {
    line <- readLines(url, n=1)
    rstudio_version <- gsub("FROM rocker/r-ver:", "", line)
    rstudio_version
}

## Get template
dockerfile_template <- function() {
    readLines(
        system.file("extdata", "bioconductor_docker",
                    "Dockerfile", package="BiocDockerManager")
    )
}

## Create payload
create_payload <- function(devel) {
    ## Argument validitity check
    stopifnot(is.logical(devel))

    ## rocker version of rocker/rstudio image
    rstudio_version <- get_rocker_rstudio_release_version(release_url)

    build_vars <- "RUN curl -O https://raw.githubusercontent.com/Bioconductor/BBS/master/3.11/R_env_vars.sh \
        && cat R_env_vars.sh | grep -o '^[^#]*' | sed 's/export //g' >>/etc/environment \
        && cat R_env_vars.sh >> /root/.bashrc \
        && rm -rf R_env_vars.sh"

    ## Create payload
    payload <- list(
        rocker_rstudio_version = if(devel) "devel" else rstudio_version,
        devel_build_variables = if(devel) build_vars
    )
    payload
}

#' @importFrom whisker whisker.render
#' @export
render_dockerfile <- function(devel, path) {
    tmpl <- dockerfile_template()

    if (devel)
        payload <- create_payload(devel = TRUE)
    else
        payload <- create_payload(devel = FALSE)
    ## Render devel
    writeLines(whisker.render(tmpl, payload), path)
}
