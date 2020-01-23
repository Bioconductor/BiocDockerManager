# Docker command line function to run with arguments
.docker_do <- function(args) {
    err <- system2("docker", args, wait = TRUE)
    if (err)
        stop(
            "\n  ", sprintf("docker %s", paste(args, collapse=" ")),
            "\n  failed: error code ", err
        )
    invisible(err)
}

#' Pull a docker image, see 'install()'
#'
.docker_pull <- function(name, tag = "latest",
                        quiet = FALSE, all_tags=FALSE)
{
    stopifnot(
        !is.na(name), length(name) == 1L,
        is.logical(quiet),
        is.logical(all_tags)
    )

    ## build command
    cmd <- c("pull",
             if (quiet) "--quiet",
             paste0(name, if(!is.na(tag)) paste0(":", tag)))

    if (all_tags) {
        cmd <- c("pull",
                 if (quiet) "--quiet",
                 "--all-tags", name)
    }

    ## Do the docker command
    .docker_do(cmd)
}


#' Install/Pull a docker image
#' 
#' @export
install <-
    function(name, tag, ...)
{
    .docker_pull(name, tag, ...)
}


#' List installed images/available on local machine
.docker_images <-
    function(name = "", quiet = FALSE)
{
    stopifnot(is.logical(quiet))

    cmd <- c("images", if (quiet) "--quiet", name)
    .docker_do(cmd)
}


#'
#' @export
installed <-
    function(name = "", ...)
{
    .docker_images(name, ...)
}


#' Check if all images available are
#'
#'
#' @export
valid <-
    function()
{
    
}

## TODO :

## valid() == valid
## pull() == install (DONE)
## installed() == list (DONE)
## available()  == available (DONE)
## use_dockerfile() == template for users to get started to extend the bioconductor_docker image

## BONUS :
## run()
## stop()
