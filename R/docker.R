#' Docker command line function to run with arguments
#'
.docker_do <- function(args) {
    err <- system2("docker", args, wait = TRUE)
    if (err)
        stop(
            "\n  ", sprintf("docker %s", paste(args, collapse=" ")),
            "\n  failed: error code ", err
        )
    invisible(err)
}

#' @export
docker_pull <- function(name, tag,
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

#' @export
docker_installed <-
    function(name, quiet = FALSE)
{
    stopifnot(is.logical(quiet))

    cmd <- c("images", if (quiet) "--quiet", name)
    .docker_do(cmd)
}
