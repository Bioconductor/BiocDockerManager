#' 
.FUN <- function(cmd, args) {
    tryCatch({
        system2(cmd, args, stdout = TRUE, stderr = TRUE)
    }, error = function(e) {
        stop(
            "'", cmd, " ",
            paste(args, collapse = " "), "' failed. reason:",
            "\n  ", conditionMessage(e)
        )
    }, warning = function(e) {
        stop(
            "'", cmd, " ",
            paste(args, collapse = " "), "' failed. reason:",
            "\n  ", conditionMessage(e)
        )
    })
}


## Docker command line function to run with arguments
.docker_do <- function(args) {
    .FUN("docker", args)
}


## Pull a docker image, see 'install()'
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

    .docker_do(cmd)
}


#' List installed images/available on local machine
#' @importFrom readr read_table
.docker_images <-
    function(name = "", quiet = FALSE)
{
    stopifnot(is.logical(quiet))

    cmd <- c("images", if (quiet) "--quiet", name)
    res <- .docker_do(cmd)
    read_table(res)
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
