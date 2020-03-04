#' Internal function to run commands using system2
#'
#' This function has a tryCatch block to show the error message
#' when the wrong
#'
#' @keywords internal
#'
#' @param cmd command line tool
#' @param args arguments for command line tool in a character vector
#'
.FUN <- function(cmd, args) {
    result <- system2(cmd, args)
    if (result) {
        ## error
        stop(
            "'", cmd, " ",
            paste(args, collapse = " "), "' failed. status: ",
            result
        )
    }
    invisible(result)
}


#' Docker command line function to run with arguments
#'
#' @keywords internal
#'
#' @param args arguments to docker command in a character vector
#'
.docker_do <- function(args) {
    .FUN("docker", args)
}


#' Pull a docker image, see 'install()'
#'
#' @keywords internal
#'
#' @param name `character(1)`, name of the image.
#' @param tag `character(1)`, name of the tag for the image. 'latest' is
#'             default.
#' @param quiet `logical(1)`, status of download is not displayed if TRUE.
#' @param all_tags `logical(1)`, pull all the tags of the image name.
#'
#' @return exit status of `docker_pull()` command.
#'
#' @examples
#'
#' \dontrun{
#' .docker_pull("bioconductor/bioconductor_docker")
#'
#' .docker_pull("bioconductor/bioconductor_docker", tag = "devel")
#'
#' .docker_pull("bioconductor/bioconductor_docker", all_tags=TRUE)
#'
#' .docker_pull("bioconductor/bioconductor_docker", quiet=TRUE)
#' }
.docker_pull <- function(name, tag = "latest",
                        quiet = FALSE, all_tags=FALSE)
{
    stopifnot(
        !is.na(name), length(name) == 1L,
        is.logical(quiet),
        is.logical(all_tags)
    )

    ## FIXME: This needs to be corrected.
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


#' List installed images/available on local machine.
#'
#' @keywords internal
#'
#' @param name `character(1)`, name of the image.
#' @param quiet `logical(1)`, output shows only IMAGE ID's if TRUE.
#' @return tibble with the docker images
#' @importFrom readr read_table
#'
#' @examples
#'
#' \dontrun{
#'     .docker_images()
#'
#'     .docker_images("bioconductor/bioconductor_docker")
#'
#'     .docker_images("bioconductor/bioconductor_docker", quiet = TRUE)
#' }
.docker_images <-
    function(name = "", quiet = FALSE)
{
    stopifnot(is.logical(quiet))
    cmd <- c("images", if (quiet) "--quiet", name)
    res <- .docker_do(cmd)
    read_table(res)
}
