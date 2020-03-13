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
.FUN <- function(cmd, args, stdout = FALSE) {
    result <- system2(cmd, args,
                      if (stdout) stdout = stdout)
    if(stdout)
        return(result)

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
.docker_do <- function(args, ...) {
    .FUN("docker", args, ...)
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
        .is_scalar_logical(quiet),
        .is_scalar_logical(all_tags)
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
    function(name)
{
    stopifnot(
        .is_character_0_or_1(name, zchar=TRUE)
    )
    cmd <- c("images", name)
    res <- .docker_do(cmd, stdout = TRUE)
    read_table(res)
}


#' Inspect docker image by label
#'
#' @keywords internal
#'
#' @param name `character(1)`, name of the image.
#'
#' @param tag `character(1)`, tag for the image.
#'
#' @param label `character(1)` label name from the list - "name",
#'     "description", "version", "url", "maintainer",
#'     "license","vendor"
#'
.docker_inspect_label <-
    function(name, tag,
             label = c("name", "description",
                       "version", "url",
                       "maintainer", "license","vendor")
             )
{
    label <- match.arg(label)

    stopifnot(
        .is_scalar_character(name),
        missing(tag) || .is_scalar_character(tag),
        .is_scalar_character(label)
    )

    cmd <- c("inspect",
             "-f",
             sprintf("'{{.Config.Labels.%s}}'", label),
             paste0(name, if(!missing(tag)) paste0(":", tag)))

    .docker_do(cmd, stdout = TRUE)
}


#' Inspect docker image digest value
#'
#' @keywords internal
#'
#' @param name `character(1)`, name of the image.
#'
#' @param tag `character(1)`, tag for the image.
#'
.docker_inspect_digest <-
    function(name, tag)
{
    stopifnot(
        .is_scalar_character(name),
        missing(tag) || .is_scalar_character(tag)
    )

    if(missing(tag))
        tag = "latest"

    cmd <- c("inspect",
             "-f", "'{{.RepoDigests}}={{.Created}}'",
             paste(name, tag, sep=":"))

    res <- .docker_do(cmd, stdout = TRUE)
    digest <- strsplit(res, split = "=")[[1]][1]

    tibble(
        image = name,
        tag = tag,
        repo_digest = substr(digest, nchar(name) + 3, nchar(digest) - 1)
    )
}
