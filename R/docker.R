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

