.git_do <- function(args) {
    err <- system2("git", args, wait = TRUE)
    if (err)
        stop(
            "\n  ", sprintf("git %s", paste(args, collapse=" ")),
            "\n  failed: error code ", err
        )
    invisible(err)
}

git_checkout <- function(branch = "master") {
    stopifnot(
        is.character(branch), length(branch) == 1L, !is.na(branch),
        nzchar(branch)
    )
    .git_do(c("checkout", branch))
}

