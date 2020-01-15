o.git_do <- function(args) {
    err <- system2("git", args, wait = TRUE)
    if (err)
        stop(
            "\n  ", sprintf("git %s", paste(args, collapse=" ")),
            "\n  failed: error code ", err
        )
    invisible(err)
}

#' @rdname git_api
#'
#' @return `git_checkout()`: Exit status of `git checkout master`
#'     invisbly.
#'     
git_checkout <- function(branch = "master") {
    stopifnot(
        is.character(branch), length(branch) == 1L, !is.na(branch),
        nzchar(branch)
    )
    .git_do(c("checkout", branch))
}

#' @rdname git_api
#'
#' @return `git_push()`: Exit status of `git push remote branch`,
#'     invisibly.
git_push <- function(name="origin", branch="master") {
    args <- c("push", name, branch)
    .git_do(args)
}

#' @rdname git_api
#'
#' @param commit_message character(1) representing commit message
#'
#' @return `git_commit()`: Exit status of
#'     `git commit -a -m commit_message`, invisibly.
git_commit <- function(commit_message) {
    args <- c("commit", "-a", "-m", sprintf("'%s'", commit_message))
    .git_do(args)
}

#' @rdname git_api
#'
#' @return `git_pull()`: Exit status of `git pull`, invisibly.
git_pull <- function() {
    .git_do("pull")
}
