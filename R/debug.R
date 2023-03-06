## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Print Debugging Information
#'
#' This function is intended mainly for use within the package, but users may
#' also call it directly in their own code.  Within the package, the value
#' of `debug` is generally reduced by 1 on each nested function call, leading
#' to indented messages. Most functions start and end with a call to
#' [argoFloatsDebug()] that has `style="bold"` and `unindent=1`.
#'
#' @param debug an integer specifying the level of debugging. Values greater
#' than zero indicate that some printing should be done. Values greater
#' than 3 are trimmed to 3. Many functions pass `debug=debug-1` down to
#' deeper functions, which yields a nesting-indent format in the output.
#' @param ... values to be printed, analogous to the `...` argument
#' list of [cat()].
#' @param style character value indicating special formatting, with `"plain"`
#' for normal text, `"bold"` for bold-faced text, `"italic"` for italicized
#' text, `"red"` for red text, `"green"` for green text, or `"blue"` for blue
#' text. These codes may not be combined.
#' @param showTime logical value indicating whether to preface message with
#' the present time. This can be useful for learning about which operations
#' are using the most time, but the default is not to show this, in the
#' interests of brevity.
#' @param unindent integer specifying the degree of reverse indentation
#' to be done, as explained in the \dQuote{Details} section.
#'
#' @examples
#'\donttest{
#' argoFloatsDebug(1, "plain text\n")
#' argoFloatsDebug(1, "red text\n", style="red")
#' argoFloatsDebug(1, "blue text\n", style="blue")
#' argoFloatsDebug(1, "bold text\n", style="bold")
#' argoFloatsDebug(1, "italic text with time stamp\n", style="italic", showTime=TRUE)
#'}
#' @author Dan Kelley
#'
#' @return None (invisible NULL).
#'
#' @importFrom utils flush.console
#'
#' @export
argoFloatsDebug <- function(debug=0, ..., style="plain", showTime=FALSE, unindent=0)
{
    catSpecial <- function(...) if (interactive()) cat(...)
    debug <- max(0L, min(3L, floor(debug+0.5))) # max 3 levels deep
    if (debug > 0) {
        n <- 4 - debug - unindent
        if (is.character(style) && style == "plain") {
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
        } else if (is.character(style) && style == "bold") {
            catSpecial("\033[1m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            catSpecial("\033[0m")
        } else if (is.character(style) && style == "italic") {
            catSpecial("\033[3m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            catSpecial("\033[0m")
        } else if (is.character(style) && style == "red") {
            catSpecial("\033[31m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            catSpecial("\033[0m")
        } else if (is.character(style) && style == "green") {
            catSpecial("\033[32m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            catSpecial("\033[0m")
        } else if (is.character(style) && style == "blue") {
            catSpecial("\033[34m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            catSpecial("\033[0m")
        } else if (is.function(style)) {
            if (n > 0)
                cat(style(paste(rep("  ", n), collapse="")))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(style(...))
        } else { # fallback
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
        }
        flush.console()
    }
    invisible(NULL)
}
