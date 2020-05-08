## Do we have permission to download files? (NOT EXPORTED)
## @param destdir character value giving directory into which to download files
## @return logical value indicating whether this user has permission to download fies to ~/data/argo
canDownload <- function(destdir="~/data/argo")
{
    ## FIXME(dek): add username for @harbinj in next line (also, I was guessing on @richardsc's linux name)
    isDeveloper <- Sys.getenv("USER") == "kelley" || Sys.getenv("USERNAME") == "jaimieharbin" || Sys.getenv("USER") == "richardsc"
    canWrite <- file.exists(destdir) && file.info(destdir)$isdir
    isDeveloper && canWrite
}

