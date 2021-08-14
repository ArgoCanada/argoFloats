## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
context("test defaults")

test_that("default destdir can be changed by option or env var", {
    old_options <- options(argoFloats.destdir = "custom/argo/path")
    expect_identical(argoDefaultDestdir(), "custom/argo/path")
    
    old_env_var <- Sys.getenv("R_ARGOFLOATS_DESTDIR")
    Sys.setenv(R_ARGOFLOATS_DESTDIR = "custom/argo/path")
    expect_identical(argoDefaultDestdir(), "custom/argo/path")
    
    options(argoFloats.destdir = NULL)
    Sys.setenv(R_ARGOFLOATS_DESTDIR = "")
    expect_identical(argoDefaultDestdir(), "~/data/argo")
    
    options(old_options)
    Sys.setenv(R_ARGOFLOATS_DESTDIR = old_env_var)
})

test_that("default server can be changed by option or env var", {
    old_options <- options(argoFloats.server = "http://my.server/")
    expect_identical(argoDefaultServer(), "http://my.server/")
    
    old_env_var <- Sys.getenv("R_ARGOFLOATS_SERVER")
    Sys.setenv(R_ARGOFLOATS_DESTDIR = "http://my.server/")
    expect_identical(argoDefaultServer(), "http://my.server/")
    
    options(argoFloats.server = NULL)
    Sys.setenv(R_ARGOFLOATS_DESTDIR = "")
    expect_identical(argoDefaultServer(), "ifremer-https")
    
    options(old_options)
    Sys.setenv(R_ARGOFLOATS_DESTDIR = old_env_var)
})

test_that("default index age can be changed by option or env var", {
    old_options <- options(argoFloats.indexAge = 12345)
    expect_identical(argoDefaultIndexAge(), 12345)

    old_env_var <- Sys.getenv("R_ARGOFLOATS_INDEx_AGE")
    Sys.setenv(R_ARGOFLOATS_INDEx_AGE = 12345)
    expect_identical(argoDefaultIndexAge(), 12345)
    
    options(argoFloats.indexAge = NULL)
    Sys.setenv(R_ARGOFLOATS_INDEX_AGE = "")
    expect_identical(argoDefaultIndexAge(), 1)

    options(old_options)
    Sys.setenv(R_ARGOFLOATS_INDEX_AGE = old_env_var)
})

test_that("default profile age can be changed by option or env var", {
    old_options <- options(argoFloats.profileAge = 12345)
    expect_identical(argoDefaultProfileAge(), 12345)

    old_env_var <- Sys.getenv("R_ARGOFLOATS_PROFILE_AGE")
    Sys.setenv(R_ARGOFLOATS_PROFILE_AGE = 12345)
    expect_identical(argoDefaultProfileAge(), 12345)
    
    options(argoFloats.profileAge = NULL)
    Sys.setenv(R_ARGOFLOATS_PROFILE_AGE = "")
    expect_identical(argoDefaultProfileAge(), 365)

    options(old_options)
    Sys.setenv(R_ARGOFLOATS_PROFILE_AGE = old_env_var)
})
