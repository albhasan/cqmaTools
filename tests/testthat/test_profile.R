test_that("filename2profile works", {

    # Wrong trajectory filename throws an error.
    expect_error(
        filename2profile("asdf_412341234_234234_34234")
    )

    files <- file.path("fake", "path", "to", "file",
        c("ALF_2011_11_02_16_914.40",
          "ALF_2011_01_12_15_4419.60",
          "ALF_2011_08_31_15_4419.60",
          "ALF_2011_07_30_16_2590.80",
          "ALF_2011_07_20_16_1828.80",
          "ALF_2011_03_01_16_3505.20",
          "ALF_2011_08_31_16_3505.20",
          "ALF_2011_09_28_16_1219.20",
          "ALF_2011_08_23_16_1524.00",
          "ALF_2011_10_24_15_4419.60")
    )
    exp_profs <- c("ALF_2011_11_02", "ALF_2011_01_12", "ALF_2011_08_31",
        "ALF_2011_07_30", "ALF_2011_07_20", "ALF_2011_03_01", "ALF_2011_08_31", 
        "ALF_2011_09_28", "ALF_2011_08_23", "ALF_2011_10_24")
    profiles <- filename2profile(files)
    expect_true(all(profiles %in% exp_profs))

})
