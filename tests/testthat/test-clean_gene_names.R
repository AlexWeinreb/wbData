test_that("Can find old ids", {
  expect_identical(
    wb_clean_gene_names(c("WBGene00012733", "WBGene00007566", "WBGene00045409")),
    c("WBGene00012734", "WBGene00206536", "WBGene00011326")
  )
})


test_that("Can interleave correct ids", {
  expect_identical(
    wb_clean_gene_names(c("WBGene00012733", "WBGene00000424", "WBGene00007566", "WBGene00045409")),
    c("WBGene00012734", "WBGene00000424", "WBGene00206536", "WBGene00011326")
  )
})


test_that("Works for single gene", {

  expect_identical(
    wb_clean_gene_names("WBGene00012733"),
    "WBGene00012734"
  )
  expect_identical(
    wb_clean_gene_names("WBGene00000424"),
    "WBGene00000424"
  )
  expect_identical(
    wb_clean_gene_names("WBGene00007566"),
    "WBGene00206536"
  )
  expect_identical(
    wb_clean_gene_names("WBGene00045409"),
    "WBGene00011326"
  )


})


test_that("Warns for typos", {

  expect_warning(wb_clean_gene_names(c("WBGene00012733",
                                       "WBGene00000424",
                                       "WBGne001")))

  expect_no_warning(wb_clean_gene_names(c("WBGene00012733",
                                          "WBGene00000424",
                                          "WBGne001"),
                                        warn_missing = FALSE))

  expect_identical(
    wb_clean_gene_names(c("WBGene00012733",
                          "WBGene00000424",
                          "WBGne001"),
                        warn_missing = FALSE),
    c("WBGene00012734", "WBGene00000424", NA_character_)
  )
})



test_that("Errors when nothing correct", {

  expect_error(wb_clean_gene_names(c("WBGen00012733",
                                       "WBGene0000424",
                                       "WBGne001")))

  expect_error(wb_clean_gene_names(c("WBGene0001r2733",
                                          "WBGee00000424",
                                          "WBGne001"),
                                        warn_missing = FALSE))


})


test_that("Warns for obsolete", {

  expect_warning(wb_clean_gene_names(c("WBGene00000587",
                                       "WBGene00000424")))

  expect_no_warning(wb_clean_gene_names(c("WBGene00000587",
                                          "WBGene00000424"),
                                        warn_missing = FALSE))

  expect_identical(
    wb_clean_gene_names(c("WBGene00000587",
                          "WBGene00000424"),
                        warn_missing = FALSE),
    c(NA_character_, "WBGene00000424")
  )

})


