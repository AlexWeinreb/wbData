

# Complete ref ----
load("tx2g_283_head.rda")

test_that("wb_g2tx works",{
  expect_identical(wb_g2tx("WBGene00001492", tx2g_283_head),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1")))

  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394"), tx2g_283_head),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                                           "Y38C1AA.1c.1","Y38C1AA.1d.1")))
})

test_that("wb_tx2g works",{
  expect_identical(wb_tx2g("Y38C1AB.8b.1", tx2g_283_head),
                   "WBGene00001492")

  expect_identical(wb_tx2g(c("Y38C1AB.8b.1", "Y38C1AA.1b.1"), tx2g_283_head),
                   c("WBGene00001492","WBGene00021394"))
})

test_that("wb_tx2g works with tx from the same gene",{
  expect_identical(wb_tx2g(c("Y38C1AB.8b.1","Y38C1AB.8c.1"), tx2g_283_head),
                   c("WBGene00001492","WBGene00001492"))

  expect_identical(wb_tx2g(c("Y38C1AA.1a.1","Y38C1AB.8b.1","Y38C1AB.8c.1","Y38C1AA.1b.1","Y38C1AB.8d.1"),
                           tx2g_283_head),
                   c("WBGene00021394","WBGene00001492","WBGene00001492","WBGene00021394","WBGene00001492"))
})


test_that("wb_g2tx of missing genes returns NA",{
  expect_identical(wb_g2tx("WBGene99999999", tx2g_283_head),
                   list(WBGene99999999 = NA_character_))

  expect_identical(wb_g2tx(c("WBGene00001492","WBGene99999999"), tx2g_283_head),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene99999999 = NA_character_))


  expect_warning(wb_g2tx("WBGene99999999",
                         tx2g_283_head,
                         warn_missing = TRUE))

  expect_warning(wb_g2tx(c("WBGene00001492","WBGene99999999"),
                         tx2g_283_head,
                         warn_missing = TRUE))
})


test_that("wb_tx2g of missing genes returns NA",{
  expect_identical(wb_tx2g("X99X9.9", tx2g_283_head),
                   NA_character_)

  expect_identical(wb_tx2g(c("Y38C1AB.8a.1","X99X9.9"), tx2g_283_head),
                   c("WBGene00001492",NA_character_))


  expect_warning(wb_tx2g("X99X9.9",
                         tx2g_283_head,
                         warn_missing = TRUE))

  expect_warning(wb_tx2g(c("Y38C1AB.8a.1","X99X9.9"),
                         tx2g_283_head,
                         warn_missing = TRUE))
})


##~ simplify ----
load("tx2g_283_square.rda")

test_that("wb_g2tx simplifies",{
  expect_identical(class(wb_g2tx("WBGene00001492", tx2g_283_head, simplify = TRUE)),
                   c("matrix", "array"))
  expect_identical(wb_g2tx("WBGene00001492", tx2g_283_head, simplify = TRUE),
                   matrix(c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                            "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                          ncol = 1, nrow = 6,
                          dimnames = list(NULL, "WBGene00001492")))

  # genes with different number of tx
  expect_identical(class(wb_g2tx(c("WBGene00001492", "WBGene00021394"),
                                 tx2g_283_head,
                                 simplify = TRUE)),
                   c("list"))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394"), tx2g_283_head, simplify = TRUE),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                                           "Y38C1AA.1c.1","Y38C1AA.1d.1")))

  # With "squared" reference: 3 tx per gene
  expect_identical(class(wb_g2tx(c("WBGene00001492", "WBGene00021394"),
                                 tx2g_283_square,
                                 simplify = TRUE)),
                   c("matrix", "array"))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394"),
                           tx2g_283_square,
                           simplify = TRUE),
                   matrix(c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                            "Y38C1AA.1a.1","Y38C1AA.1b.1","Y38C1AA.1c.1"),
                          nrow = 3, ncol = 2,
                          dimnames = list(NULL, c("WBGene00001492","WBGene00021394"))))
})



test_that("wb_g2tx simplifies with unknown genes",{
  expect_identical(class(wb_g2tx("WBGene99999999", tx2g_283_head, simplify = TRUE)),
                   c("character"))
  expect_identical(wb_g2tx("WBGene99999999", tx2g_283_head, simplify = TRUE),
                   c(WBGene99999999 = NA_character_))

  # genes with different number of tx
  expect_identical(class(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                                 tx2g_283_head,
                                 simplify = TRUE)),
                   c("list"))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                           tx2g_283_head,
                           simplify = TRUE),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                                           "Y38C1AA.1c.1","Y38C1AA.1d.1"),
                        WBGene99999999 = NA_character_))

  # With "squared" reference: 3 tx per gene
  expect_identical(class(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                                 tx2g_283_square,
                                 simplify = TRUE)),
                   c("list"))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                           tx2g_283_square,
                           simplify = TRUE),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1","Y38C1AA.1c.1"),
                        WBGene99999999 = NA_character_))
})





# With NA in ref ----
load("tx2g_283_miss.rda")


test_that("wb_g2tx works",{
  expect_identical(wb_g2tx("WBGene00001492", tx2g_283_miss),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1")))

  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394"), tx2g_283_miss),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                                           "Y38C1AA.1c.1","Y38C1AA.1d.1")))
})

test_that("wb_tx2g works",{
  expect_identical(wb_tx2g("Y38C1AB.8a.1", tx2g_283_miss),
                   "WBGene00001492")

  expect_identical(wb_tx2g(c("Y38C1AB.8a.1", "Y38C1AA.1d.1"), tx2g_283_miss),
                   c("WBGene00001492","WBGene00021394"))
})


test_that("wb_g2tx of missing genes returns NA",{
  expect_identical(wb_g2tx("WBGene99999999", tx2g_283_miss),
                   list(WBGene99999999 = NA_character_))

  expect_identical(wb_g2tx(c("WBGene00001492","WBGene99999999"), tx2g_283_miss),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene99999999 = NA_character_))


  expect_warning(wb_g2tx("WBGene99999999",
                         tx2g_283_miss,
                         warn_missing = TRUE))

  expect_warning(wb_g2tx(c("WBGene00001492","WBGene99999999"),
                         tx2g_283_miss,
                         warn_missing = TRUE))
})

test_that("wb_tx2g of missing genes returns NA",{
  expect_identical(wb_tx2g("X99X9.9", tx2g_283_miss),
                   NA_character_)

  expect_identical(wb_tx2g(c("Y38C1AB.8b.1","X99X9.9"), tx2g_283_miss),
                   c("WBGene00001492",NA_character_))


  expect_warning(wb_g2tx("X99X9.9",
                         tx2g_283_miss,
                         warn_missing = TRUE))

  expect_warning(wb_g2tx(c("WBGene00001492","X99X9.9"),
                         tx2g_283_miss,
                         warn_missing = TRUE))
})


test_that("wb_g2tx simplifies",{
  expect_identical(class(wb_g2tx("WBGene00001492", tx2g_283_miss, simplify = TRUE)),
                   c("matrix", "array"))
  expect_identical(wb_g2tx("WBGene00001492", tx2g_283_miss, simplify = TRUE),
                   matrix(c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                            "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                          ncol = 1, nrow = 6,
                          dimnames = list(NULL, "WBGene00001492")))

  # genes with different number of tx
  expect_identical(class(wb_g2tx(c("WBGene00001492", "WBGene00021394"),
                                 tx2g_283_miss,
                                 simplify = TRUE)),
                   c("list"))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394"), tx2g_283_miss, simplify = TRUE),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                                           "Y38C1AA.1c.1","Y38C1AA.1d.1")))
})



test_that("wb_g2tx simplifies with unknown genes",{
  expect_identical(class(wb_g2tx("WBGene99999999", tx2g_283_miss, simplify = TRUE)),
                   c("character"))
  expect_identical(wb_g2tx("WBGene99999999", tx2g_283_miss, simplify = TRUE),
                   c(WBGene99999999 = NA_character_))

  # genes with different number of tx
  expect_identical(class(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                                 tx2g_283_miss,
                                 simplify = TRUE)),
                   c("list"))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                           tx2g_283_miss,
                           simplify = TRUE),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                                           "Y38C1AA.1c.1","Y38C1AA.1d.1"),
                        WBGene99999999 = NA_character_))

  # With "squared" reference: 3 tx per gene
  expect_identical(class(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                                 tx2g_283_miss,
                                 simplify = TRUE)),
                   c("list"))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394", "WBGene99999999"),
                           tx2g_283_miss,
                           simplify = TRUE),
                   list(WBGene00001492 = c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                                           "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        WBGene00021394 = c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                                           "Y38C1AA.1c.1","Y38C1AA.1d.1"),
                        WBGene99999999 = NA_character_))
})


# With NA in input ----

test_that("wb_g2tx works with NA in input (and in ref)",{
  expect_identical(wb_g2tx(NA_character_, tx2g_283_head),
                   list(NA_character_) |> setNames(NA_character_))
  expect_identical(wb_g2tx(NA_character_, tx2g_283_miss),
                   list(NA_character_) |> setNames(NA_character_))

  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394",NA_character_),
                           tx2g_283_head),
                   list(c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                          "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                          "Y38C1AA.1c.1","Y38C1AA.1d.1"),
                        NA_character_) |>
                     setNames(c("WBGene00001492","WBGene00021394",NA_character_)))
  expect_identical(wb_g2tx(c("WBGene00001492", "WBGene00021394",NA_character_),
                           tx2g_283_miss),
                   list(c("Y38C1AB.8a.1","Y38C1AB.8b.1","Y38C1AB.8c.1",
                          "Y38C1AB.8d.1","Y38C1AB.8e.1","Y38C1AB.8f.1"),
                        c("Y38C1AA.1a.1","Y38C1AA.1b.1",
                          "Y38C1AA.1c.1","Y38C1AA.1d.1"),
                        NA_character_) |>
                     setNames(c("WBGene00001492","WBGene00021394",NA_character_)))
})

test_that("wb_tx2g works with NA in input (and in ref)",{
  expect_identical(wb_tx2g(NA_character_, tx2g_283_head),
                   NA_character_)
  expect_identical(wb_tx2g(NA_character_, tx2g_283_miss),
                   NA_character_)

  expect_identical(wb_tx2g(c("Y38C1AB.8a.1", "Y38C1AA.1b.1",NA_character_),
                           tx2g_283_head),
                   c("WBGene00001492","WBGene00021394",NA_character_))
  expect_identical(wb_tx2g(c("Y38C1AB.8a.1", "Y38C1AA.1d.1",NA_character_),
                           tx2g_283_miss),
                   c("WBGene00001492","WBGene00021394",NA_character_))
})


test_that("wb_g2tx warns with NA in input and in ref",{
  expect_warning(wb_g2tx(NA_character_, tx2g_283_head, warn_missing = TRUE))
  expect_warning(wb_g2tx(NA_character_, tx2g_283_miss, warn_missing = TRUE))

  expect_warning(wb_g2tx(c("WBGene00001492", "WBGene00021394",NA_character_),
                           tx2g_283_head,
                           warn_missing = TRUE))
  expect_warning(wb_g2tx(c("WBGene00001492", "WBGene00021394",NA_character_),
                           tx2g_283_miss,
                           warn_missing = TRUE))
})

test_that("wb_tx2g warns with NA in input and in ref",{
  expect_warning(wb_tx2g(NA_character_, tx2g_283_head, warn_missing = TRUE))
  expect_warning(wb_tx2g(NA_character_, tx2g_283_miss, warn_missing = TRUE))

  expect_warning(wb_tx2g(c("Y38C1AB.8a.1", "Y38C1AA.1b.1",NA_character_),
                         tx2g_283_head,
                         warn_missing = TRUE))
  expect_warning(wb_tx2g(c("Y38C1AB.8a.1", "Y38C1AA.1b.1",NA_character_),
                         tx2g_283_miss,
                         warn_missing = TRUE))
})






