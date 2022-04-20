

# With correct ref ----

load("gids_283_head.rda")

test_that("s2i works",{
  expect_identical(s2i("aat-1", gids_283_head),
                   "WBGene00000002")

  expect_identical(s2i(c("aap-1", "aat-5"), gids_283_head),
                   c("WBGene00000001", "WBGene00000006"))
})


test_that("s2i of missing genes returns NA",{
  expect_identical(s2i("aat-10", gids_283_head),
                   NA_character_)

  expect_identical(s2i(c("aap-1", "aat-5", "aat-10"), gids_283_head),
                   c("WBGene00000001", "WBGene00000006", NA_character_))

  expect_warning(s2i("aat-10",
                     gids_283_head,
                     warn_missing = TRUE))

  expect_warning(s2i(c("aap-1", "aat-5", "aat-10"),
                     gids_283_head,
                     warn_missing = TRUE))
})




test_that("i2s works",{
  expect_identical(i2s("WBGene00000002", gids_283_head),
                   "aat-1")

  expect_identical(i2s(c("WBGene00000001", "WBGene00000006"), gids_283_head),
                   c("aap-1", "aat-5"))
})

test_that("i2s of missing genes returns NA",{
  expect_identical(i2s("WBGene99999999", gids_283_head),
                   NA_character_)

  expect_identical(i2s(c("WBGene00000001", "WBGene00000006", "WBGene99999999"), gids_283_head),
                   c("aap-1", "aat-5", NA_character_))

  expect_warning(i2s("WBGene99999999",
                     gids_283_head,
                     warn_missing = TRUE))

  expect_warning(i2s(c("WBGene00000001", "WBGene00000006", "WBGene99999999"),
                     gids_283_head,
                     warn_missing = TRUE))
})



test_that("wb_seq2id works",{
  expect_identical(wb_seq2id("F27C8.1", gids_283_head),
                   "WBGene00000002")

  expect_identical(wb_seq2id(c("Y110A7A.10", "C55C2.5"), gids_283_head),
                   c("WBGene00000001", "WBGene00000006"))
})

test_that("wb_seq2id of missing genes returns NA",{
  expect_identical(wb_seq2id("X99X9.9", gids_283_head),
                   NA_character_)

  expect_identical(wb_seq2id(c("Y110A7A.10", "C55C2.5", "X99X9.9"), gids_283_head),
                   c("WBGene00000001", "WBGene00000006", NA_character_))

  expect_warning(wb_seq2id("X99X9.9",
                     gids_283_head,
                     warn_missing = TRUE))

  expect_warning(wb_seq2id(c("Y110A7A.10", "C55C2.5", "X99X9.9"),
                     gids_283_head,
                     warn_missing = TRUE))
})





test_that("wb_id2seq works",{
  expect_identical(wb_id2seq("WBGene00000002", gids_283_head),
                   "F27C8.1")

  expect_identical(wb_id2seq(c("WBGene00000001", "WBGene00000006"), gids_283_head),
                   c("Y110A7A.10", "C55C2.5"))
})

test_that("wb_id2seq of missing genes returns NA",{
  expect_identical(wb_id2seq("WBGene99999999", gids_283_head),
                   NA_character_)

  expect_identical(wb_id2seq(c("WBGene00000001", "WBGene00000006", "WBGene99999999"), gids_283_head),
                   c("Y110A7A.10", "C55C2.5", NA_character_))

  expect_warning(wb_id2seq("WBGene99999999",
                     gids_283_head,
                     warn_missing = TRUE))

  expect_warning(wb_id2seq(c("WBGene00000001", "WBGene00000006", "WBGene99999999"),
                     gids_283_head,
                     warn_missing = TRUE))
})





test_that("wb_seq2name works",{
  expect_identical(wb_seq2name("F27C8.1", gids_283_head),
                   "aat-1")

  expect_identical(wb_seq2name(c("Y110A7A.10", "C55C2.5"), gids_283_head),
                   c("aap-1", "aat-5"))
})

test_that("wb_seq2name of missing genes returns NA",{
  expect_identical(wb_seq2name("X99X9.9", gids_283_head),
                   NA_character_)

  expect_identical(wb_seq2name(c("Y110A7A.10", "C55C2.5", "X99X9.9"), gids_283_head),
                   c("aap-1", "aat-5", NA_character_))

  expect_warning(wb_seq2name("X99X9.9",
                           gids_283_head,
                           warn_missing = TRUE))

  expect_warning(wb_seq2name(c("Y110A7A.10", "C55C2.5", "X99X9.9"),
                           gids_283_head,
                           warn_missing = TRUE))
})





test_that("wb_symbol2seq works",{
  expect_identical(wb_symbol2seq("aat-1", gids_283_head),
                   "F27C8.1")

  expect_identical(wb_symbol2seq(c("aap-1", "aat-5"), gids_283_head),
                   c("Y110A7A.10", "C55C2.5"))
})

test_that("wb_symbol2seq of missing genes returns NA",{
  expect_identical(wb_symbol2seq("aat-10", gids_283_head),
                   NA_character_)

  expect_identical(wb_symbol2seq(c("aap-1", "aat-5", "aat-10"), gids_283_head),
                   c("Y110A7A.10", "C55C2.5", NA_character_))

  expect_warning(wb_symbol2seq("aat-10",
                           gids_283_head,
                           warn_missing = TRUE))

  expect_warning(wb_symbol2seq(c("aap-1", "aat-5", "aat-10"),
                           gids_283_head,
                           warn_missing = TRUE))
})



# With missing in ref ----

load("gids_283_missing.rda")


test_that("s2i of NA genes returns NAs when missing in ref",{
  expect_identical(s2i(NA_character_, gids_283_miss),
                   NA_character_)

  expect_identical(s2i(c("aap-1", "aat-5", NA_character_), gids_283_miss),
                   c("WBGene00000001", "WBGene00000006", NA_character_))

  expect_warning(s2i(NA_character_,
                     gids_283_miss,
                     warn_missing = TRUE))

  expect_warning(s2i(c("aap-1", "aat-5", NA_character_),
                     gids_283_miss,
                     warn_missing = TRUE))
})



test_that("i2s of NA genes returns NA when NA in ref",{
  expect_identical(i2s(NA_character_, gids_283_miss),
                   NA_character_)

  expect_identical(i2s(c("WBGene00000001", "WBGene00000006", NA_character_), gids_283_miss),
                   c("aap-1", "aat-5", NA_character_))

  expect_warning(i2s(NA_character_,
                     gids_283_miss,
                     warn_missing = TRUE))

  expect_warning(i2s(c("WBGene00000001", "WBGene00000006", NA_character_),
                     gids_283_miss,
                     warn_missing = TRUE))
})



test_that("wb_seq2id of NA genes returns NA when NA in ref",{
  expect_identical(wb_seq2id(NA_character_, gids_283_miss),
                   NA_character_)

  expect_identical(wb_seq2id(c("Y110A7A.10", "C55C2.5", NA_character_), gids_283_miss),
                   c("WBGene00000001", "WBGene00000006", NA_character_))

  expect_warning(wb_seq2id(NA_character_,
                           gids_283_miss,
                           warn_missing = TRUE))

  expect_warning(wb_seq2id(c("Y110A7A.10", "C55C2.5", NA_character_),
                           gids_283_miss,
                           warn_missing = TRUE))
})



test_that("wb_id2seq of NA genes returns NA when NA in ref",{
  expect_identical(wb_id2seq(NA_character_, gids_283_miss),
                   NA_character_)

  expect_identical(wb_id2seq(c("WBGene00000001", "WBGene00000006", NA_character_), gids_283_miss),
                   c("Y110A7A.10", "C55C2.5", NA_character_))

  expect_warning(wb_id2seq(NA_character_,
                           gids_283_miss,
                           warn_missing = TRUE))

  expect_warning(wb_id2seq(c("WBGene00000001", "WBGene00000006", NA_character_),
                           gids_283_miss,
                           warn_missing = TRUE))
})




test_that("wb_seq2name of NA genes returns NA when NA in ref",{
  expect_identical(wb_seq2name(NA_character_, gids_283_miss),
                   NA_character_)

  expect_identical(wb_seq2name(c("Y110A7A.10", "C55C2.5", NA_character_), gids_283_miss),
                   c("aap-1", "aat-5", NA_character_))

  expect_warning(wb_seq2name(NA_character_,
                             gids_283_miss,
                             warn_missing = TRUE))

  expect_warning(wb_seq2name(c("Y110A7A.10", "C55C2.5", NA_character_),
                             gids_283_miss,
                             warn_missing = TRUE))
})





test_that("wb_symbol2seq of NA genes returns NA when NA in ref",{
  expect_identical(wb_symbol2seq(NA_character_, gids_283_miss),
                   NA_character_)

  expect_identical(wb_symbol2seq(c("aap-1", "aat-5", NA_character_), gids_283_miss),
                   c("Y110A7A.10", "C55C2.5", NA_character_))

  expect_warning(wb_symbol2seq(NA_character_,
                               gids_283_miss,
                               warn_missing = TRUE))

  expect_warning(wb_symbol2seq(c("aap-1", "aat-5", NA_character_),
                               gids_283_miss,
                               warn_missing = TRUE))
})


