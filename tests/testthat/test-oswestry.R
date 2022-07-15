testthat::test_that(
  "cols throws an error when there are not exactly 10 cols", {
  testthat::expect_error(score.oswestry(cols = 1:15),  # Too many columns
                         'cols should be of length')
  testthat::expect_error(score.oswestry(cols = 1:3),   # Too few columns
                         'cols should be of length')
  }
)


testthat::test_that(
  "language throws an error when it's not set to a supported language", {
    testthat::expect_error(score.oswestry(cols = 1:10, language = 'spanish'),
                           "Please, choose between")
  }
)


testthat::test_that(
  "scores are computed correctly in supported language",{
    testthat::expect_equal(
      oswestry_eng[,c(1,25)],
      score.oswestry(data = oswestry_eng,
                     cols = 3:12,
                     obs = 'id')
    )
    testthat::expect_equal(
      oswestry_fr[,c(1,25)],
      score.oswestry(data = oswestry_fr,
                     cols = 3:12,
                     obs = 'id',
                     language = 'french')
    )
  }
)

testthat::test_that(
  "keepResponses correctly keeps or removes individual item responses", {
    testthat::expect_equal(
      oswestry_eng [,-c(2,13:24)],
      score.oswestry(data = oswestry_eng,
                     cols = 3:12,
                     obs = 'id',
                     keepResponses = TRUE)
    )
    testthat::expect_equal(
      oswestry_eng[,-c(2,3:24)],
      score.oswestry(data = oswestry_eng,
                     cols = 3:12,
                     obs = 'id',
                     keepResponses = FALSE)
    )
  }
)

testthat::test_that(
  "keepScoring correctly keeps or removes individial item scoring", {
    testthat::expect_equal(
      oswestry_eng[,-c(2,3:12)],
      score.oswestry(data = oswestry_eng,
                     cols = 3:12,
                     obs = 'id',
                     keepScoring = TRUE)
    )
    testthat::expect_equal(
      oswestry_eng[,-c(2,3:24)],
      score.oswestry(data = oswestry_eng,
                     cols = 3:12,
                     obs = 'id',
                     keepScoring = FALSE)
    )
  }
)
