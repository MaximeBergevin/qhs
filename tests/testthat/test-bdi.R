testthat::test_that(
  "cols throws an error when there are not exactly 21 cols", {
  testthat::expect_error(score.bdi(cols = 1:15),       # Too many columns
                         'cols should be of length')
  testthat::expect_error(score.bdi(cols = 1:3),        # Too few columns
                         'cols should be of length')
  }
)


testthat::test_that(
  "scores are computed correctly",{
    testthat::expect_equal(
      bdi_eng[,c(1,45)],
      score.bdi(data = bdi_eng,
                     cols = 3:23,
                     obs = 'id')
    )
  }
)


testthat::test_that(
  "keepResponses correctly removes individual item responses", {
    testthat::expect_equal(
      bdi_eng[,-c(2,24:44)],
      score.bdi(data = bdi_eng,
                cols = 3:23,
                obs = 'id',
                keepResponses = TRUE)
    )
  }
)


testthat::test_that(
  "keepScoring correctly removes individial item scoring", {
    testthat::expect_equal(
      bdi_eng[,-c(2,3:23)],
      score.bdi(data = bdi_eng,
                     cols = 3:23,
                     obs = 'id',
                     keepScoring = TRUE)
      )
    }
  )
