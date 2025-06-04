# precomputed values using tests/test_compute_bisg.R
precomputed_bisg <- structure(
  list(
    name = c(
      "thiem",
      "zuidema",
      "woodum",
      "dodd",
      "willcocks",
      "gipe",
      "afra",
      "chilcote",
      "jurisch",
      "patterson"
    ),
    year = rep(2020, 10),
    state = rep("WA", 10L),
    county = c(
      "island",
      "spokane",
      "whatcom",
      "klickitat",
      "king",
      "kittitas",
      "snohomish",
      "cowlitz",
      "king",
      "douglas"
    ),
    bisg_aian = c(
      0.00037110421216914,
      0.00658226387182746,
      0.0284277637778205,
      0.0180125869958098,
      0,
      0.00259639064915099,
      0.0115251189635379,
      0.0122257529469864,
      0.0089774949304735,
      0.0102150848816697
    ),
    bisg_api = c(
      0.0780218618298757,
      0.00042455134820734,
      0.0570376401545724,
      0.00077984931944855,
      0.0547890101852704,
      0.00138370290508388,
      0.156838445448317,
      0.00085571151349377,
      0.0396716842487496,
      0.00107127054295823
    ),
    bisg_black_nh = c(
      8.72695747373717e-05,
      0.00013585938058259,
      0.00646736454248696,
      0.00208884956138178,
      0.00874947915128102,
      0.00013666516274874,
      0.0228918435603956,
      0.00013684638101364,
      0,
      0.00785661771066974
    ),
    bisg_hispanic = c(
      0.0063167746442836,
      0.00489400537065451,
      0.0685281128284246,
      0.0139305815782108,
      0.00925380213767859,
      0.00744085731935334,
      0.0795099933144588,
      0.0109714185193617,
      0,
      0.0573367446824043
    ),
    bisg_white_nh = c(
      0.901171482059678,
      0.978932537754341,
      0.807711409399109,
      0.943803185903043,
      0.901256036158691,
      0.979735912690319,
      0.693595068459168,
      0.964792526953389,
      0.888497783389607,
      0.893580745957654
    ),
    bisg_other = c(
      0.0140315076792566,
      0.00903078227438726,
      0.0318277092975871,
      0.021384946642106,
      0.0259516723670789,
      0.00870647127334401,
      0.0356395302541235,
      0.0110177436857559,
      0.0628530374311693,
      0.0299395362246442
    )
  ),
  row.names = c(NA, -10L),
  class = "data.frame"
)

test_that("bisg() returns correct values", {
  out <- bisg(
    name = precomputed_bisg$name,
    state = precomputed_bisg$state,
    county = precomputed_bisg$county,
    year = 2020
  )
  expect_equal(
    out[, !colnames(out) == ".found"],
    precomputed_bisg
  )
})

test_that("BISG can be computed for all 50 states with known name", {
  for (st in datasets::state.abb) {
    out <- bisg(
      name = "Smith",
      state = st,
      county = .race_x_county_data(st, 2020)$county[1],
      year = 2020
    )
    expect_false(is.na(out$bisg_aian[1]), info = st)
  }
})

test_that("BISG can be computed for all 50 states with unknown name", {
  for (st in datasets::state.abb) {
    out <- bisg(
      name = "NOT_A_NAME",
      state = st,
      county = .race_x_county_data(st, 2020)$county[1],
      year = 2020
    )
    expect_false(is.na(out$bisg_aian[1]), info = st)
  }
})

test_that("race x surname reference table has a single 'all other names' row", {
  df <- .race_x_surname_data()
  expect_length(which(df$name == 'all other names'), 1)
})
