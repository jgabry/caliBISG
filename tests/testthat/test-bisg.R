test_that("bisg() returns correct values", {
  # the values in the snapshot were originally computed with data-raw/test-compute-bisg.R
  # and should match the values in data-raw/precomputed-bisg.R
  expect_snapshot_value(
    bisg(
      state = "WA",
      year = 2020,
      name = c("keopuhiwa", "kerkhove", "hrubesky",
               "dickison", "sellek", "fedorka", "surkov", "hubin", "borman",
               "simpson", "smolic", "lemus-olsen", "wurz", "hammons", "barger",
               "newsom", "pryor", "nikolaisen", "diecker", "gutkin", "kornec",
               "garvida", "kleingartner", "emmil", "hadley", "martinez garnica",
               "molvik", "mcrandle", "hopke", "jentgen", "reams", "smith-rybin",
               "petchang", "glaspey", "frasier", "tonkara", "klabacka", "chudnofsky",
               "bowman-hattery", "milnor", "lazarevich", "yakker", "tumalad",
               "matuszek", "ravindran", "rockas", "hills", "depeazer", "caballero",
               "anderson hale", "siglin", "cole-church", "zelenovic", "ranta",
               "torres-colon", "charie", "haskin", "renz", "hand", "waag", "o'donnell",
               "hoins", "rowley", "courtois", "sagami", "shamso", "renkert",
               "ricco", "williams", "lea vell", "kliman", "march", "lochner",
               "poggetti", "rashidy", "maughan", "kiele", "lu", "valle rubio",
               "squillace", "wilma", "wyemura parker", "belocura", "merideth",
               "gruzensky", "giles-moore", "gaboy", "valdovines herrera", "belanger",
               "kowis", "boyes", "hamberg", "sootticoon", "huerta-zuniga", "cheh",
               "kalzan", "tkach", "sachi", "eyrich", "cassell"),
      county = c("spokane", "king", "snohomish",
                 "kittitas", "clark", "cowlitz", "king", "spokane", "pierce",
                 "grays harbor", "pierce", "spokane", "spokane", "yakima", "skagit",
                 "lewis", "douglas", "thurston", "benton", "stevens", "pierce",
                 "thurston", "pierce", "snohomish", "skagit", "clark", "douglas",
                 "whatcom", "snohomish", "pierce", "grays harbor", "pierce", "king",
                 "yakima", "spokane", "snohomish", "snohomish", "snohomish", "whatcom",
                 "skagit", "pierce", "cowlitz", "kitsap", "whitman", "king", "king",
                 "chelan", "pierce", "pierce", "snohomish", "grays harbor", "pierce",
                 "snohomish", "thurston", "pierce", "king", "clallam", "island",
                 "clark", "king", "pend oreille", "benton", "san juan", "king",
                 "whatcom", "king", "king", "pierce", "lewis", "mason", "king",
                 "clallam", "clark", "king", "spokane", "whatcom", "spokane",
                 "whatcom", "benton", "snohomish", "cowlitz", "king", "clark",
                 "pierce", "snohomish", "yakima", "king", "chelan", "yakima",
                 "skagit", "pacific", "kittitas", "king", "franklin", "snohomish",
                 "king", "clark", "jefferson", "kitsap", "island")
    ),
    style = "deparse"
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
