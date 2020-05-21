library(gh)

res <- gh(
  "POST /repos/:owner/:repo/issues",
  owner = "zhiiiyang",
  repo = "OTworkout",
  title = Sys.Date(),
  body = "Please post your OT workout screenshot!"
)