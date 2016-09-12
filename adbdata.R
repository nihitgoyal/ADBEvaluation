path <- file.path("input")
all <- list.files(path)
all.files <- file.path(path, all)
txt <- lapply(all.files, readLines)
nms <- gsub("input", "", all.files)
evaluations <- setNames(txt, nms)
evaluations <- sapply(evaluations, function(x) paste(x, collapse = " "))

save(evaluations, file = "data/evaluations.rdata", compress = "xz")