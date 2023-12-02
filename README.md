

[![Anurag's github stats](https://github-readme-stats.vercel.app/api?username=jokergoo&theme=radical&show_icons=true)](https://github.com/anuraghazra/github-readme-stats)

Activities on my repos (R packages, 2013-04-18 ~ 2022-12-10): (code for generating this plot: https://jokergoo.github.io/spiralize_vignettes/examples.html#github-commits)

<details>
<summary>code</summary>
  
```r

library(spiralize)
library(grid)

repos = c("GlobalOptions", "GetoptLong", "circlize", "bsub", "pkgndep", "ComplexHeatmap", "EnrichedHeatmap", 
    "HilbertCurve", "gtrellis", "cola", "simplifyEnrichment", "InteractiveComplexHeatmap", "spiralize", "rGREAT", "simona")

df_all = data.frame(commits = numeric(0), date = character(0), repo = character(0))
for(r in repos) {
    # go to each repo folder
    setwd(paste0("~/project/development/", r))
    df = read.table(pipe("git log --date=short --pretty=format:%ad | sort | uniq -c"))
    colnames(df) = c("commits", "date")
    df$repo = r

    df_all = rbind(df_all, df)
}

df_all$date = as.Date(df_all$date)

start = min(df_all$date)
end = max(df_all$date)

d = start + seq(1, end - start + 1) - 1
n = numeric(length(d))
nl = lapply(repos, function(x) numeric(length(d)))
names(nl) = repos

for(i in seq_len(nrow(df_all))) {
    ind = as.double(difftime(df_all[i, "date"], start), "days") + 1
    n[ind] = n[ind] + df_all[i, "commits"]

    nl[[ df_all[i, "repo"] ]][ind] = nl[[ df_all[i, "repo"] ]][ind] + df_all[i, "commits"]
}

calc_pt_size = function(x) {
    pt_size = x
    pt_size[pt_size > 20] = 20
    pt_size[pt_size < 2 & pt_size > 0] = 2
    pt_size
}
xlim = range(d)

pl = list()
pl[[1]] = grid.grabExpr({
    spiral_initialize_by_time(xlim, verbose = FALSE, normalize_year = TRUE)
    spiral_track()
    spiral_points(d, 0.5, pch = 16, size = unit(calc_pt_size(n), "pt"))
    grid.text("All packages", x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 14))

    for(t in c("2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01",
               "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01")) {
        spiral_text(t, 0.5, gsub("-\\d+-\\d+$", "", as.character(t)), gp = gpar(fontsize = 8), facing = "inside")
    }
})

for(i in order(sapply(nl, sum), decreasing = TRUE)) {
    pl[[ names(nl)[i] ]] = grid.grabExpr({
        spiral_initialize_by_time(xlim, verbose = FALSE, normalize_year = TRUE)
        spiral_track()
        spiral_points(d, 0.5, pch = 16, size = unit(calc_pt_size(nl[[i]]), "pt"))
        grid.text(names(nl)[i], x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 14))

        for(t in c("2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01",
                   "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01")) {
            spiral_text(t, 0.5, gsub("-\\d+-\\d+$", "", as.character(t)), gp = gpar(fontsize = 8), facing = "inside")
        }
    })
}

library(cowplot)
png("~/test.png", 300*4*1.5, 300*4*1.5, res = 72*1.5)
plot_grid(plotlist = pl, ncol = 4)
dev.off()
```

</details>

![test](https://github.com/jokergoo/jokergoo/assets/449218/247a86f7-d425-4e4d-b7f7-900f6035de68)
