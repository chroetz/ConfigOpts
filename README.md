# ConfigOpts

Defines an S3 class Opts. It stores configuratio options that can be used as arguemnts for function calls.

Example:
```r
library(ConfigOpts)
opts <- makeOpts("Sequence", range = c(-1,2))
print(opts)
writeOpts(opts, "opts.json")
```
