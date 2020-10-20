
# wbData

<!-- badges: start -->

<!-- badges: end -->

Personal package to easily download and use Wormbase data. At this
point, it only downloads the “geneIDs” table and provides function to
convert between Wormbase IDs and gene symbols. I may add functions to
download and access GTF files later.

The three advantages are:

  - finding files for any given Wormbase release (even though the actual
    path changes)
  - keeping all these files in a single place
  - facilitating common operations.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AlexWeinreb/wbData")
```

## Example

To load the geneIDs table for Wormbase release WS277 and convert between
gene ID and gene symbol:

``` r
library(wbData)

gids <- wb_load_gene_ids("WS273")
s2i("unc-10", gids)
#> [1] "WBGene00006750"
i2s(c("WBGene00006752", "WBGene00004412"), gids)
#> [1] "unc-13" "rpl-1"
```

The downloaded file is stored on the local computer to avoid
re-downloading every time. The cache can be emptied as follows:

``` r
wb_clean_cache(273)
```
