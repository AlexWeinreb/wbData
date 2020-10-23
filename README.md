
# wbData

Easily download and use Wormbase data. Can be used to download or read
the “geneIDs” table or the GTF files from Wormbase, and provides
functions to convert between Wormbase IDs and gene symbols.

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

## Example of gene ID conversion

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

## Example reading gene coordinates

``` r
gene_coords <- wbData::wb_load_gene_coords(273)
gene_coords
#> # A tibble: 46,911 x 7
#>    gene_id       chr   start   end strand position                 gene_biotype 
#>    <chr>         <chr> <dbl> <dbl> <chr>  <chr>                    <chr>        
#>  1 WBGene000144~ MtDNA     1    55 +      MtDNA:         1-      ~ tRNA         
#>  2 WBGene000144~ MtDNA    58   111 +      MtDNA:        58-      ~ tRNA         
#>  3 WBGene000109~ MtDNA   113   549 +      MtDNA:       113-      ~ protein_codi~
#>  4 WBGene000109~ MtDNA   549   783 +      MtDNA:       549-      ~ protein_codi~
#>  5 WBGene000144~ MtDNA   785   840 +      MtDNA:       785-      ~ tRNA         
#>  6 WBGene000144~ MtDNA   842   896 +      MtDNA:       842-      ~ tRNA         
#>  7 WBGene000144~ MtDNA   898  1593 +      MtDNA:       898-     1~ rRNA         
#>  8 WBGene000144~ MtDNA  1595  1646 +      MtDNA:     1,595-     1~ tRNA         
#>  9 WBGene000144~ MtDNA  1648  1702 +      MtDNA:     1,648-     1~ tRNA         
#> 10 WBGene000144~ MtDNA  1707  1761 +      MtDNA:     1,707-     1~ tRNA         
#> # ... with 46,901 more rows
```

This can be used in conjunction with other packages, for example to look
up a gene model in IGV or JBrowse:

``` r
library(tidyverse)

gene_coords %>%
  filter(gene_id == s2i("unc-10", gids)) %>%
  pull(position)
  clipr::write_clip()
```

## Data management

The downloaded file is stored on the local computer to avoid
re-downloading every time. That way, `wbData` can also be used to obtain
Wormbase files to use with other software.

The cache directory can be specified in three ways:

  - Explicitly by specifying a path as argument to the package
    functions.
  - Through `option("wb_dir_cache")`. In that case leave the argument as
    `NULL`.
  - If the `dir_cache` argument is `NULL` and the option is not
    specified, a user-specific cache directory is chosen based on the
    operating system by [rappdirs](https://github.com/r-lib/rappdirs/).

The cache can be emptied with:

``` r
wb_clean_cache(273)
```
