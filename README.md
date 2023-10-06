
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

To load the geneIDs table for Wormbase release WS273 and convert between
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
head(gene_coords)
#> # A tibble: 6 × 7
#>   gene_id        chr   start   end strand position                  gene_biotype
#>   <chr>          <chr> <int> <int> <chr>  <chr>                     <chr>       
#> 1 WBGene00014450 MtDNA     1    55 +      MtDNA:         1-       … tRNA        
#> 2 WBGene00014451 MtDNA    58   111 +      MtDNA:        58-       … tRNA        
#> 3 WBGene00010957 MtDNA   113   549 +      MtDNA:       113-       … protein_cod…
#> 4 WBGene00010958 MtDNA   549   783 +      MtDNA:       549-       … protein_cod…
#> 5 WBGene00014452 MtDNA   785   840 +      MtDNA:       785-       … tRNA        
#> 6 WBGene00014453 MtDNA   842   896 +      MtDNA:       842-       … tRNA
```

This can be used in conjunction with other packages, for example to look
up a gene model in IGV or JBrowse:

``` r

gene_coords |>
  dplyr::filter(gene_id == s2i("unc-10", gids)) |>
  dplyr::pull(position) |>
  clipr::write_clip()

clipr::read_clip()
```

## Reading the CGC strain list

You can load the list of strains available at the CGC. The downloaded
list is saved in cache, and by default is only downloaded again if the
cached file is more than 2 days old. Use the `refresh` argument to force
refreshing.

``` r
strain_list <- wb_load_cgc_list()
nrow(strain_list)
#> [1] 24536
```

For example, looking at a specific strain:

``` r
strain_list$Genotype[strain_list$Strain == "NC902"]
#> [1] "unc-119(ed3) III; wdEx381."
```

Or looking for strains that have particular characteristics. For
example, using a [regular
expression](https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html)
to look for strains that have *hbl-1* potentially associated with a red
fluorescent protein:

``` r

fluo_keywords <- c("cherry", "tomato", "scarlet", "rfp", " red ")
my_pattern <- paste("hbl-1", ".*::.*", fluo_keywords, sep="", collapse = "|")
with_fluo <- which(grepl(my_pattern, tolower(strain_list$Genotype)) | grepl(my_pattern, tolower(strain_list$Description)))
strain_list[with_fluo,]
#> # A tibble: 3 × 8
#>   Strain Species      Genotype Description Mutagen Outcrossed `Made by` Received
#>   <chr>  <chr>        <chr>    <chr>       <chr>   <chr>      <chr>     <chr>   
#> 1 VT3751 C. elegans   maIs105… maIs105 [c… Crispr… x2         Orkan Il… 06/28/1…
#> 2 VT3869 <em>Caenorh… wIs51 V… wIs51 [SCM… Crispr… x2         Orkan Il… 03/01/2…
#> 3 VT3922 <em>Caenorh… lin-28(… Precocious… Crispr… x2         Orkan Il… 03/01/2…
```

## Data management

The downloaded file is stored on the local computer to avoid
re-downloading every time. That way, `wbData` can also be used to obtain
Wormbase files to use with other software.

The cache directory can be specified in three ways:

- Explicitly by specifying a path as argument to the package functions.
- Through `option("wb_dir_cache")`. In that case leave the argument as
  `NULL`.
- If the `dir_cache` argument is `NULL` and the option is not specified,
  a user-specific cache directory is chosen based on the operating
  system by [rappdirs](https://github.com/r-lib/rappdirs/).

To only list the files in cache without deleting them:

``` r
wb_clean_cache(273, delete = FALSE)
#> [1] "C:\\Users\\ALEXIS~1\\AppData\\Local/wbData/wbData/Cache/c_elegans.PRJNA13758.WS273.canonical_geneset.gtf.gz"
#> [2] "C:\\Users\\ALEXIS~1\\AppData\\Local/wbData/wbData/Cache/c_elegans.PRJNA13758.WS273.geneIDs.txt.gz"
```

The cache can be emptied with:

``` r
wb_clean_cache(273)
```
