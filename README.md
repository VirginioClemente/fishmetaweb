# fishmetaweb

An R package that predicts trophic interactions among ray-finned fishes. It
carries a fitted random-forest model together with the trait and phylogenetic
data the model reads, and answers two questions: how likely is it that one
species feeds on another, and what food web follows from a given list of
species.

The model is shipped fitted. Nothing is estimated when you use the package, and
no internet connection is required.

## Installation

```r
# install.packages("remotes")
remotes::install_github("VirginioClemente/fishmetaweb")
```

or, from a local copy of the repository:

```
R CMD INSTALL .
```

The package needs R 4.0 or later. `igraph` is optional: it is used to return a
graph object, and without it the same result is returned as an edge list.

## Use

```r
library(fishmetaweb)

# the probability that a bluefin tuna eats an anchovy
interaction_probability("Engraulis encrasicolus", "Thunnus thynnus")

# the pair is ordered, so the other direction is a different question
interaction_probability("Thunnus thynnus", "Engraulis encrasicolus")

# a food web for an assemblage
assemblage <- c("Engraulis encrasicolus", "Sardina pilchardus",
                "Thunnus thynnus", "Xiphias gladius", "Merluccius merluccius")
web <- build_metaweb(assemblage)
igraph::E(web)$probability
```

`build_metaweb()` scores every ordered pair that can be formed from the species
you supply and keeps those reaching the threshold. Edges run from prey to
predator and carry the predicted score as the edge attribute `probability`.
Species with no retained link stay in the graph as isolated vertices, so the
assemblage you asked for is the assemblage you get back.

Pass `as_igraph = FALSE` for a plain data frame, and `threshold = ` to use a cut
of your own:

```r
edges <- build_metaweb(assemblage, threshold = 0.8, as_igraph = FALSE)
```

The default cut, returned by `link_threshold()`, is the one used for the
published network. It weighs a missed link and a spurious link equally. Raise it
for a smaller and more confident web, lower it to favour completeness.

## Species the model does not cover

The model covers the species in its trait table, which you can list with
`metaweb_species()` and test with `has_species()`. A species is covered when
FishBase describes it and it has a place in the reference phylogeny; species
failing either condition have no predictors and cannot be scored.

Nothing fails silently. `interaction_probability()` returns `NA` for a pair
involving an unknown species, and `build_metaweb()` drops unknown species and
builds the web from the rest. Both warn, naming the species that were left out:

```r
> build_metaweb(c(assemblage, "Homo sapiens"))
Warning message:
1 species is not in the model and has been left out of the assembled web:
Homo sapiens
```

## What the predictions are

A score is the probability that the pair resembles a documented feeding
interaction in traits, ecology and phylogeny. It says that the predator could
plausibly consume the prey if the two met, not that the interaction happens, and
not that it matters in either species' diet.

The package imposes no geography. Two species may be scored as a plausible pair
even if their ranges never overlap, which is why a local web should be built
from the species actually present in the area of interest.

## Data behind the model

Traits come from FishBase. Entries missing there were completed by iterative
conditional imputation, so every covered species carries a complete predictor
vector; traits whose imputation could not be validated accurately enough were
excluded from the model.

Relatedness enters as twenty phylogenetic eigenvectors computed from the fully
sampled ray-finned fish phylogeny of the Fish Tree of Life (Rabosky et al. 2018),
in the version where species without molecular data are grafted within their
taxonomic group by the TACT procedure of Chang et al. (2020).

Documented interactions used for fitting come from the Global Biotic
Interactions database. Pairs treated as non-interacting are species recorded
together in the same biogeographic province within the same decade that the
database does not report as interacting; occurrence records come from OBIS and
GBIF.

## The global network

This package predicts interactions on demand. The complete predicted network for
all covered species is published separately as a figshare data deposit,
<https://doi.org/10.6084/m9.figshare.34840320>, and is the right starting point
when you need the whole edge list rather than a local web. That deposit holds
the full edge list as a Parquet file and as a compressed CSV, together with a
network summary table and the metadata of the projection run.

The fitted pipeline that produced the model is deposited at
<https://doi.org/10.6084/m9.figshare.34840326>.

## Paper

The model, its validation and the released network are described in the
following work, which we ask you to cite when you use these predictions,

> TO COMPLETE - authors, title, journal, year, DOI

## Licence

CC BY 4.0.
