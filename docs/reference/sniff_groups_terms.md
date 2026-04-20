# Extract and Analyze Key Terms from Research Groups

Identifies and extracts key terms from titles and abstracts of
publications within different research groups using natural language
processing techniques, and computes term statistics including TF-IDF
scores.

## Usage

``` r
sniff_groups_terms(
  net_groups,
  algorithm = "rake",
  phrase_pattern = "(A|N)*N(P+D*(A|N)*N)*",
  model_dir = tempdir(),
  n_cores = 1,
  show_progress = TRUE,
  n_terms = 15,
  min_freq = 2,
  digits = 4
)
```

## Arguments

- net_groups:

  A list containing network data with publication information. Must
  include elements: `network` (with vertex attributes 'group', 'TI',
  'AB'), `pubs_by_year`, and `aggregate`.

- algorithm:

  Term extraction algorithm to use. Options are:

  - "rake" - Rapid Automatic Keyword Extraction (default)

  - "pointwise" - Pointwise Mutual Information

  - "phrase" - Phrase pattern matching

- phrase_pattern:

  Regular expression pattern for phrase extraction when algorithm =
  "phrase" (default: "(A\|N)*N(P+D*(A\|N)*N)*")

- model_dir:

  Directory where UDPipe models are stored (default: tempdir())

- n_cores:

  Number of CPU cores to use for parallel processing (default: 1)

- show_progress:

  Logical indicating whether to show progress bar (default: TRUE)

- n_terms:

  Number of top terms to return in summary table (default: 15)

- min_freq:

  Minimum frequency threshold for terms (default: 2)

- digits:

  Number of decimal places to round numerical values (default: 4)

## Value

A list with two components:

- `terms_by_group`: A named list (by group) of data frames containing
  extracted terms with statistics

- `terms_table`: A summary tibble with top terms by frequency and TF-IDF
  for each group

## Details

This function performs the following steps:

1.  Validates input structure and parameters

2.  Loads the UDPipe language model from the specified directory

3.  Processes text data (titles and abstracts) for each group

4.  Applies the selected term extraction algorithm (RAKE, PMI, or phrase
    patterns)

5.  Computes term frequencies and TF-IDF scores

6.  Returns ranked terms for each research group with comprehensive
    statistics

The function uses UDPipe for tokenization, lemmatization and POS tagging
before term extraction. For phrase extraction, the default pattern finds
noun phrases.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming groups is output from sniff_groups()
terms <- sniff_groups_terms(groups, algorithm = "rake")

# View terms for first group
head(terms$terms_by_group[[1]])

# View summary table
print(terms$terms_table)

# Customized extraction with custom model directory
net_groups_terms <- sniff_groups_terms(net_groups,
  algorithm = "phrase",
  model_dir = tempdir(),
  n_terms = 10,
  min_freq = 3,
  n_cores = 4
)
} # }
```
