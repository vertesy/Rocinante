# Proposed follow-up tasks

Each item below is one small pull request. First add characterization tests in Rocinante, then copy the helper and those tests to the proposed destination. **Do not remove or redirect the Rocinante implementation until downstream adoption is independently confirmed.** Confirmation must come from the destination package's released or pinned version and from its downstream callers; passing the migration pull request alone is not confirmation.

For every case, compare the destination helper with the source helper in a clean R session. “Same” below means `identical()` unless graphics require snapshot or device-output comparison. Record warnings and errors as messages for compatibility purposes.

## Clipboard helpers

### Task: migrate `clip2clip.vector()`

- **Source:** `R/Rocinante.R`; **destination:** `DataInCode`.
- **Characterize:** clipboard text containing one value, multiple newline-delimited values, character values requiring quoting, and `NA`; also characterize an unavailable clipboard.
- **Compatibility:** same return value, class, names, and dimensions; same printed output/messages; byte-for-byte clipboard content, including quotes, escapes, separators, and final newline; graphics: not applicable.

### Task: migrate `clip2clip.commaSepString()`

- **Source:** `R/Rocinante.R`; **destination:** `DataInCode`.
- **Characterize:** one token, several comma-separated tokens, empty tokens, whitespace around tokens, quoted-looking text, and `NA`; also characterize an unavailable clipboard.
- **Compatibility:** same return value, class, names, and dimensions; same printed output/messages; byte-for-byte clipboard content, including retained whitespace, quotes, separators, and final newline; graphics: not applicable.

### Task: migrate `write_clip.replace.dot.with.comma()`

- **Source:** `R/Rocinante.R`; **destination:** `DataInCode`.
- **Characterize:** numeric vector, named numeric vector, matrix/data frame, missing values, default `decimal_mark`, and a non-default decimal mark; also characterize an unavailable clipboard.
- **Compatibility:** same return value, class, names, and dimensions; same printed output/messages; byte-for-byte clipboard content, including decimal mark, row/column layout, missing-value representation, and final newline; graphics: not applicable.

## Calculation helpers

### Task: migrate `eucl.dist.pairwise()`

- **Source:** `R/Rocinante.R`; **destination:** `CodeAndRoll2`.
- **Characterize:** two-column numeric matrix and data frame, with and without row names; equal, negative, decimal, and missing values; zero rows; and invalid one- or three-column input.
- **Compatibility:** same return value (including numeric precision and `NA` propagation), class, names, and dimensions; same printed output/messages and errors; clipboard content and graphics: not applicable.

### Task: migrate `sign.dist.pairwise()`

- **Source:** `R/Rocinante.R`; **destination:** `CodeAndRoll2`.
- **Characterize:** two-column numeric matrix and data frame, with and without row names; equal, negative, decimal, and missing values; zero rows; and invalid one- or three-column input.
- **Compatibility:** same return value (including numeric precision and `NA` propagation), class, names, and dimensions; same printed output/messages and errors; clipboard content and graphics: not applicable.

### Task: migrate `rowACF()`

- **Source:** `R/Rocinante.R`; **destination:** `CodeAndRoll2`.
- **Characterize:** named numeric matrix and data frame; one and multiple rows; complete, constant, and missing-value series; default arguments, explicit `na_pass`, `plot = TRUE`, and forwarded `acf()` arguments.
- **Compatibility:** same return value and component values, class, names, and dimensions, including `apply()` simplification; same printed output/messages and warnings; same graphics/device output when plotting; clipboard content: not applicable.

### Task: migrate `colACF()`

- **Source:** `R/Rocinante.R`; **destination:** `CodeAndRoll2`.
- **Characterize:** named numeric matrix and data frame; one and multiple columns; complete, constant, and missing-value series; default arguments, explicit `na_pass`, `plot = TRUE`, and forwarded `acf()` arguments.
- **Compatibility:** same return value and component values, class, names, and dimensions, including `apply()` simplification; same printed output/messages and warnings; same graphics/device output when plotting; clipboard content: not applicable.

### Task: migrate `acf.exactLag()`

- **Source:** `R/Rocinante.R`; **destination:** `CodeAndRoll2`.
- **Characterize:** named and unnamed complete series, constant series, and series with missing values; lag zero, default lag, a higher valid lag, and an out-of-range lag; explicit `na_pass`, `plot = TRUE`, and forwarded `acf()` arguments.
- **Compatibility:** same return value (including selected lag, precision, and `NA` behavior), class, names, and dimensions; same printed output/messages, warnings, and errors; same graphics/device output when plotting; clipboard content: not applicable.

### Task: migrate `rowACF.exactLag()`

- **Source:** `R/Rocinante.R`; **destination:** `CodeAndRoll2`.
- **Characterize:** named numeric matrix and data frame; one and multiple rows; complete, constant, and missing-value series; lag zero, default lag, and a higher valid lag; explicit `na_pass`, `plot = TRUE`, and forwarded `acf()` arguments.
- **Compatibility:** same two-significant-digit return values and missing-value behavior, class, names, and dimensions, including `apply()` simplification; same printed output/messages, warnings, and errors; same graphics/device output when plotting; clipboard content: not applicable.

### Task: migrate `colACF.exactLag()`

- **Source:** `R/Rocinante.R`; **destination:** `CodeAndRoll2`.
- **Characterize:** named numeric matrix and data frame; one and multiple columns; complete, constant, and missing-value series; lag zero, default lag, and a higher valid lag; explicit `na_pass`, `plot = TRUE`, and forwarded `acf()` arguments.
- **Compatibility:** same two-significant-digit return values and missing-value behavior, class, names, and dimensions, including `apply()` simplification; same printed output/messages, warnings, and errors; same graphics/device output when plotting; clipboard content: not applicable.

## Plotting helpers

### Task: migrate `colSums.barplot()`

- **Source:** `R/Rocinante.R` and the duplicate at `R/Rocinante.less.used.R`; **destination:** `ggExpress`.
- **Characterize:** named numeric matrix and data frame; positive, negative, zero, and missing values; defaults, `na_rm = FALSE`, custom `col`, and forwarded `barplot()` arguments such as horizontal orientation and main title.
- **Compatibility:** same return value (bar midpoints), class, names, and dimensions; same printed output/messages and warnings; pixel-equivalent graphics and identical plotting geometry, labels, colors, orientation, limits, and titles; clipboard content: not applicable.

### Task: migrate `panelCorSpearman()`

- **Source:** `R/Rocinante.R`; **destination:** `ggExpress`.
- **Characterize:** complete vectors, paired missing values, positive and negative association, ties, and zero correlation; defaults, each supported correlation `method`, custom `digits`, `prefix`, and `cex.cor`, plus omitted `cex.cor` to exercise `missing()`.
- **Compatibility:** same return value, class, names, and dimensions; same printed output/messages and warnings from correlation tests; pixel-equivalent graphics with identical coefficient/significance text, positions, sizes, and colors, and the same restoration of graphics parameters; clipboard content: not applicable.

## Equation-formatting helpers

### Task: migrate `lm_equation_formatter()`

- **Source:** `R/Rocinante.less.used.R`; **destination:** `ggExpress`.
- **Characterize:** `lm()` fits with positive, negative, zero, and missing (`NA`) slope coefficients, plus a fit with more than one predictor; capture the current default precision of `signif()`.
- **Compatibility:** same formatted return value character-for-character (spacing, labels, sign, and precision), class, names, and dimensions; same printed output/messages and errors; clipboard content and graphics: not applicable.

### Task: migrate `lm_equation_formatter2()`

- **Source:** `R/Rocinante.less.used.R`; **destination:** `ggExpress`.
- **Characterize:** `lm()` fits with positive, negative, zero, and missing (`NA`) intercept or slope, plus a fit with more than one predictor; include coefficients that exercise three-significant-digit rounding.
- **Compatibility:** same formatted return value character-for-character (spacing, operators, sign, and precision), class, names, and dimensions; same printed output/messages and errors; clipboard content and graphics: not applicable.

### Task: migrate `lm_equation_formatter3()`

- **Source:** `R/Rocinante.less.used.R`; **destination:** `ggExpress`.
- **Characterize:** `lm()` fits with positive, negative, zero, and missing (`NA`) intercept or slope; default and custom `y.var.name`/`x.var.name`; coefficients that exercise three-significant-digit rounding; and a fit with more than one predictor.
- **Compatibility:** same formatted return value character-for-character (variable names, spacing, operators, sign, and precision), class, names, and dimensions; same printed output/messages and errors; clipboard content and graphics: not applicable.

## Database-link helpers

### Task: migrate `link_SNPedia_clip2clip()`

- **Source:** `R/Rocinante.less.used.R`; **destination:** `DatabaseLinke.R`.
- **Characterize:** one and multiple rsIDs supplied as one-column tibbles; missing and special-character IDs; default Excel-link mode, plain-link mode, Markdown-link mode, both format flags true, and a custom `searchQueryPrefix`; also characterize an unavailable clipboard.
- **Compatibility:** same return value, class, names, and dimensions; identical URLs and table structure; same printed output/messages and branch precedence; byte-for-byte clipboard content, including formulas/Markdown, row and column separators, quoting, and final newline; graphics: not applicable.

### Task: migrate `link_Franklin_clip2clip()`

- **Source:** `R/Rocinante.less.used.R`; **destination:** `DatabaseLinke.R`.
- **Characterize:** one and multiple two-column coordinate/allele rows, missing values, and special characters; default Excel-link and plain-link modes; a custom `searchQueryPrefix`; invalid one- and three-column inputs; also characterize an unavailable clipboard.
- **Compatibility:** same return value, class, names, and dimensions; identical coordinate normalization, URLs, and table structure; same printed output/messages and errors; byte-for-byte clipboard content, including formulas, row and column separators, quoting, and final newline; graphics: not applicable.

### Task: migrate `link_VarSome_clip2clip()`

- **Source:** `R/Rocinante.less.used.R`; **destination:** `DatabaseLinke.R`.
- **Characterize:** one and multiple rsIDs supplied as one-column tibbles; missing and special-character IDs; default Excel-link mode, plain-link mode, Markdown-link mode, both format flags true, custom `hg`, custom prefix, and custom suffix; also characterize an unavailable clipboard.
- **Compatibility:** same return value, class, names, and dimensions; identical URLs, link labels, and table structure; same printed output/messages and branch precedence; byte-for-byte clipboard content, including formulas/Markdown, row and column separators, quoting, and final newline; graphics: not applicable.
