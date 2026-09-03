# Rocinante
A collection of custom R functions. Helper functions complementing [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2). Many functionalities were part of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll).


<br><br>

## Installation

1.) [Download `Rocinante.R`](https://github.com/vertesy/Rocinante/blob/main/R/Rocinante.R), save as local `.R` file, and `source(~/path/to/Rocinante.R)`: 

2.) Directly source from the web:

```R
source("https://raw.githubusercontent.com/vertesy/Rocinante/main/R/Rocinante.R")
```
<br>

### Troubleshooting

*If you encounter a **bug**, something doesn't work or unclear, please let me know by raising an issue on [Rocinante](https://github.com/vertesy/Rocinante/issues) – Please check if it has been asked.*<br>

## List of Functions in Rocinante.R (84)

Updated: 2026/08/25 17:28

- #### 1 `get_col()`

  Extract a column from a matrix as a vector.

- #### 2 `get_row()`

  Extract a row from a matrix as a tibble.

- #### 3 `get_subvec()`

  Extract a range of elements from a vector.

- #### 4 `format_decimal()`

  Format numbers without scientific notation.

- #### 5 `kk()`

  Keep an R session alive and report elapsed time every five minutes.

- #### 6 `stry()`

  Evaluate an expression with errors suppressed.

- #### 7 `warnings.erase()`
- #### 8 `rprofile()`
- #### 9 `rocinanteSource()`
- #### 10 `lock_current_file()`
- #### 11 `unlock_current_file()`
- #### 12 `repoTrafficGraph_ALL()`
- #### 13 `openALL_CreatePackageFiles()`
- #### 14 `openALL_ConfigFiles()`
- #### 15 `d.all()`
- #### 16 `r.all()`
- #### 17 `helpPak()`
- #### 18 `# ooo()`
- #### 19 `ccc()`
- #### 20 `oofix()`
- #### 21 `osXpath7()`
- #### 22 `osXpath()`
- #### 23 `cbepath()`
- #### 24 `getCurrentScriptName()`
- #### 25 `getCurrentScriptPath()`
- #### 26 `listFunctionsByPackage()`
- #### 27 `sourceGitHub()`
- #### 28 `sourceLines()`
- #### 29 `sourcePartial()`
- #### 30 `args.2.global()`
- #### 31 `memory.biggest.objects()`
- #### 32 `# printEveryN()`
- #### 33 `say()`
- #### 34 `sayy()`
- #### 35 `oo()`
- #### 36 `view.head()`
- #### 37 `view.head2()`
- #### 38 `unload()`
- #### 39 `backup()`
- #### 40 `list.dirs.depth.n()`
- #### 41 `list_subdirectories_at_depth()`
- #### 42 `iidentical.names()`
- #### 43 `iidentical()`
- #### 44 `iidentical.all()`
- #### 45 `findFunctionPackage()`
- #### 46 `clip2clip.vector()`
- #### 47 `clip2clip.commaSepString()`
- #### 48 `write_clip.replace.dot.with.comma()`
- #### 49 `PCA.percent.var.explained()`
- #### 50 `eucl.dist.pairwise()`
- #### 51 `sign.dist.pairwise()`
- #### 52 `rowACF()`
- #### 53 `colACF()`
- #### 54 `acf.exactLag()`
- #### 55 `rowACF.exactLag()`
- #### 56 `colACF.exactLag()`
- #### 57 `colSums.barplot()`
- #### 58 `richColors()`
- #### 59 `qqheatmap()`
- #### 60 `legend.col()`
- #### 61 `# panelCorPearson()`
- #### 62 `panelCorSpearman()`
- #### 63 `quantile_breaks()`
- #### 64 `hclust.getOrder.row()`
- #### 65 `hclust.getOrder.col()`
- #### 66 `hclust.getClusterID.row()`
- #### 67 `hclust.getClusterID.col()`
- #### 68 `hclust.ClusterSeparatingLines.row()`
- #### 69 `hclust.ClusterSeparatingLines.col()`
- #### 70 `Gap.Postions.calc.pheatmap()`
- #### 71 `matlabColors.pheatmap()`
- #### 72 `annot_col.create.pheatmap.vec()`
- #### 73 `annot_col.create.pheatmap.df()`
- #### 74 `annot_col.fix.numeric()`
- #### 75 `annot_row.create.pheatmap.df()`
- #### 76 `val2col()`
- #### 77 `ssh2osX()`
- #### 78 `osX2ssh()`
- #### 79 `STRINGdb.reformat.ann.table.per.gene()`
- #### 80 `rnd4l()`
- #### 81 `# fractions()`
- #### 82 `# unique.wNames()`
- #### 83 `# findGlobals2()`
- #### 84 `# checkStrict()`

## List of Functions in Rocinante.less.used.R (26)

Updated: 2026/08/25 17:28
- #### 1 `getMemoryInfo()`
Retrieve Memory Information. 

- #### 2 `    extract_pages()`
Extract a page count from macOS memory information.

- #### 3 `plotMemoryUsage()`
Plot system and R object memory usage.

- #### 4 `getSLURMjobDetails()`
Retrieve details of the current SLURM job for a specified user.

- #### 5 `  run_command()`
Run a system command and return its output.

- #### 6 `colSums.barplot()`
- #### 7 `hist.XbyY()`
- #### 8 `getMemoryInfoSimple()`
- #### 9 `    extract_pages()`
- #### 10 `plotMemoryUsageSimple()`
- #### 11 `# qheatmap()`
- #### 12 `lm_equation_formatter()`
- #### 13 `lm_equation_formatter2()`
- #### 14 `lm_equation_formatter3()`
- #### 15 `GC_content()`
- #### 16 `getSequences.DNAStringSet()`
- #### 17 `link_SNPedia_clip2clip()`
- #### 18 `link_Franklin_clip2clip()`
- #### 19 `link_VarSome_clip2clip()`
- #### 20 `getVennOverlaps()`
- #### 21 `ww.randomize()`
- #### 22 `append_non_na()`
- #### 23 `dateOK()`
- #### 24 `dateAndTime()`
- #### 25 `backupRprofile()`
- #### 26 `# make_bash_compatible()`
