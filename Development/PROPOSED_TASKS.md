# Proposed follow-up tasks

Each task is intended for a separate, small pull request. Preserve every function's output value and format.

## Move clipboard-to-code helpers

- Review `clip2clip.vector()`, `clip2clip.commaSepString()`, and `write_clip.replace.dot.with.comma()` for `DataInCode`.
- Copy one helper at a time, add focused tests, and leave the Rocinante implementation unchanged until downstream use is verified.

## Move general calculation helpers

- Review `eucl.dist.pairwise()`, `sign.dist.pairwise()`, and the ACF helpers for `CodeAndRoll2`.
- Record representative outputs before copying code, then compare names, types, dimensions, values, and printed output exactly.

## Move plotting helpers

- Review `colSums.barplot()`, `panelCorSpearman()`, and the `lm_equation_formatter*()` helpers for `ggExpress`.
- Capture plot objects or graphics output before migration and compare them without changing defaults.

## Move database-link helpers

- Review `link_SNPedia_clip2clip()`, `link_Franklin_clip2clip()`, and `link_VarSome_clip2clip()` for `DatabaseLinke.R`.
- Test URL strings, table structure, clipboard content, and console messages before replacing the Rocinante copies.
