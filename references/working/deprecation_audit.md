# ggplot2 Deprecation Audit — Superpower

## Status: Complete

## Audit Results

### `aes_string()` Usage

No instances found in `R/` or `inst/`. Previous commits (014fbb9, 4fdb9ae) already replaced all `aes_string()` calls with `aes(.data[[]])` in:

- `R/ANOVA_design.R` (lines 348-350)
- `R/ANOVA_exact.R` (lines 354-358, 759-763)

Note: `mess/` directory still contains `aes_string()` in `ANOVA_exact_dev.R` and `old_design_function.R`, but these files are not part of the built package.

### `size` → `linewidth` in Line Geoms

No remaining instances. Previous commits already replaced `size` with `linewidth` in:

- `R/ANOVA_design.R`: `geom_errorbar(..., linewidth = .6, ...)` (lines 358, 367)
- `R/plot_power.R`: `geom_line(linewidth = 1.5)` and `geom_line(..., linewidth = 1)` (lines 286, 291, 326, 329, 367, 370)
- `R/minimize_balance_alpha.R`: `geom_line(linewidth = 1.3)` (line 82)

Remaining `size =` usages are all correct (in `geom_point()` or `base_size` theme args):

- `R/ANOVA_design.R:356,365`: `geom_point(..., size = 5, ...)` — correct for point geoms
- `R/minimize_balance_alpha.R:83`: `geom_point(..., size = 3)` — correct for point geoms

### `guide = FALSE`

No instances found in `R/` or `inst/`.

### `qplot()` Usage

No instances found in `R/` or `inst/`.

### Other Deprecated Usage (`aes_q`, `aes_all`, `aes_auto`)

No instances found in `R/` or `inst/`.

## Changes Made (This Session)

### 1. Added `rlang` import for `.data` pronoun

The previous commits introduced `.data[[]]` usage but did not add the corresponding rlang import. This could cause R CMD check NOTEs.

**DESCRIPTION** — Added `rlang` to `Imports` field.

**R/ANOVA_design.R** — Added `#' @importFrom rlang .data` to the roxygen2 block (line 49).

**NAMESPACE** — Regenerated via `devtools::document()`. Now contains `importFrom(rlang,.data)`.

### 2. Added `.claude` to `.Rbuildignore`

Added `^\.claude$` to prevent the `.claude/` directory from triggering a NOTE during R CMD check.

### 3. RoxygenNote updated

`devtools::document()` updated `RoxygenNote` in DESCRIPTION from `7.3.2` to `7.3.3` (reflects installed roxygen2 version).

## Import Changes

- [x] `@importFrom rlang .data` added to `R/ANOVA_design.R`
- [x] `rlang` added to DESCRIPTION `Imports`
- [x] `devtools::document()` run after import changes
- [x] NAMESPACE regenerated with `importFrom(rlang,.data)`

## Verification

- [x] `devtools::check()` passes: 0 errors, 0 warnings, 1 NOTE (`.claude` dir — now excluded via `.Rbuildignore`)
- [x] No ggplot2 deprecation warnings at runtime
- [x] All existing tests pass
