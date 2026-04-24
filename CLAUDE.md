# CLAUDE.md — Superpower R Package

## Project Overview

Superpower is an R package for simulation-based power analysis of ANOVA designs. It is available on CRAN and maintained by Aaron Caldwell. The package includes Shiny apps, extensive vignettes, and several exported functions that produce ggplot2 visualizations.

Repository: https://github.com/arcaldwell49/Superpower

## Code Style Conventions

- Use `# descriptive header --------` for section headers in R scripts (not blocks of `#` symbols).
- Follow tidyverse-adjacent style: snake_case for variable/function names, spaces around operators.
- Roxygen2 is used for all documentation (`#'` comments above exported functions).
- The package uses `@importFrom` directives rather than blanket `@import` where possible (exception: ggplot2).
- When writing or modifying code, keep changes minimal and targeted. Do not refactor surrounding code unless it is directly related to the fix.

## References Folder

The `references/` directory contains supporting context for Claude Code tasks.

- `references/general/` — General reference materials (style guides, package conventions, relevant documentation).
- `references/working/` — Working notes, scratchpad files, and task-specific context for in-progress work.

This folder is excluded from the R package build (listed in `.Rbuildignore`).

## Package Check

After any code changes, always run:

```r
devtools::check()
```

All existing tests must continue to pass. If `devtools::check()` produces new NOTEs, WARNINGs, or ERRORs that were not present before your changes, treat them as problems to resolve before considering the task complete.

## Branch Policy

Commit to whatever branch is currently checked out. If you believe the current branch is unexpected or could cause issues (e.g., you're on `main`/`master` when a feature branch would be more appropriate), raise the concern as a comment or issue so the maintainer can verify before proceeding.

---

## Current Task: Fix All Deprecated ggplot2 Usage

### Problem

The package triggers deprecation warnings from ggplot2 at runtime. Known deprecations include `aes_string()` and `size` aesthetic for line-based geoms, but there may be others. The goal is to audit the entire package and fix all deprecated ggplot2 usage.

### Step 1: Setup — .Rbuildignore

Before doing anything else, ensure the following entries exist in `.Rbuildignore` (add them if missing):

```
^CLAUDE\.md$
^references$
```

These prevent the CLAUDE.md file and references folder from being included in the built package.

### Step 2: Audit — Find All Deprecated Usage

Search all `.R` files under `R/` and any Shiny app files (check `inst/` as well) for deprecated ggplot2 patterns. At minimum, grep for:

```bash
# aes_string (deprecated since ggplot2 3.0.0)
grep -rn "aes_string" R/ inst/

# size aesthetic in line-based geoms (deprecated since ggplot2 3.4.0)
# Look for size in geom_line, geom_errorbar, geom_path, geom_segment, etc.
grep -rn "size\s*=" R/ inst/ | grep -iE "geom_line|geom_path|geom_errorbar|geom_segment|geom_abline|geom_hline|geom_vline|geom_step|geom_ribbon|element_line"

# qplot (deprecated since ggplot2 3.4.0)
grep -rn "qplot" R/ inst/

# stat = "identity" misuse or other deprecated stat usage
grep -rn 'stat\s*=' R/ inst/

# guide = FALSE (deprecated; should be guide = "none")
grep -rn 'guide\s*=\s*FALSE' R/ inst/

# Other potential deprecations: check for any use of gg-deprecated functions
grep -rn "aes_q\|aes_all\|aes_auto" R/ inst/
```

Record every file and line number with deprecated usage. Create a checklist in `references/working/deprecation_audit.md` to track progress.

### Step 3: Fix — Replacement Patterns

Apply these replacements throughout the codebase:

#### `aes_string()` → `aes()` with `.data[[]]` pronoun

The `.data` pronoun from rlang allows tidy evaluation with string variable names inside `aes()`. This is the standard approach for packages (see `vignette("ggplot2-in-packages")`).

**Before:**
```r
aes_string(x = "varname", y = "response", colour = "group")
```

**After:**
```r
aes(x = .data[["varname"]], y = .data[["response"]], colour = .data[["group"]])
```

If the variable name is stored in a character object rather than a literal string:

**Before:**
```r
aes_string(x = my_var_name)
```

**After:**
```r
aes(x = .data[[my_var_name]])
```

#### `size` → `linewidth` in line-based geoms

This applies to: `geom_line`, `geom_path`, `geom_errorbar`, `geom_segment`, `geom_abline`, `geom_hline`, `geom_vline`, `geom_step`, `geom_crossbar`, `geom_ribbon` (for the outline), and `element_line()` in theme calls.

**Before:**
```r
geom_errorbar(..., size = 0.6)
```

**After:**
```r
geom_errorbar(..., linewidth = 0.6)
```

Note: `size` is still correct for point-based geoms (`geom_point`, `geom_jitter`, `geom_text`, etc.) and for the `size` aesthetic when mapping to point size. Only change `size` to `linewidth` when it controls the width of lines.

#### `guide = FALSE` → `guide = "none"`

**Before:**
```r
scale_colour_manual(..., guide = FALSE)
```

**After:**
```r
scale_colour_manual(..., guide = "none")
```

#### `qplot()` → `ggplot()` + `geom_*()`

Replace any `qplot()` calls with explicit `ggplot()` + geom layer equivalents.

#### `aes_q()` / `aes_all()` / `aes_auto()` → `aes()` with `.data[[]]`

Same approach as `aes_string()` replacement.

### Step 4: Check Imports

After making changes, verify that the NAMESPACE and roxygen2 imports are correct:

- If `.data` is used, ensure `@importFrom rlang .data` exists somewhere in the package (typically in the package-level documentation file or in the relevant function file). Alternatively, ensure rlang is in the Imports field of DESCRIPTION.
- Run `devtools::document()` to regenerate NAMESPACE after any import changes.

### Step 5: Verify

Run a full package check:

```r
devtools::document()
devtools::check()
```

Confirm:

1. Zero new WARNINGs, ERRORs, or NOTEs compared to before the changes.
2. No ggplot2 deprecation warnings remain. If you can construct a quick test, run a function that triggers plot generation and verify the warnings are gone.
3. All existing tests pass.

### Step 6: Document Changes

After completing the fixes, update `references/working/deprecation_audit.md` with a summary of all changes made (file, line, what was changed and why). This serves as a record for the maintainer to review.
