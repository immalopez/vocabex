# Running statistical-analysis

This directory contains the analyses underlying the manuscript.
Each execution creates a self-contained run with its reports, tables, figures, 
fitted models, source snapshots, and software-environment record.

## Recreate the R environment

The analysis package library is locked with `renv`. It was captured with R 4.5.1;
the separately installed system components are recorded in
`system-requirements.yml`.

From this directory, install `renv` if necessary and restore the package library:

```sh
Rscript -e 'install.packages("renv")'
Rscript -e 'renv::restore(prompt = FALSE)'
```

The lockfile pins R package versions and non-CRAN sources, including the exact
GitHub revision of `ggsankey`. CmdStan itself is not installed by `renv`. This
project was captured with CmdStan 2.39.0 and `cmdstanr` 0.8.0. If CmdStan is not
already available, install the recorded version after restoring the R packages:

```sh
Rscript -e 'cmdstanr::install_cmdstan(version = "2.39.0")'
```

Check the restored packages, R version, Pandoc, CmdStan, and build tools without
fitting any models:

```sh
Rscript check_environment.R
```

The original environment was tested on Apple Silicon macOS. Other platforms may
use a different compiler, but they should use the package, R, CmdStan, and Pandoc
versions recorded in `renv.lock` and `system-requirements.yml`. 

## Overview

The usual workflow uses two commands: 
1. Create a complete analysis run generating fresh outputs if required.
2. Select/mark that run as the source for the manuscript.

Note: If a run stops before completion, its working directory remains under `runs/.incomplete/` 
with the recorded error. Completed runs appear directly under `runs/`.

From the `statistical_analyses/` directory run: 

```sh
Rscript run_analysis.R
```

When the analysis finishes, the command prints the run ID and directory, for example:

```text
Completed run 20260912T104500Z_a1b2c3d4
/path/to/statistical_analyses/runs/20260912T104500Z_a1b2c3d4
```

It then prints a step-by-step timing table and the total runtime in minutes, and
plays a completion sound in case you're not looking at the console. 

Generated results under that run directory include:

- `reports/` for the rendered analysis and data-exploration reports;
- `tables/` for exported result tables; and
- `figures/` for generated figures; and
- `tables/analysis_timings.csv` for per-step and total runtimes, including cache status;
- `logs/analysis.log` for timestamped run, cache, warning, and error events;
- `logs/*-render.log` for the live output from each rendered report; and
- `diagnostics/*.rds` for cached results and expensive computations.

Long-running model fits and diagnostics use content-addressed caching. This
includes the RQ1 and RQ3 mixed models, RQ2 Bayesian fits, DHARMa simulations,
PSIS-LOO calculations, prior-sensitivity fits, and RQ1 influence checks. A run
reuses the newest validated completed-run result only when its data, formula,
priors, seed, fitting settings, cache version, and relevant package versions
match. Otherwise, the step is recalculated. Reused results are copied and
re-registered inside the new run; the timing report, logs, and metadata identify
the source run.

Exact LOO artifacts are content-addressed and reused from a completed run when
the fitted model, moment-matched LOO object, settings, and relevant package
versions match. On a cache miss, exact refitting can take substantially longer.
For a fast exploratory run, pass `--skip-exact-loo`; moment-matched PSIS-LOO and
the high-k inspection are still produced, but the tables explicitly record zero
exact refits and retain the unresolved high-k count.

After deciding that the run should supply the manuscript results, 
select it using the printed run ID by executing:

```sh
Rscript select_run.R 20260912T104500Z_a1b2c3d4
```

The selection is recorded in `manuscript_run.yml`. 
Selecting a run identifies the approved analysis; it does not edit or rerender the manuscript.

Selecting another completed run updates that pointer and creates a compact record under `run_records/RUN_ID/`. 
Experimental runs can therefore remain available without changing the manuscript selection.

## Running the analysis

A normal run prepares the data, renders `analysis.Rmd` and `data_exploration.Rmd`, 
performs the RQ2 prior-sensitivity analysis, and collects the resulting artifacts in `runs/RUN_ID/`.

The optional arguments are:

```sh
Rscript run_analysis.R --run-id=descriptive_unique_id
Rscript run_analysis.R --skip-preparation
Rscript run_analysis.R --skip-sensitivity
Rscript run_analysis.R --include-agreement
```

Use `--run-id` when a descriptive identifier will make an experimental run easier to recognize. 
Run IDs must be unique.

`--skip-preparation` starts from the existing `data_for_analysis.csv` rather than rerunning `data_preparation.R`. 
`--skip-sensitivity` omits the RQ2 prior-sensitivity stage. 
Use the command without skip options for a complete manuscript run.

## LIL inter-rater agreement

Inter-rater agreement is deliberately optional in `run_analysis.R` as it has no dependency on the manuscript run.
Initially, agreement was implemented in Python but later migrated to R/RStudio to keep analyses together.

To refresh agreement without creating or altering a model run, use:

```sh
Rscript il_agreement.R
```

This writes `il_agreement_by_column.csv`, `il_agreement_composites.csv`, and
`il_agreement_disagreements.csv` under `agreement_outputs/`. The first table
contains exact, positive, and negative agreement, Cohen's kappa, Gwet's AC1, and
bootstrap confidence intervals. The composite table contains absolute-agreement
single-measure ICCs and Bland-Altman summaries for independently calculated LIL totals.

To include the same calculation in a new reproducible run, use:

```sh
Rscript run_analysis.R --include-agreement
```

The default inputs are `annotated_exercise_data.csv` and `lil_rater_2.csv`. A
different second-rater path inside `statistical_analyses/` can be supplied with
`--agreement-rater-2=relative/path/to/ratings.csv`. Agreement outputs are then
stored in that new run's `tables/` directory and registered without claiming a
dependency on the fitted models.

## Cohen and Fleiss categorical agreement

The migrated Cohen and Fleiss calculations use the original rating files under
`../code/resources/` and write all result tables under `agreement_outputs/`.
Comma-separated combinations in the source ratings remain complete categorical
labels, matching the former Python calculations.

Run the analyses from the `statistical_analyses/` directory:

```sh
Rscript categorical_agreement.R
```

The runner produces `cohen_kappa_summary.csv`, one confusion matrix per Cohen
analysis, `fleiss_kappa_summary.csv`, and the Fleiss per-item category-count
matrix. 

## TIL recall agreement

The default input is `til_rater_2.csv`.

```sh
Rscript til_agreement.R
```

The analysis writes `til_agreement_by_column.csv` and
`til_agreement_disagreements.csv` under `agreement_outputs/`. The summary reports
exact, positive, and negative agreement, Cohen's kappa, Gwet's AC1, prevalence,
and 95% confidence intervals. 

## Validation

For a quick check of the run-management code, execute:

```sh
Rscript tests/test_run_registry.R
Rscript tests/test_run_cache.R
```

These tests exercise run, fitted-object, cache, corruption, and timing validation
without refitting the full statistical models.
