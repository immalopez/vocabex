# Reproducible statistical-analysis runs

This directory contains the analyses underlying the manuscript. Each execution creates a self-contained run with its reports, tables, figures, fitted models, source snapshots, and software-environment record.

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
versions recorded in `renv.lock` and `system-requirements.yml`. The optional
`beepr` package is deliberately not locked; it is used only for a completion
sound and is not required for the analysis.

## Overview

The usual workflow uses two commands: create a complete analysis run, review its outputs, and then select that run as the source for the manuscript.

Choose the command for the interface you are using. Run only one of these alternatives.

**RStudio Terminal** (the tab showing a shell prompt such as `$` or `%`):

```sh
Rscript run_analysis.R
```

**R Console** (the pane showing the `>` prompt):

```r
system2("Rscript", "run_analysis.R")
```

Run the command from the `statistical_analyses/` directory. 

When the analysis finishes, the command prints the run ID and directory, for example:

```text
Completed run 20260912T104500Z_a1b2c3d4
/path/to/statistical_analyses/runs/20260912T104500Z_a1b2c3d4
```

It then prints a step-by-step timing table and the total runtime in minutes, and
plays a completion sound. The same summary and sound are produced when a run
fails. On macOS the runner uses a native system sound because RStudio commonly
ignores the console's ASCII bell; other platforms use `beepr` when installed and
fall back to the platform or console alert.

Review the generated results under that run directory, especially:

- `reports/` for the rendered analysis and data-exploration reports;
- `tables/` for exported result tables; and
- `figures/` for generated figures; and
- `tables/analysis_timings.csv` for per-step and total runtimes, including cache status;
- `logs/analysis.log` for timestamped run, cache, warning, and error events;
- `logs/*-render.log` for the live output from each rendered report; and
- `diagnostics/rq1_influence_checks.rds` for the full, computationally
  expensive RQ1 leave-one-participant-out and sampled leave-one-item-out checks.

Long-running model fits and diagnostics use content-addressed caching. This
includes the RQ1 and RQ3 mixed models, RQ2 Bayesian fits, DHARMa simulations,
PSIS-LOO calculations, prior-sensitivity fits, and RQ1 influence checks. A run
reuses the newest validated completed-run result only when its data, formula,
priors, seed, fitting settings, cache version, and relevant package versions
match. Otherwise, the step is recalculated. Reused results are copied and
re-registered inside the new run; the timing report, logs, and metadata identify
the source run.

The RQ2 prior-sensitivity analysis changes only the prior specification. Both
models use 4 chains, 2,000 iterations per chain (1,000 warmup), 6 threads per
chain, a diagonal Euclidean metric, `adapt_delta = 0.99`, and
`max_treedepth = 12`. Both receive the same sampler and calibration diagnostics.
RQ2 LOO first uses moment-matched PSIS. Every observation that remains above
Pareto k = .7 is inspected and replaced by an exact leave-one-out refit; the
reported LOOIC and `p_loo` use that corrected object. This rule is applied to
both prior specifications. Exact settings and seeds are exported to
`tables/rq2_prior_sensitivity_settings.csv`; the high-k inspection, diagnostic
summaries, and calibration bins are exported alongside it.

Exact LOO artifacts are content-addressed and reused from a completed run when
the fitted model, moment-matched LOO object, settings, and relevant package
versions match. On a cache miss, exact refitting can take substantially longer.
For a fast exploratory run, pass `--skip-exact-loo`; moment-matched PSIS-LOO and
the high-k inspection are still produced, but the tables explicitly record zero
exact refits and retain the unresolved high-k count. Do not select such a run as
the manuscript run when the exact-refit LOO results are required.

After deciding that the run should supply the manuscript results, select it using the printed run ID. In the **RStudio Terminal**, run:

```sh
Rscript select_run.R 20260912T104500Z_a1b2c3d4
```

In the **R Console**, run instead:

```r
system2("Rscript", c("select_run.R", "20260912T104500Z_a1b2c3d4"))
```

The selection is recorded in `manuscript_run.yml`. Selecting a run identifies the approved analysis; it does not edit or rerender the manuscript.

## Running the analysis

A normal run prepares the data, renders `analysis.Rmd` and `data_exploration.Rmd`, performs the RQ2 prior-sensitivity analysis, and collects the resulting artifacts in `runs/RUN_ID/`.

The optional arguments are:

```sh
Rscript run_analysis.R --run-id=descriptive_unique_id
Rscript run_analysis.R --skip-preparation
Rscript run_analysis.R --skip-sensitivity
Rscript run_analysis.R --include-agreement
```

Use `--run-id` when a descriptive identifier will make an experimental run easier to recognize. Run IDs must be unique.

`--skip-preparation` starts from the existing `data_for_analysis.csv` rather than rerunning `data_preparation.R`. `--skip-sensitivity` omits the RQ2 prior-sensitivity stage. Use the command without skip options for a complete manuscript run.

## LIL inter-rater agreement

Inter-rater agreement is deliberately optional in `run_analysis.R`. This keeps a
standalone agreement refresh from changing or replacing the selected manuscript
run, while allowing a future complete run to snapshot the ratings and include the
agreement tables. Running any analysis does not change `manuscript_run.yml`;
selection remains an explicit separate action.

The authoritative first rater is `annotated_exercise_data.csv`. Its LIL components
are reconstructed from the learner-response fields with the same operational
rules as `data_preparation.R`, except that missing source responses remain
missing for the agreement analysis. In particular, missing annotations are never
changed to zero before agreement is calculated.

The second rater should be stored separately in `lil_rater_2.csv`. It must contain the
item identifiers `participant_ID`, `text_file`, and `error`, followed by the same
six unprefixed LIL columns: `goal_aligned`, `relevant_goals`,
`interesting_content`, `ownership`, `understanding`, and `positive_emotion`.
There is one row per learner exercise.
The adapter aligns the files using all three identifiers and reports unmatched or
missing pairs rather than silently discarding them.

LIL is learner-experienced: each rater evaluates the learner's responses rather
than inferring LIL from exercise material. The output tables retain `unit` and
`evidence_source` columns to make that evidence basis explicit.

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
dependency on the fitted models. The currently selected run is unaffected unless
the new run is later passed explicitly to `select_run.R`.

## Cohen and Fleiss categorical agreement

The migrated Cohen and Fleiss calculations use the original rating files under
`../code/resources/` and write all result tables under `agreement_outputs/`.
Comma-separated combinations in the source ratings remain complete categorical
labels, matching the former Python calculations.

Run the analyses from the `statistical_analyses/` directory. In the **RStudio
Terminal**, use:

```sh
Rscript categorical_agreement.R
```

In the **R Console**, use:

```r
source("categorical_agreement.R")
```

The runner produces `cohen_kappa_summary.csv`, one confusion matrix per Cohen
analysis, `fleiss_kappa_summary.csv`, and the Fleiss per-item category-count
matrix. The Fleiss uncertainty estimates use 2,000 item-level bootstrap samples
and seed 42 by default. These settings and the input/output directories can be
changed from the terminal; run `Rscript categorical_agreement.R --help` for the
available options.

## TIL recall agreement

The `recall` annotation belongs to the task-induced involvement load (TIL)
retrieval dimension. To calculate its agreement, copy
`til_rater_2_template.csv` to `til_rater_2.csv` and add the second rater's binary
`recall` judgments. Keep one row per exercise and retain `text_file` exactly as it
appears in `annotated_exercise_data.csv`. Because `text_file` is unique in these
data, the runner uses it to recover `participant_ID` and `error` from the
authoritative first-rater file. Extra columns in the second-rater CSV are allowed
and ignored.

From the **RStudio Terminal**, run:

```sh
Rscript til_agreement.R
```

From the **R Console**, run:

```r
source("til_agreement.R")
```

The analysis writes `til_agreement_by_column.csv` and
`til_agreement_disagreements.csv` under `agreement_outputs/`. The summary reports
exact, positive, and negative agreement, Cohen's kappa, Gwet's AC1, prevalence,
and 95% confidence intervals. The intervals use a participant-level cluster
bootstrap (2,000 samples with seed 42 by default) to respect the repeated
exercises within learners.

If a run stops before completion, its working directory remains under `runs/.incomplete/` with the recorded error. Completed runs appear directly under `runs/`.

## Using a completed run

A completed run has the following structure:

```text
runs/RUN_ID/
├── manifest.json
├── inputs/         input snapshots and generated analysis datasets
├── source/         analysis code used for the run
├── fits/           fitted-model objects and supporting objects
├── models/         model descriptions and fitting settings
├── diagnostics/    saved diagnostic results
├── tables/         exported result tables
├── figures/        figures generated during rendering
├── reports/        rendered HTML reports
├── logs/           execution events and report-rendering output
├── artifacts/      records linking outputs to the run and models
└── environment/    package versions and R session information
```

Use reports, tables, figures, and fitted objects from the same run. The run manifest records the inputs, source version, command, Git state, and output checksums associated with those results. Paths stored in manifests are relative to `statistical_analyses/` or to the run directory, so run records remain portable and do not expose machine-specific locations.

To see which run currently supplies the manuscript, open:

```sh
cat manuscript_run.yml
```

Selecting another completed run updates that pointer and creates a compact record under `run_records/RUN_ID/`. Experimental runs can therefore remain available without changing the manuscript selection.

## Validation

`run_analysis.R` validates a run before marking it complete. This includes the recorded inputs, source snapshots, model files, diagnostics, reports, tables, figures, and software environment.

For a quick check of the run-management code, execute:

```sh
Rscript tests/test_run_registry.R
Rscript tests/test_run_cache.R
```

These tests exercise run, fitted-object, cache, corruption, and timing validation
without refitting the full statistical models.

## Implementation note

`R/run_registry.R` provides the shared run-recording and validation functions used by the runner and analysis scripts. Most users only need to call `run_analysis.R` and `select_run.R`; `run_registry.R` is useful when maintaining or extending the run system.
