# Category-Based EDI Workflow Update

## Model Selection

The workflow resolves model IDs from `clif_model_registry.model_category` using
the standard category `epic_deterioration_index`, then filters `clif_scores`
before collecting eligible in-admission rows. Join keys are compared as exact
strings in Arrow; CSV keys are explicitly typed as strings. All matching versions are
included. Selection does not depend on an institution's model IDs, display
names, or `is_live` flag.

The registry must have populated, unique `model_id` values and populated
`model_category` values. Every score model ID must have a registry mapping.
Invalid tables produce explicit errors identifying the missing field or mapping.

## Configuration Migration

Use the standard CLIF scores and registry files and their join keys. Remove
the obsolete `score_model_id_value`, `edi_model_id`, and
`score_source_verified_edi_only` settings. The migration is documented in
`config/README.md`; the loader rejects obsolete selection values.

## New August Analyses

- **Figure 7:** full-admission maximum EDI; thresholds 0–100 by 1 by default;
  counts and percentages ever reaching each threshold.
- **Figure 8:** first crossing of the configured EDI threshold from admission;
  hourly counts using ceiling, cumulative counts/percentages, and N/mean/quartiles.
  Six-hour plot bins display the first 72 hours by default, with all crossers
retained in the denominator.

Scientific calculations follow the supplied August script. Paths, site labels,
thresholds, plotting settings, and disclosure limits use the configuration and
existing output conventions. Joint public count-boundary masks protect small
differences across the new threshold and cumulative outputs, including suppressed
Table 1 crossing/noncrossing strata and their denominator-revealing percentages.

## Timestamp Portability

Timezone-naive Arrow timestamps are read as wall clocks in the configured
timezone, rather than depending on the machine's timezone. Aware timestamps
preserve their instants. CSV date-only values normalize to local midnight.
The previous loader could shift naive clocks and misinterpret Date values;
baseline comparisons must account for these corrections, including DST effects.
Prediction horizons and trajectory windows also use elapsed durations rather
than calendar-hour periods, so configured hours retain their meaning across DST.

## Verification

`Rscript tests/run_tests.R` covers category mapping, multiple model versions,
invalid registries, threshold/hour boundaries, zero/late crossings, count
suppression, and host-independent timestamp handling. Synthetic parquet and CSV
end-to-end fixtures use unrelated model IDs, different site labels/timezones,
and custom score columns. Site-run baseline comparisons and generated results
remain in ignored local output directories.

Institution-specific reference scripts and previous local reports are preserved
in ignored `local_reference/`; the shared executable is `code/edi_analysis.R`.
