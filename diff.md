# Rush EDI-Only Correction

## Summary

The original Rush run was not limited to Epic Deterioration Index (EDI). Its
configured score file contains both EDI and Epic Sepsis scores, but the active
configuration did not specify a model-ID field or value. The corrected local
Rush configuration filters `ACUITY_SYSTEM_ID = 30434503`, the ID registered as
`RUSH IP DETERIORATION INDEX` / `epic_deterioration_index`.

The rerun reduces the score-qualified cohort from 123,611 to 99,273 encounters
and improves composite hospital-level AUC from 0.631 to 0.697. The original
mixed-model metrics must not be reported as EDI performance.

## Model Evidence

The legacy workflow explicitly selects EDI before joining scores to the cohort:

```r
filter(model_id == "30434503") # model_name == "IP Deterioration Index"
```

Source: `EDI old code/EDI - All Patients 08_2026.R`, line 176.

The Rush score source contains these model IDs:

| Model ID | Score rows | Model registry mapping |
|---:|---:|---|
| 30434500 | 135,664,430 | RUSH Early Detection of Sepsis / `epic_sepsis_model` |
| 30434503 | 114,387,174 | RUSH IP Deterioration Index / `epic_deterioration_index` |

The source filename, `acuity_score_DETERIORATION_INDEX.parquet`, is therefore
not evidence that the file is EDI-only.

## Configuration And Code Changes

The ignored local `config/config.json` now contains:

```json
"score_model_id_column": "ACUITY_SYSTEM_ID",
"score_model_id_value": 30434503,
"edi_model_id": null
```

`utils/config.R` now rejects a partially configured filter. A model-ID column
and model-ID value must be supplied together. It also rejects an unfiltered
source unless `score_source_verified_edi_only` is explicitly set to `true`.
This prevents a mixed-model source from silently resulting in an unfiltered
run.

`code/edi_analysis.R` now:

1. Logs the active model filter before score collection.
2. Includes the model filter in the no-valid-scores error message.
3. Publishes the configured filter in the sanitized run manifest.

The filter remains optional for a source that has been independently verified
to contain only EDI, but that assertion must be explicit in configuration.
`README.md` and `config/README.md` now state that both fields must be
configured for a mixed-model source.

## Rush Results: Mixed Models Versus EDI Only

The baseline is the prior unfiltered Rush run completed 2026-08-20. The
corrected EDI-only run completed 2026-09-14. Both use the same cohort and
outcome settings; the score-model filter is the intended change.

### Cohort

| Measure | Prior mixed-model run | EDI-only run | Change |
|---|---:|---:|---:|
| Location-eligible hospitalizations | 124,362 | 124,362 | 0 |
| At least one in-admission score | 123,611 | 99,273 | -24,338 (-19.7%) |
| Maximum score >=40 | 21,858 | 21,839 | -19 (-0.1%) |
| Maximum score <40 | 101,753 | 77,434 | -24,319 (-23.9%) |
| ICU outcome | 10,317 | 8,355 | -1,962 |
| Vasopressor outcome | 4,349 | 3,670 | -679 |
| IMV outcome | 2,616 | 2,173 | -443 |
| In-hospital mortality | 3,338 | 2,723 | -615 |

Nearly all threshold-40 alerts were already associated with EDI score rows,
but the unfiltered analysis included 24,319 additional below-threshold
encounters. This altered the denominator, hospitalization-level maximum score,
and all performance estimates.

### Composite Performance At Threshold 40

| Horizon | Metric | Prior mixed-model run | EDI-only run | Change |
|---|---|---:|---:|---:|
| 4 hours | AUC | 0.577 | 0.673 | +0.096 |
| 4 hours | Sensitivity | 0.216 | 0.230 | +0.014 |
| 12 hours | AUC | 0.565 | 0.651 | +0.086 |
| 12 hours | Sensitivity | 0.222 | 0.238 | +0.015 |
| Hospital level | AUC | 0.631 | 0.697 | +0.066 |
| Hospital level | Sensitivity | 0.362 | 0.453 | +0.091 |

### Hospital-Level AUC At Threshold 40

| Outcome | Prior mixed-model run | EDI-only run | Change |
|---|---:|---:|---:|
| ICU | 0.631 | 0.687 | +0.056 |
| Vasopressors | 0.711 | 0.781 | +0.070 |
| IMV | 0.698 | 0.761 | +0.063 |
| In-hospital mortality | 0.781 | 0.896 | +0.115 |
| Composite | 0.631 | 0.697 | +0.066 |

The corrected public output is in the ignored local directory
`output/rush_validation/final_no_phi/`. Its `rush_run_manifest.csv` records
`score_model_filter=ACUITY_SYSTEM_ID=30434503`.

## Legacy Versus Current Cohort Logic

The corrected model filter aligns the current workflow with the old script's
EDI-only requirement. Other cohort and outcome differences remain:

| Area | Legacy Emory script | Current Rush workflow |
|---|---|---|
| Age and admission validity | No explicit age/date validation | Requires age >=18, complete IDs/times, and non-negative admission duration |
| ADT intervals | Raw ADT records | Clips intervals to the admission and removes invalid intervals |
| Location categories | Exact case-sensitive comparisons | Trims and matches case-insensitively |
| Score model | Hard-coded `30434503` | Configured `ACUITY_SYSTEM_ID=30434503` |
| Demographics | Inner join removes encounters without matching patient data | Left join retains score-eligible encounters with missing demographics |
| IMV and vasopressors | Earliest source event without explicit admission-window restriction | Events must occur during admission |

The legacy comments state that observation is allowed as an initial location,
but its executable code allows only `ed` and `ward`. The active Rush
configuration also permits only `ed` and `ward`.

The legacy report is Emory-only and the corrected analysis is Rush-only. Their
results remain cross-site, non-contemporaneous comparisons and cannot be used
as a controlled before/after assessment.

## Verification

1. Configuration loading confirmed `ACUITY_SYSTEM_ID`, `30434503`, and
   `score_source_verified_edi_only=false`.
2. Focused configuration checks confirm that incomplete model filters and an
   unverified unfiltered score source are rejected.
3. `Rscript code/edi_analysis.R` completed successfully on 2026-09-14.
4. The protected EDI-only score checkpoint contains 49,160,418 score rows for
   99,273 unique hospitalizations, matching the regenerated flowsheet.
5. The public performance file contains 105 published metric rows.
6. The public run manifest records `ACUITY_SYSTEM_ID=30434503` as the score
   model filter.

`config/config.json` and generated outputs remain intentionally untracked:
the local configuration contains site filesystem details, and the output
directory includes protected intermediate files.
