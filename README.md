# Early Engagement, Learner Background and Course Completion in Distance Higher Education

Reproducible analysis code for the study:

> Raman, H. (2026). *Early engagement, learner background and course completion
> in distance higher education: a multilevel and landmark survival analysis of
> the Open University Learning Analytics Dataset.*

This repository contains the R script that reproduces every statistic, table and
figure reported in the paper using the publicly available **Open University
Learning Analytics Dataset (OULAD)**.

## Summary

The study examines learner, engagement and course-level predictors of successful
course completion, and the timing of withdrawal, across 22 presentations of seven
modules in OULAD. Using a **day-14 landmark design** (so that all predictors are
observed before the outcome period begins), it fits:

- a **multilevel logistic regression** of completion, with crossed random effects
  for module-presentation and region;
- **Kaplan–Meier** estimates and a **stratified Cox proportional hazards model**
  of withdrawal;
- a **random forest vs logistic regression** predictive comparison, with SHAP
  importance.

## Repository contents

| Path | Description |
|------|-------------|
| `R/oulad_analysis.R` | Full analysis pipeline: cleaning, descriptives, multilevel models, survival analysis, prediction. |
| `figures/` | The five figures from the paper (300 dpi). |
| `LICENSE` | MIT licence for the code. |
| `.gitignore` | Excludes raw data and generated outputs from version control. |

## Data

The OULAD data are **not** included in this repository. Download them from the
Open University and place the CSV files in a folder named `OULAD/`:

- Source: https://analyse.kmi.open.ac.uk/open_dataset
- Dataset DOI: https://doi.org/10.1038/sdata.2017.171
- Licence: Creative Commons Attribution 4.0 (CC BY 4.0)

Required files: `studentInfo.csv`, `studentRegistration.csv`, `courses.csv`,
`studentVle.csv`.

Expected folder layout:

```
.
├── OULAD/
│   ├── studentInfo.csv
│   ├── studentRegistration.csv
│   ├── courses.csv
│   └── studentVle.csv
└── R/
    └── oulad_analysis.R
```

## Requirements

- R 4.3 or later
- R packages: `dplyr`, `tidyr`, `readr`, `ggplot2`, `lme4`, `car`, `survival`,
  `ranger`, `pROC`, `fastshap`, `scales`

The script installs any missing packages automatically on first run.

## How to run

From the repository root:

```r
source("R/oulad_analysis.R")
```

All tables (as CSV) and figures (as PNG) are written to an `oulad_outputs/`
folder, along with `results_summary.txt`, which lists every value reported in the
paper, and `session_info.txt`, which records the exact package versions used. A
fixed random seed makes the results fully reproducible.

## Citing

If you use this code, please cite the paper (above) and the dataset:

> Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open University Learning
> Analytics dataset. *Scientific Data, 4*, 170171.
> https://doi.org/10.1038/sdata.2017.171

## Author

**Harshit Raman** — EdCreate Foundation, Greater Noida, India
GitHub: [@harshit-stas](https://github.com/harshit-stas)

## License

The code in this repository is released under the MIT License (see `LICENSE`).
The OULAD dataset is licensed separately under CC BY 4.0 by its authors.
