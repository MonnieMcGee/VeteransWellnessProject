# Dallas Veterans Well-Being Assessment

## Assessment of Dallas Veterans' Services:
### Integrated Assessment of Veteran Well-Being Using Qualitative, Spatial, and Mobility-Based Data Science Methods

This repository contains the complete reproducible analysis pipeline for the Dallas Veterans Well-Being Assessment project. The project integrates qualitative, natural language processing, structural topic modeling, and geospatial methods to understand barriers to accessing veterans' services in Dallas, Texas.

The repository includes the complete workflow used to construct the Reddit analysis corpus, develop and validate the Experienced System Fragmentation Index (ESFI), fit Structural Topic Models (STM), and generate all publication-ready tables and figures. :contentReference[oaicite:0]{index=0}

---

## Data Availability

This repository contains the complete analysis code and reproducible workflow used in the study.

The `RawData` directory is intentionally excluded from the public repository.

- Veterans Insights Forum (VIIF) data contain human-subject research materials collected under SMU IRB protocol #25-020 and are not publicly distributed.
- Reddit data were obtained from publicly available sources but are not redistributed through this repository.

The repository includes all code, derived analysis objects, and reproducibility scripts necessary to reproduce the analyses from the original data.
---

# Authors

- **Monnie McGee**, PhD  
  Department of Statistics and Data Science  
  Southern Methodist University

- **Jennifer Ebinger**, EdD  
  Office of Engaged Learning  
  Southern Methodist University

- **Jessie Zarazaga**, PhD  
  Department of Civil and Environmental Engineering  
  Southern Methodist University

---

# Repository Structure

```
Code/
RawData/
DerivedData/
Output/
Papers/
Presentations/
Reproducibility/
```

---

# Analysis Pipeline

Scripts should be executed sequentially.

```
00_project_setup.R
01_project_inventory.R
02_raw_input_manifest.R
03_build_analysis_corpus.R
04_develop_esfi_dictionary.R
05_validate_esfi_dictionary.R
06_prepare_stm_corpus.R
07_fit_reddit_stm.R
08_validate_esfi_with_stm.R
09_generate_publication_tables_figures.R
10_session_info.R
```

The pipeline progresses from raw Reddit and Veterans Insights Forum data through corpus construction, ESFI dictionary development and validation, Structural Topic Modeling, statistical analyses, and automatic generation of publication-ready tables and figures.

---

# Software Requirements

- R (≥ 4.5)
- quanteda
- stm
- tidyverse
- lubridate
- readr

Random seeds are set throughout the analysis to maximize reproducibility. :contentReference[oaicite:2]{index=2}

---

# Data Flow

```
Raw Data
    │
    ▼
Analysis Corpus
    │
    ├── ESFI Development
    │
    └── STM Preparation
            │
            ▼
      Structural Topic Model
            │
            ▼
 Validation and Figures
```

---

# Frozen Analysis Objects

Several intermediate objects are intentionally frozen and should never be modified once created. These include the finalized Reddit corpus, document-feature matrices, metadata, and the final ESFI dictionary. They serve as the foundation for all downstream analyses and ensure that every manuscript result can be reproduced exactly.

---

# Reproducibility

The repository was designed to support fully reproducible research.

- Scripts execute in numerical order.
- Each script reads only outputs produced by previous scripts.
- Upstream data objects are never modified.
- Publication tables and figures are generated automatically from frozen intermediate outputs.
- Session information, package versions, and project manifests are recorded to document the computational environment. 

---

# Citation

If you use this software, analysis pipeline, or derivative work, please cite:

> McGee M, Ebinger J, Zarazaga J. *Assessment of Dallas Veterans' Services: Integrated Assessment of Veteran Well-Being Using Qualitative, Spatial, and Mobility-Based Data Science Methods.* (Manuscript in preparation.)

Once the manuscript is published, this citation will be updated with the journal reference and DOI.

---

## Research Ethics

Human-subjects components of this project were reviewed by the Southern Methodist University Institutional Review Board (IRB #25-020) and determined to be exempt under 45 CFR 46.104(d)(2). Data sharing in this repository is consistent with the approved protocol.
---

# License

The source code in this repository is licensed under the GNU General Public License v3.0 (GPL-3.0). See the LICENSE file for details.

---

# Acknowledgments

This work was supported by the City of Dallas Veterans Affairs Commission under Contract **FHO-2025-0002075**.
