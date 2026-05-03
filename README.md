# Heart Disease Risk Explorer

A Shiny app for interactive exploration of the UCI Heart Disease dataset.


## Dataset

- **Source:** UCI Machine Learning Repository (https://archive-beta.ics.uci.edu/dataset/45/heart+disease)
- **Study:** Detrano et al. (1989), American Journal of Cardiology
- **Patients:** 303 from Cleveland Clinic Foundation
- **Variables:** 13 clinical features
- **Target:** Heart disease diagnosis (binary: Disease / No Disease)

##  License

**Dataset:** Creative Commons Attribution 4.0 International (CC BY 4.0)

This license allows to:
- Share (copy and redistribute the material in any format)
- Adapt (remix, transform, and build upon the material for any purpose)
- Requirement (must give appropriate credit to the UCI Machine Learning Repository and the original authors)

**Code:** MIT License

## 🚀 How to Run

```r
install.packages(c("shiny", "ggplot2", "plotly", "dplyr", "tidyr"))
shiny::runGitHub("DASC3240_FinalProject", "1ryyann")
