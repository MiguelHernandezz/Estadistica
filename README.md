<div align="center">

# 📊 Estadística · UAH

**Statistics (Estadística, 780004) · B.S. Computer Engineering · Universidad de Alcalá · 2024–2025**

![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=flat-square&logo=rstudioide&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-161B22?style=flat-square)
![Team](https://img.shields.io/badge/team-2_people-00C853?style=flat-square)

</div>

Two lab projects built on the same question: **did public transport usage in Spain go hand in hand with the spread of COVID-19?** The first one describes both datasets; the second one tests hypotheses on them.

> Both projects were done in a **team of 2**. My part in each one is listed below.

---

## 📁 Contents

| Folder | Project | Main deliverable |
|---|---|---|
| [`PL1-covid-transporte`](PL1-covid-transporte) | Descriptive statistics: COVID-19 evolution and public transport usage in Spain | [Report (PDF, 41 pp.)](PL1-covid-transporte/Informe_Estadistico_Descriptivo.pdf) |
| [`PL2-inferencia-covid-transporte`](PL2-inferencia-covid-transporte) | Inferential statistics on the same data: normality tests, confidence intervals, hypothesis testing | [Report (PDF, 44 pp.)](PL2-inferencia-covid-transporte/Informe_Estadistico_Inferencial.pdf) · [Slides](PL2-inferencia-covid-transporte/Presentacion_Informe_Inferencial.pptx) |

---

## PL1 · Descriptive report

**Data**
- COVID-19 cases, hospitalisations, ICU admissions and deaths by sex, age group and province of residence, Jan 2020 – Apr 2022 (`casos_hosp_uci_def_sexo_edad_provres.csv`, ~38 MB; source: ISCIII).
- Public transport passengers in Spain, 1996–2024 (INE).

**What's in it**
- Central tendency, dispersion and shape measures (mean, median, quartiles, variance, standard deviation, Pearson skewness), computed with a reusable R function.
- Frequency tables by interval, histograms, box plots (with outliers) and bar charts in base R and `ggplot2`.
- Comparative analysis between both datasets and conclusions.

**My part:** the full **public transport report** (`Informe-Transporte/`), plus the bibliography and appendix. Shared with my teammate: comparative analysis and conclusions.

## PL2 · Inferential report

**What's in it**
- Normality tests: Shapiro-Wilk, Anderson-Darling and Jarque-Bera (`nortest`, `tseries`).
- Homogeneity of variances with Levene's test (`car`).
- Confidence intervals for the mean: z-test on the full sample and t-test on a 27-observation sample (`BSDA`).
- One-sided **proportion tests** (`prop.test`): men vs women, Madrid vs Soria, age 10–19 vs 30–39, winter vs summer.
- Main finding: COVID-19 cases and public transport usage both peak in high-density areas and in winter, and transport usage differs significantly between 2020 and 2022.

**My part:** the **public transport case (case Y)**, the research and selection of sources (INE, ISCIII), the cover and the bibliography. Shared with my teammate: comparative analysis and conclusions.

---

## ▶️ Run it

Requires [R](https://cran.r-project.org/) ≥ 4.x. Every script is self-contained: it installs any missing CRAN package and loads its own data from the CSV files in the same folder.

**RStudio:** open the `.Rproj` file of a folder and *Source* any script.

**Command line:**

```bash
cd PL1-covid-transporte/Informe-Covid
Rscript Calculos_Covid.R          # descriptive stats + ggplot2 charts (Proyecto.R loads the data)
Rscript Graficas_covid.R          # frequency tables, pie charts, histograms, box plots

cd ../Informe-Transporte
Rscript "Grafica transporte publico general.R"

cd ../../PL2-inferencia-covid-transporte/Informe-Covid
Rscript PL2.R                     # normality tests, Levene, confidence intervals
Rscript ContrasteHipotesis.R      # one-sided proportion tests (runs PL2.R first)
```

Outside RStudio, charts are written to `Rplots.pdf` in each folder. `PL1-covid-transporte` loads a ~38 MB CSV with 1.3M rows, so the first run takes a little longer.

---

<div align="center">

Miguel Hernández López · [GitHub](https://github.com/MiguelHernandezz) · [LinkedIn](https://www.linkedin.com/in/miguel-hernandez-lopez/)

</div>
