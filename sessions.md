---
layout: default
---

## Course sessions and topics

### Textbooks

| | Reference |
|:---|:---|
| **Urdinez & Cruz** | Urdinez & Cruz (2020). *R for Political Data Science*. CRC Press. |
| **Gelman et al.** | Gelman, Hill & Vehtari (2021). *Regression and Other Stories*. Cambridge UP. |
| **Imai** | Imai (2017). *Quantitative Social Science*. Princeton UP. |
| **BdM & Fowler** | Bueno de Mesquita & Fowler (2021). *Thinking Clearly with Data*. Princeton UP. |

---

### Session 1: Introduction (Feb 5)

Course overview, structure, and evaluation. The research process: theory, data generating processes, and data. Sources of uncertainty and the logic of statistical inference. Introduction to version control with Git and GitHub. PS1: setting up a GitHub repository.

| Source | Chapter |
|:---|:---|
| Gelman et al. | Ch. 1 |
| Imai | Ch. 1.3 |
| BdM & Fowler | Ch. 1 |

---

### Session 2: Applied Regression (Feb 12)

Review of the regression framework: conditional expectations and OLS. Bivariate and multiple regression. Interpreting coefficients with multiple predictors: "holding constant." Interaction effects: continuous-by-continuous and continuous-by-categorical. Categorical predictors and dummy variables. Presenting results with `modelsummary`.

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 1–5 |
| Gelman et al. | Ch. 6–7, 10 |
| BdM & Fowler | Ch. 5, 10 |

---

### Session 3: Binary Outcomes (Feb 19)

When the outcome is binary: limitations of OLS. The linear probability model (LPM): advantages and problems. Logistic regression: the logit model and maximum likelihood. Interpreting logit output: odds ratios, predicted probabilities, and marginal effects. Visualizing effects with `marginaleffects`. Model fit and classification basics.

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 8 |
| Gelman et al. | Ch. 13–14 |

---

### Session 4: Model Interpretation and Diagnostics (Feb 26)

Interpreting model output: moving beyond coefficient tables. Magnitude of effects: predicted values, marginal effects, and first differences. Publication-quality tables and coefficient plots with `modelsummary`. Simulation-based uncertainty. Brief diagnostics: heteroskedasticity, robust standard errors, log transformations.

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 5 (§5.6) |
| Gelman et al. | Ch. 11–12 |

---

### Session 5: Best Practices in Computing (Mar 5)

Working with plain text files. Organizing coding projects: folder structure, file naming, separating tasks. Writing better R code: functions, checks and warnings, automation. Integrating R output with papers (LaTeX, Quarto). Code editors and the command line. Version control with Git: deeper dive.

| Source | Chapter |
|:---|:---|
| Healy | *The Plain Person's Guide to Plain Text Social Science* |
| MIT | *The Missing Semester of Your CS Education* (online) |

---

### Session 6: Panel Data I (Mar 12)

What is panel data: structure, notation, and formats. Describing panel datasets: overall, between, and within variation. Pooled OLS and the problem of unobserved heterogeneity. Introduction to fixed effects: logic and the within transformation.

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 7 (§7.1–7.3) |
| Gelman et al. | Ch. 21.4 |
| BdM & Fowler | Ch. 13 |

---

### Session 7: Panel Data II (Mar 19)

Fixed effects estimation in detail. Random effects and the Hausman test. Two-way fixed effects (unit + time). Clustered standard errors. Introduction to staggered difference-in-differences: problems with TWFE under staggered adoption, and modern estimators (Callaway & Sant'Anna, Sun & Abraham).

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 7 (§7.4–7.6) |
| Gelman et al. | Ch. 10.5 |
| BdM & Fowler | Ch. 13 |
| Cunningham | *Causal Inference: The Mixtape*, Ch. 9 (online) |

---

### Session 8: Spatial Data I (Mar 26)

Types of spatial data: points, lines, polygons. Coordinate reference systems. Working with the `sf` package: loading, transforming, and joining spatial data. Creating choropleth and point maps with `ggplot2` and `tmap`.

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 16 (§16.1–16.4) |
| Imai | Ch. 7.2 |

---

### Session 9: Spatial Data II (Apr 9)

Spatial weights matrices: contiguity, distance, k-nearest neighbors. Spatial autocorrelation: global and local Moran's I. Spatial regression models: spatial lag and spatial error. Model selection with LM tests. Applications in political science.

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 16 (§16.5) |
| Imai | Ch. 7.2.5 |

---

### Session 10: Other Outcomes (Apr 16)

Count data: Poisson regression and overdispersion. Negative binomial regression. Ordinal outcomes: ordered logit/probit and the proportional odds assumption. Duration data: survival functions, hazard rates, Kaplan-Meier curves, and Cox proportional hazards.

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 9 |
| Gelman et al. | Ch. 15 |

---

### Session 11: Project Presentations (Apr 23)

Student presentations of final essay proposals (10–15 min each). Peer feedback and discussion. Instructor guidance on data and analysis strategies.

---

### Session 12: Exam + Review (Apr 30)

In-class exam (first half). Course synthesis and review (second half): choosing the right method, common pitfalls, and a preview of advanced topics (multilevel models, causal inference, machine learning).

| Source | Chapter |
|:---|:---|
| Urdinez & Cruz | Ch. 10 |
| Gelman et al. | Ch. 18–21 |
| BdM & Fowler | Ch. 11–14 |
