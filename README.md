# Generative AI & Education Dashboard

## Project Overview
This project explores the impact and presence of **Generative Artificial Intelligence (GenAI)** in the field of **Education**.  
It focuses on identifying the main players, domains of application, educational levels involved, and potential implications of GenAI adoption in teaching and learning processes.

---


## Research Questions
The dashboard was designed to answer the following key questions:

1. Who are the main players in the field of generative AI for education?  
2. In which domains (e.g., medicine, physics, social sciences) is GenAI most commonly used?  
3. Which educational levels (primary, secondary, higher education) show the highest adoption of GenAI?  
4. How might teaching and learning change—positively or negatively—with the introduction of GenAI?  
5. Which GenAI technologies are most widely used in academic contexts?

---

## Workflow
The development of the dashboard followed these main stages:

1. **Gather** – Data collection from academic and social sources.  
2. **Analyse** – Data cleaning, text processing, and exploratory analysis.  
3. **Interpret** – Extraction of insights and topic modeling.  
4. **Plan** – Dashboard layout and functionality design.  
5. **Shiny App Building** – Implementation of the interactive application.

---

## Data Sources
Three main sources were used to collect information:

- **Scopus**  
  - Academic papers retrieved using the following query:  
    ```
    TITLE-ABS-KEY(("education" OR "teaching" OR "teach" OR "school" OR "student")
    AND ("generative AI" OR "machine learning" OR "artificial intelligence"))
    AND PUBYEAR > 2019 AND PUBYEAR < 2026
    ```
  - Approx. 20,000 papers and articles.

- **Lens.org**  
  - Patent data collected using abstract-level search:  
    ```
    (education OR teach*) AND ("generative AI" OR "generative model" OR "artificial intelligence" OR AI)
    ```
  - Approx. 848 patents.

- **Twitter**  
  - Social data including around 61,000 tweets related to education and generative AI.

---

## Technical Implementation

### Environment
The dashboard was implemented in **R** using the **Shiny framework**.

### Main Libraries
- `shiny`  
- `tidyverse`  
- `tidytext`  
- `ggplot2`  
- `wordcloud`  
- `topicmodels`

### Dataset
- **Education_final**: consolidated dataset combining data from the different sources (papers, patents, tweets).

### User Interface
- **SidebarPanel** with `selectInput()` for user selection.  
- **MainPanel** with `uiOutput()` for dynamic rendering.  
- Navigation through a **TabsetPanel** containing five main tabs.  

### Server Logic
- Dynamic UI generation with `renderUI()`  
- Event-driven interactions with `observeEvent()`  
- Integration of multiple visual outputs and topic modeling analysis.

### Visualization Components
The dashboard includes:
- Tables  
- Bar Plots  
- Pie Charts  
- Word Clouds  
- Topic Modeling visualizations  

---

## Application
The final interactive dashboard was deployed locally through **R Shiny**.
