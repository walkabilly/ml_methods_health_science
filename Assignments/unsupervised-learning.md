---
title: 'Assignment: Unsupervised Learning'
output:
  html_document:
    keep_md: true
  pdf_document: default
---

**Value:** 10% of Final Grade  

---

#### **Description**  
This assignment focuses on developing your understanding and application of unsupervised algorithms. You will work with the **Can Path Student Dataset** to perform a Principal Component Analysis using variables you select. 

---

#### **Assignment Objectives**  
By completing this assignment, you will:  
1. Develop proficiency in implementing PCA methods.  
2. Understand the importance of cluster selection
3. Gain experience in comparing models to select the best-performing configuration.
4. Learn to interpret PCA results.  

---

#### **Assignment Tasks**  

1. **Dataset Exploration**  
   - Load and examine the **Can Path Student Dataset (Not Imputed)**.  
   - Conduct an initial exploratory data analysis (EDA) to understand key features and relationships.  

2. **Baseline PCA  Model**  
   - Implement a baseline PCA model with default parameters.  
   - Evaluate the model using appropriate metrics.  

3. **Cluster Selection Tuning**  
   - Identify key decisions related to cluster selection.
   - Use techniques to find the optimal configuration.  
   - Document the process using an RMarkdown File. 

4. **PCA Regression Model Comparisons**  
   - Conduct a PCA regression using your original cluster selection (Part 2 - Baseline PCA Model) and your tuned model (Part 3 - Cluster Select):  
    - Use BMI as the outcome variable for the PCA regression. This will 
    - Analyze differences in performance using quantitative metrics and discuss possible reasons for the results.  

5. **Feature Importance Analysis**  
   - Extract and interpret feature importance scores from the PCA regression from the best fitting model.
   - Visualize the top contributing features and discuss their relevance to the dataset.  

---

#### **Deliverables**  

Submit a structured report in .Rmd format and upload to your personal Github page. I will be running your .Rmd file so please make sure your submission is executable and replicable. Remember to document your process thoroughly as with descriptions in the text of the .Rmd file. 

1. **Analysis Report**  
     - A summary of the dataset and EDA results.  
     - Steps and rationale for building the baseline and updated models.  
     - Comparisons between models with performance metrics and observations.  
     - Interpretation of PCA results.  

2. **Code and Documentation**  
   - Submit well-documented code, organized into scripts or notebooks.  
   - Ensure reproducibility by providing clear instructions and comments.  

3. **Visualizations**  
   - Include at least two visualizations to support your analysis.  

---

#### **Assessment Criteria**  
1. **Dataset Preparation and EDA (15%)**  
   - Thoroughness of exploration and preprocessing steps.  

2. **Baseline Model Implementation (15%)**  
   - Correctness and clarity in building and evaluating the baseline PCA model.  

3. **Rotations and clusters (25%)**  
   - Rigor in process and documentation of results.  

4. **Model Comparisons (25%)**  
   - Thoughtfulness in model selection and depth of analysis in comparing performance.  
   
5. **Documentation and Presentation (20%)**  
   - Clarity, organization, and professionalism in the report and code.  
