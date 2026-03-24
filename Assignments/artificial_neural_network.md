---
title: "Assignment: Artificial Neural Network"
output:
  html_document:
    keep_md: yes
---

**Value:** 15% of Final Grade  

---

#### **Description**  
This assignment focuses on developing your understanding and application of artificial neural networks (ANN). You will work with the **Can Path Student Dataset** to train an artificial neural network, conduct detailed hyperparameter tuning, and compare the performance of your model with other models or configurations. This exercise emphasizes building robust models and evaluating their performance critically.

---

#### **Assignment Objectives**  
By completing this assignment, you will:  
1. Develop proficiency in implementing ANN for predictive analysis.  
2. Understand the importance of hyperparameter tuning and its impact on model performance.  
3. Gain experience in comparing models to select the best-performing configuration.  
4. Learn to interpret ANN results.  

---

#### **Assignment Tasks**  

1. **Dataset Exploration**  
   - Load and examine the **Can Path Student Dataset**.  
      - Select an outcome of your choice. You can use diabetes like we did in the data work if you want. The outcome can be continuous, binary, or categorial. 
   - Conduct an initial exploratory data analysis (EDA) to understand key features and relationships. 
   - Handle missing data, outliers, or inconsistencies if necessary.  

2. **Baseline Artificial Neural Network**  
   - Split the dataset into training and testing sets.  
   - Implement a baseline ANN model with default hyperparameters.  
   - Evaluate the model using appropriate metrics (e.g., accuracy, precision, recall, F1-score, or ROC-AUC).  

3. **Hyperparameter Tuning**  
   - Identify key hyperparameters to tune.  
   - Use techniques to find the optimal hyperparameter configuration.  
   - Document the tuning process, including parameters tested and evaluation results.  

4. **Model Comparisons**  
   - Compare the tuned ANN against the baseline ANN.
   - Analyze differences in performance using quantitative metrics and discuss possible reasons for the results.  

5. **Feature Importance Analysis**  
   - Discuss why you can or cannot use feature important in an ANN.

---

#### **Deliverables**  

Submit a structured report in .Rmd format and upload to your personal Github page in a new repo. I will be running your .Rmd file so please make sure your submission is executable and replicable. Remember to document your process thoroughly as with descriptions in the text of the .Rmd file. Submit the link for the github repo into the assignment page in Canvas.

1. **Analysis Report**  
     - A summary of the dataset and EDA results.  
     - Steps and rationale for building the baseline and tuned models.  
     - Comparisons between models with performance metrics and observations.  
     - Interpretation of feature importance results.  

2. **Code and Documentation**  
   - Submit well-documented code, organized into scripts or notebooks.  
   - Ensure reproducibility by providing clear instructions and comments.  

3. **Visualizations**  
   - Include at least two visualizations to support your analysis (e.g., feature importance plots, ROC curves, confusion matrix).  

---

#### **Assessment Criteria**  
1. **Dataset Preparation and EDA (15%)**  
   - Thoroughness of exploration and preprocessing steps.  

2. **Baseline Model Implementation (15%)**  
   - Correctness and clarity in building and evaluating the baseline ANN.  

3. **Hyperparameter Tuning (25%)**  
   - Rigor in tuning process and documentation of results.  

4. **Model Comparisons (25%)**  
   - Thoughtfulness in model selection and depth of analysis in comparing performance.  

5. **Feature Importance Analysis (10%)**  
   - Quality of interpretation and presentation of feature importance.  

6. **Documentation and Presentation (10%)**  
   - Clarity, organization, and professionalism in the report and code.  
