# 🧠 Machine Learning Work Samples – Biomedical Applications in R

## 📘 Overview

This folder contains a curated collection of applied machine learning work completed by **Kathleen Ashbaker** for the course **BIOSTAT 546: Machine Learning in Biomedical Sciences** at the University of Washington.

The projects use the **R programming language** to explore and implement key classification and predictive modeling techniques, including:
- Logistic Regression
- Linear and Quadratic Discriminant Analysis (LDA, QDA)
- k-Nearest Neighbors (kNN)
- Naive Bayes Classifier
- Decision Trees and Random Forests

---

## 📁 Folder Contents

| File Name           | Description                                                                 |
|---------------------|-----------------------------------------------------------------------------|
| `HOMEWORK_2_KA.pdf` | Binary classification of breast cancer tumors using multiple ML models     |
| `HOMEWORK_3_KA.pdf` | Naive Bayes modeling for medical diagnosis; ROC and AUC analysis            |
| `HOMEWORK_4_KA.pdf` | Tree-based methods and ensemble learning; includes Random Forests and pruning |

---

## 🔍 Homework Highlights

### 📄 `HOMEWORK_2_KA.pdf` – **Breast Cancer Diagnosis**
- **Dataset**: Wisconsin Diagnostic Breast Cancer (WDBC)
- **Methods**:
  - Logistic Regression with probability cutoffs (0.25, 0.5, 0.75)
  - Linear and Quadratic Discriminant Analysis
  - k-Nearest Neighbors (k = 1 to 20)
- **Key Metrics**:
  - AUC ≈ 0.95+ for all models
  - Accuracy up to **92.9%** with kNN

### 📄 `HOMEWORK_3_KA.pdf` – **Naive Bayes Classification**
- **Context**: Classifying medical diagnostic data
- **Techniques**:
  - Naive Bayes with Gaussian assumptions
  - ROC curve analysis and AUC computation
  - Manual derivation of Bayes rule outcomes
- **Goal**: Evaluate model fit and robustness with visualization of decision boundaries

### 📄 `HOMEWORK_4_KA.pdf` – **Tree-Based Models**
- **Topics**:
  - CART (Classification and Regression Trees)
  - Tree pruning to prevent overfitting
  - Random Forest ensemble learning
- **Metrics**:
  - Variable importance plots
  - Misclassification error rates vs. tree depth
- **Output**: Interpretable tree structures and model performance comparison

---

## 💻 Technical Stack

- **Language**: R
- **Libraries Used**:
  - `ggplot2`, `GGally` – Visualization
  - `MASS` – LDA/QDA
  - `class` – kNN
  - `pROC` – ROC/AUC analysis
  - `rpart`, `randomForest` – Decision trees and ensembles
  - `caret` – Confusion matrices, data splitting

---

## 📌 Educational Value

These assignments demonstrate:
- End-to-end ML workflows on real-world biomedical datasets
- Thoughtful model selection, tuning, and evaluation
- Strong grasp of both statistical reasoning and practical coding in R
- Visualization of complex classification boundaries

---

## 👤 Author

**Kathleen Ashbaker**  
Graduate Student, University of Washington  
GitHub: [@QueenKatherys](https://github.com/QueenKatherys)  
Email: Available upon request

---

## 📂 Notes

These documents are part of a **machine learning work sample folder** submitted to prospective employers and academic reviewers. They are representative of Kathleen’s analytical rigor, clear communication, and technical competence in biomedical data science.

