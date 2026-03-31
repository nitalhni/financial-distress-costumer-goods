# Financial Distress Prediction (Consumer Goods Industry)

## 📌 Objective
This project aims to predict corporate financial distress using machine learning techniques, enabling early risk detection and supporting strategic financial decision-making.

## 📊 Dataset
- Source: Financial statements of consumer goods companies listed on the Indonesia Stock Exchange (IDX)
- Period: 2020–2023
- Total: 119 companies
- Features include financial ratios such as:
  - Return on Assets (ROA)
  - Current Ratio (CR)
  - Debt to Asset Ratio (DAR)
  - Sales Growth Rate (SGR)
  - Inventory Turnover Ratio (ITR)

## ⚙️ Tools & Technologies
- R (Random Survival Forest, Survival Analysis)
- Microsoft Excel (data preprocessing)

## 🔍 Methodology
1. Data collection and preprocessing using Excel  
2. Feature selection based on financial ratios  
3. Model development using **Random Survival Forest (RSF)**  
4. Model evaluation using **Concordance Index (C-index)**  
5. Variable importance analysis (VIMP)  

## 📈 Results
- Achieved **C-index of 0.859**, indicating strong predictive performance  
- The model effectively distinguishes between distressed and non-distressed companies  

## 🔑 Key Insights
- **Debt to Asset Ratio (DAR)** and **Return on Assets (ROA)** are the most influential predictors  
- Companies with:
  - High DAR  
  - Negative ROA  
  - Low liquidity (CR)  
  tend to have higher financial distress risk  

## 💡 Business Impact
- Enables early identification of high-risk companies  
- Supports investors and stakeholders in making **data-driven financial decisions**  
- Can be used as a reference for **risk mitigation strategies**

## 📁 Project Structure
- `data/` → dataset or sample data  
- `analysis.R` → modeling and analysis  
- `README.md` → project documentation  

## 🚀 Future Improvements
- Incorporate more financial indicators and macroeconomic variables  
- Compare with other models (e.g., Logistic Regression, XGBoost)  
- Deploy as a predictive dashboard for real-time monitoring  
