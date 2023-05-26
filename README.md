# [Group4] House Prices - Advanced Regression Techniques
The goals of this project.
redict sales prices and practice feature engineering, RFs, and gradient boosting

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|吉瀚宇|資碩工一|111753157|團隊中的吉祥物🦒，負責增進團隊氣氛| 
|思沛淇|資碩計一|111753214|團隊的中流砥柱，一個人打十個，地下勢力團員|
|陳輝|資碩計一|111753228|團隊中的吉祥物🦒，負責增進團隊氣氛|
|張義猷|資碩計一|111753230|團隊中的吉祥物🦒，負責增進團隊氣氛|
|徐宇文|資管碩一|111356003|團隊中的吉祥物🦒，負責增進團隊氣氛|
## Quick start
You might provide an example commend or few commends to reproduce your analysis, i.e., the following R script
```R
Rscript code/your_script.R --input data/training --output results/performance.tsv
```

## Folder organization and its related description
idea by Noble WS (2009) [A Quick Guide to Organizing Computational Biology Projects.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424) PLoS Comput Biol 5(7): e1000424.

### docs
* Your presentation, 1112_DS-FP_groupID.ppt/pptx/pdf (i.e.,1112_DS-FP_group1.ppt), by **06.08**
* Any related document for the project
  * i.e., software user guide

### data
* Input
  * Source [House Prices - Advanced Regression Techniques](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/overview)
  * Format [CSV File](data/sample_submission.csv)
  * Size  train.csv -> 81 columns, 1460 rows
          test.csv  -> 80 cloumns, 1459 rows
* Output 
  * [Ensamble2.csv](results/Ensamble2.csv)
### code
* Analysis steps
    * 在test.csv中新增一個 "SalePrice" 欄位，並設定初始值為 0
    * 合併train.csv以及test.csv兩個資料集
    * 數據清理和預處理：對收集到的數據進行清理和預處理，包括處理缺失值、處理異常值、數據平滑化、特徵選擇或提取
    * 根據問題的特點，對數據進行特徵工程，例如特徵縮放、特徵轉換、特徵組合等，以提取有用的信息。
    * 根據原本的特徵，再增加其他新特徵
    * 根據問題的性質和數據的特點，選擇合適的機器學習模型，並設置相應的參數。
    * 使用訓練集對選定的模型進行訓練，通過迭代優化模型的參數，以最小化訓練誤差。
    * 使用測試集評估已訓練模型的性能，例如計算準確率、精確率、召回率等指標。
    * 根據評估結果，對模型進行優化，可能需要調整模型參數、嘗試不同的特徵工程方法或嘗試其他模型。
    * 將訓練好且經過優化的模型應用於實際場景，並進行實時預測或決策。
* Which method or package do you use? 
  * original packages in the paper
  * additional packages you found

### results
* What is a null model for comparison?
* How do your perform evaluation?
  * Cross-validation, or extra separated data

## References
* Packages you use
* Related publications
