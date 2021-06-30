# AMR_project
 This is for the AMR project data analysis at HKU.
 
 ![School of Public Health](https://github.com/hereagain-Y/AMR_project/blob/main/Readme/HKU_ph.png)
 
 
# Table of Contents

* [General info]
* [Clean Routine ]
* [Questionnaire Analysis]
* [Farm Data Analysis]
* [R Functions]
* [Author]


## General Information 
This project is aimed to XXXXXXX
### Part I. Clean Routine 
* [Summarytable1 & 2. R](https://github.com/hereagain-Y/AMR_project/blob/main/Clean%20routine/Summarytable2.R)    
An easy step was craeted for cleaning our Ast result dataset, by running these two file, a summary table1 collecting information about isolates and sample counts from every sources can rendered. Data can be downloaded [here](https://aces.sph.hku.hk/aces/_ast_result_viewlist.php?order=a_cefoxitin_result&ordertype=ASC)  
Also, for summarytable 2, resistance rates calculated from each source in every bacterials are reported in the output excel file. A simple heatmap showing the relative frequency of resistant in E.coli is obtaind in this file.     
<p align="center">
<img src="https://github.com/hereagain-Y/AMR_project/blob/main/Readme/ecomap.png" width="400" height="300">
</p>

* [Find missing isolates. R](https://github.com/hereagain-Y/AMR_project/blob/main/Clean%20routine/Find_missing_isolates.R)          
In this file, we are aimed to find out the missing isolates which finished MULDI-TOF and validated as from the targeted bacteria, but forgot to complete AST test.

* [Questionnaire_monthly_summary.R](https://github.com/hereagain-Y/AMR_project/blob/main/Clean%20routine/Questinnaire_monthly_summary.R)  
Table 1 about our questionnaire answers is constructed, comparisions can be customized to our Gender and Age according to the group variable. Chisq/ fisher test is applied for categorical data analysis, other wise, Wilcoxon-Mann-Whitney test or Kruskal Wallis test is adopted.  

###  Part II. Questionnaire Analysis  

* [Antibiotics Classification](https://github.com/hereagain-Y/AMR_project/blob/main/Questionnaire%20analysis/ABX_classification.R)  
_Antibiotics classification based on WHO guideline._
* Hetero-resistance  
[_Check_homo1.R_](https://github.com/hereagain-Y/AMR_project/blob/main/Questionnaire%20analysis/Check_homo1.R) & [_Check_heter2.R_](https://github.com/hereagain-Y/AMR_project/blob/main/Questionnaire%20analysis/Check_heter2.R)    
_Hetero-resistance condition _No.1_: Isolates from same samples exhibt different AST result using MIC cut-off method.  
Hetero-resistance condition _No.2_: Isolates who meet the requirement 1 must also meet the _No.2_ criteria which is showing high-resistance (>= 8 fold)._

* [_Heatmap_](https://github.com/hereagain-Y/AMR_project/tree/main/Questionnaire%20analysis/Heatmap_pattern)    
Plot human ast data, grouped by variables we are interested (_ie. Age, Gender, Drug use frequency_), it supposed to look like this.   
<p align="center">
<img src="https://github.com/hereagain-Y/AMR_project/blob/main/Readme/0615AgeGroup3.png" width="800" height="400">
</p>
 

### Part III. Farm Data Analysis
### Part IV. R Functions 
* summary of plot functions   

|Function            | Plot   | Description             | 
|---------           | :------| -----------------------:| 
|✅plotmap()           | heatmep| plot for....            |

* Count function 

|Function                 | Location | Description             | 
|---------                | :--------| -----------------------:| 
|✅antibiolength.count() | Count_antibiogram.R| Calculate the antibiogram length for each isolates|
|-------------------------| :------------------|----------------------:                           |
|✅count_duplicates()      count_duplicates.R| Count the dupliates times of replicated rows in dataframes|
|---------                | :--------| -----------------------:| 
|✅count_frquency()| count_frquency.R| duplicated frequency of every element in a vector, and select the elements that meet our frequency.|
|---------                | :--------| -----------------------:| 

#### Important Links 
- HTML: https://www.dropbox.com/home/Proj_ACES/




* [Conclusion]
* [Author]
