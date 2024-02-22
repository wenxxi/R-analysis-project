# 經濟壓力與抗憂鬱藥物使用的相關性
### Author: B0902063 B0603176
### Date: 1/5/2022
## 壹、研究動機
  憂鬱症在台灣是一種相當普遍的精神疾病，隨著社會經濟的發展，人數似乎有逐年攀升的趨勢。而新聞中經常報導現代人龐大的經濟壓力（如：高房價、高物價）導致的憂鬱與困境，因此，在本研究中，我們以消費者物價指數和房價負擔能力作為民生經濟的指標，進行經濟壓力與憂慮症藥物使用的關聯性分析。



## 貳、研究方法
  - 下載官方資料並整裡出所需數據
  - 通過R本身內建函式庫與安裝ggplot2, rgdal, tmap等套件以進行統計分析
  - 繪製直方圖、點狀圖與相關係數的計算並將統計資料結合台灣地圖使資料可視化

## 參、資料統計與結果分析
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, 
  fig.show = "asis", fig.showtext = TRUE
) #將文字設定為中文
```

### 100~109年度使用抗憂慮劑之病人數10年統計
```{r}
#install.packages("ggplot2")
csvpath1 = file.path("patient_yr.csv")
csv1 = read.csv(csvpath1,sep=",")

csv1
str(csv1)
```

```{r}
library(ggplot2)

p1 = ggplot(csv1, aes(x = 年分 , y = 使用抗憂慮劑之病人數10年統計)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = 使用抗憂慮劑之病人數10年統計), vjust = -0.3, size = 3) 
 theme(text = element_text(family= "黑體-繁 中黑")) #繪製直方圖並調整文字格式
p1
```

### 100~109消費者物價指數
```{r}
csvpath2 = file.path("eco1.csv")
csv2 = read.csv(csvpath2,sep=",")

csv2
str(csv2)
```

```{r}
p2 = ggplot(csv2, aes(x = 年分 , y = 消費者物價指數)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = 消費者物價指數), vjust = -0.3, size = 3) 
 theme(text = element_text(family= "黑體-繁 中黑"))
p2
```
### 100~109憂慮症用藥人數與消費者物價指數的關聯性
```{r}
cor(csv1$使用抗憂慮劑之病人數10年統計, csv2$消費者物價指數) #相關係數
```

```{r}
p3 = plot(x = csv2$消費者物價指數, y = csv1$使用抗憂慮劑之病人數10年統計, main = "100~109憂慮症用藥人數與消費者物價指數的關聯性分析", xlab ="消費者物價指數", ylab = "使用抗憂慮劑之病人數10年統計",col=c("blue")) #點狀圖
p3
abline(lm(csv1$使用抗憂慮劑之病人數10年統計~csv2$消費者物價指數), col = c("Red")) #回歸直線

```
### 台灣抗憂鬱藥物使用之縣市比例圖
```{r}
csvpath3 = file.path("pat_city.csv")
csv3 = read.csv(csvpath3,sep=",")

csv3
str(csv3)
```

```{r}
data = csv3[,2:6]
avg = apply(data,1,mean) #計算行的平均值
avg

csv3$各縣市抗憂鬱藥物使用人數五年平均統計 = avg
csv3
```

```{r}
total = sum(csv3[,7])
total

ratio = c(csv3[,7]/total)
ratio
```

```{r}
p4 = pie(ratio, csv3[,1], main = "台灣抗憂鬱藥物使用之縣市比例圖", family = "黑體-繁 中黑", cex = 0.5) #製作圓餅圖並調整文字格式
```

```{r}
rate_data = data.frame(
  city = csv3$各縣市抗憂鬱藥物使用人數統計, rate = ratio
) #建立包含縣市與藥物使用比例的數據框
rate_data
```

```{r}
#install.packages("rgdal")
#install.packages("tmap")
library(rgdal)
library(tmap)

tw = readOGR("/Users/wenxi/Desktop/R/gadm36_TWN_shp/gadm36_TWN_2.shp") #讀取檔案路徑


new_tw = tw[-1,]
new_tw = new_tw[-1,]
new_tw = new_tw[-15,]
new_tw #已刪除不需要的資料

new_tw$zh_NAME=new_tw$NL_NAME_2
new_tw$zh_NAME[new_tw$zh_NAME=="台中"]="臺中市"
new_tw$zh_NAME[new_tw$zh_NAME=="台南"]="臺南市"
new_tw$zh_NAME[new_tw$zh_NAME=="台北市"]="臺北市"
new_tw$zh_NAME[new_tw$zh_NAME=="台東縣"]="臺東縣" #將不同的縣市名稱改為相同



new_tw@data=merge(new_tw@data, rate_data, by.x="zh_NAME", by.y="city",sort=F) #根據縣市名將地圖與統計資料合併
head(new_tw@data) #預覽數據

qtm(new_tw,fill="rate", text="zh_NAME", text.size =0.7, fill.title="抗憂鬱藥物使用比例", fill.palette="Reds") #製作統計地圖
```



### 各縣市房價負擔能力與憂鬱藥物使用的關聯
```{r}
csvpath4 = file.path("houseafford.csv")
csv4 = read.csv(csvpath4,sep=",")

csv4
str(csv4)

data2 = csv4[,2:6]
avg2 = apply(data2,1,mean)
avg2

csv4$房貸負擔能力平均 = avg2
csv4
```

```{r}
cor(ratio, csv4$房貸負擔能力平均) #相關係數
```

```{r}
p4 = plot(x = csv4$房貸負擔能力平均, y = ratio, main = "各縣市房價負擔能力與憂鬱藥物使用人數比例的關聯性分析", xlab ="房貸負擔能力", ylab = "各縣市抗憂鬱藥物使用人數比例",col=c("blue"),family = "黑體-繁 中黑") #點狀圖
p4
abline(lm(ratio~csv4$房貸負擔能力平均), col = c("Red")) #回歸直線
```

## 肆、結論
以全台灣角度分析：
抗憂鬱藥物使用人數與消費者物價指數均呈現逐年成長的趨勢，透過關聯性分析可知兩者高度正相關。
以個別縣市分析：
抗憂鬱藥物使用人數以直轄市占比最高，透過房貸負擔能力作為各縣市經濟壓力的指標，分析後發現兩者呈現高度正相關。
透過以上統計結果，可以初步推斷抗憂鬱藥物的使用與經濟壓力存在正相關性。所以在經濟發展的同時關注人民的身心健康也是國家政府應該注重的方向。此外，目前的分析中已經考量物價與房價的因素，日後如果有相關的研究可以往更多經濟因素進行探討並結合憂鬱症的臨床治療分析已提出可能的緩解方案。

## 伍、參考資料
- 衛生福利部統計處：抗憂鬱藥物使用人數
- 行政院主計總處：消費者物價指數統計
- 內政部不動產資訊平台：房價負擔能力統計
- http://libir.tmu.edu.tw/bitstream/987654321/58223/2/enews_33_biostat.pdf
- https://rpubs.com/skydome20/R-Note4-Plotting_System
