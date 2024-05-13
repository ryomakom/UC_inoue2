---
title: "旧統一教会が参院選に与えた影響の推定"
output: html_document
date: '2024-03-14'
author: 朝日新聞デジタル企画報道部　小宮山亮磨 @ryomakom
---

## この文書について
　日本選挙学会の機関誌「選挙研究」に投稿する予定の論文「旧統一教会が参院選に与えた影響の推定　自民党・井上義行氏の得票を例に」の分析手法や結果について、もとになったデータやRのコードとともに公開します。

## 必要なパッケージをとってくる

```{r message=FALSE}

library(tidyverse)
library(modelsummary)
library(MatchIt)
library(rgenoud)
library(cobalt)
library(zipangu)
library(estimatr)
library(gt)

if (!require("broom", character.only = TRUE)) {
  install.packages("broom")
  library("broom", character.only = TRUE)
}

```
