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

## 元データを加工
　以下の四つのファイルを読み込み、分析用に加工する。

・各市区町村に割り振られたコード

・旧統一教会がウェブサイトで公表している「家庭教会」の所在地

・参院選における井上義行氏の得票記録

・e-Statから取得した各市区町村の「社会・人口統計体系」データ


```{r}

# 市区町村ごとに割り振られたコード
jiscode <- read_csv("data/提出用/jiscode.csv")

#旧統一教会の拠点289カ所がおかれた259自治体にタグを振る
church <- read_csv("data/提出用/教会所在地.csv") %>%
  mutate(address_components=separate_address(address)) %>%
  unnest_wider(col = address_components) %>%
  select(-address) %>% 
  mutate(prefecture=str_sub(prefecture,start=11,end=-1),
         pref_city=str_c(prefecture,city),
         uc=1) %>% 
  distinct(pref_city,uc) %>% 
  left_join(jiscode %>% select(code,pref_city)) %>% 
  select(code,uc)

# 井上氏の得票データを読み込み、「社会・人口統計体系」データを合体
inoue <- read_csv("data/提出用/votes.csv") %>%
  mutate(inoue_ratio=井上得票*100/投票者数,
         exp_inoue_ratio=(自民得票計-井上得票)*100/投票者数,
         ldp_ratio=自民_党_の得票*100/投票者数) %>%
  select(year,code,pref_city,inoue_ratio,exp_inoue_ratio,ldp_ratio) %>%
  pivot_wider(names_from = year,
              values_from = c(inoue_ratio,exp_inoue_ratio,ldp_ratio)) %>%
  mutate(inoue_ratio_change=inoue_ratio_2022-inoue_ratio_2019,
         exp_inoue_ratio_change=exp_inoue_ratio_2022-exp_inoue_ratio_2019,
         ldp_ratio_change=ldp_ratio_2022-ldp_ratio_2019) %>%
  dplyr::select(-inoue_ratio_2022,
                -inoue_ratio_2019,
                -exp_inoue_ratio_2022,
                -exp_inoue_ratio_2019,
                -ldp_ratio_2022,
                -ldp_ratio_2019) %>%
  left_join(church) %>%
  mutate(uc=ifelse(is.na(uc),0,1)) %>% 
  left_join(read_csv("data/提出用/mun_data.csv") %>% # 各市区町村の「社会・人口統計体系」データ
              select(-地域),
            by=c("code"="area_code")) %>% 
  select_if(negate(anyNA)) %>% # NAを含む列を除外
  select(-contains("（男）"), # 意味が重複する列を除外
         -contains("（女）"),
         -contains("（外国人）"),
          -contains("その他")) %>% 
  dplyr::select(code,pref_city,uc,inoue_ratio_change,exp_inoue_ratio_change,
                ldp_ratio_change,`A2301_住民基本台帳人口（総数）`,everything())


glimpse(inoue)
```

## 井上氏の得票に各変数が与える効果を調べる
　各変数を標準化したうえで、井上氏の得票率変化量を目的変数とした単回帰分析をして、それぞれの効果量を調べる。

```{r}

# 得票率とUC有無以外のすべての説明変数を人口で割る
for(i in 8:105){
  inoue[,i] <- inoue[,i]/inoue[,7]
}

# 井上氏の得票率変化量とUC有無以外のすべての変数を標準化
for(i in 5:105){
  inoue[,i] <- inoue[,i] %>%
    scale()
}

# 分析のためにデータフレーム形式に変える
inoue <- inoue %>% 
  as.data.frame()


# 各変数と井上氏の得票率変化量との単回帰分析をする
resFrame1<-data.frame() # forループのたびに上書きする一時的なデータフレーム
resFrame2<-data.frame() # 最終結果を書き込むデータフレーム

for(i in 5:105){
  result <- summary(lm(inoue$inoue_ratio_change~inoue[,i]))
  
  coef <- result$coefficients
  
  resFrame1 <- coef # i番目の出力結果を代入
  resFrame2 <- rbind(resFrame2,resFrame1) # resFrame2の末行にresFrame1を代入
}
```


　効果量の大きい順に変数を並べると以下のようになる。


```{r}

resFrame2 %>%
  as_tibble() %>%
  mutate(num=row_number()/2,
         int=ifelse(str_sub(num,-2,-2)==".",0,1)) %>%
  filter(int==1) %>%
  select(-int) %>%
  left_join(names(inoue) %>%
              as_tibble() %>%
              mutate(num=row_number()-4) %>%
              filter(num>=1) %>%
              rename(variable=value)) %>%
  mutate(abs_estimate=abs(Estimate)) %>%
  select(num,variable,everything()) %>%
  arrange(desc(abs_estimate)) %>% 
  knitr::kable()

```

## マッチングをしていく

　以下の３モデルのそれぞれについて、最近傍マッチングと遺伝的マッチングを行う。つまり計６パターンのマッチングをする。

A)	社会・人口統計体系から入手した98個の変数すべてに、井上氏以外の自民党得票率変化量を加えた計99個
B)	社会・人口統計体系の変数全体のうち効果量の大きいもの10個に、井上氏以外の自民党得票率変化量を加えた計11個
C)	社会・人口統計体系の各分野のうち効果量が最大の変数9個に、井上氏以外の自民党得票率変化量を加えた計10個

　まずは最近傍マッチングをする。

```{r echo = TRUE,message = FALSE, warning = FALSE,result='hide'}

# 各モデルで使う変数を指定する
varsA <- c("exp_inoue_ratio_change",
           "`A2301_住民基本台帳人口（総数）`",
           "`A2101_住民基本台帳人口（日本人）`",
           "`A4101_出生数`",
           "`A4200_死亡数`",
           "`A5101_転入者数（日本人移動者）`",
           "`A5102_転出者数（日本人移動者）`",
           "`A5103_転入者数`",
           "`A5104_転出者数`",
           "`A7103_住民基本台帳世帯数（日本人）`",
           "`A9101_婚姻件数`",
           "`A9201_離婚件数`",
           "`B1101_総面積（北方地域及び竹島を除く）`",
           "`B1102_総面積（北方地域及び竹島を含む）`",
           "`B1103_可住地面積`",
           "`B1104_主要湖沼面積`",
           "`B1105_林野面積`",
           "`B1106_森林面積`",
           "`B1107_森林以外の草生地面積`",
           "`C2109_事業所数（国・地方公共団体）（経済センサス‐基礎調査結果）`",
           "`C310201_農家数（販売農家）`",
           "`C310202_農家数（自給的農家）`",
           "`C3403_製造業事業所数`",
           "`C3404_製造業従業者数`",
           "`E1101_幼稚園数`",
           "`E1501_幼稚園在園者数`",
           "`E2101_小学校数`",
           "`E2401_小学校教員数`",
           "`E2501_小学校児童数`",
           "`E3101_中学校数`",
           "`E3401_中学校教員数`",
           "`E3501_中学校生徒数`",
           "`E3901_義務教育学校数`",
           "`E3902_義務教育学校前期課程学級数`",
           "`E3903_義務教育学校後期課程学級数`",
           "`E3904_義務教育学校教員数`",
           "`E3905_義務教育学校前期課程児童数`",
           "`E3906_義務教育学校後期課程生徒数`",
           "`E4101_高等学校数`",
           "`E4501_高等学校生徒数`",
           "`F1101_労働力人口`",
           "`F1102_就業者数`",
           "`F1103_就業者数・主に仕事`",
           "`F1104_就業者数・家事のほか仕事`",
           "`F1105_就業者数・通学のかたわら仕事`",
           "`F1106_就業者数・休業者`",
           "`F1107_完全失業者数`",
           "`F1108_非労働力人口`",
           "`F1109_非労働力人口・家事`",
           "`F1110_非労働力人口・通学`",
           "`F2116_就業者数（65歳以上）`",
           "`F2201_第1次産業就業者数`",
           "`F2211_第2次産業就業者数`",
           "`F2221_第3次産業就業者数`",
           "`F2401_雇用者数（国勢調査結果）`",
           "`F2402_役員数`",
           "`F2403_雇人のある業主数`",
           "`F2404_雇人のない業主数`",
           "`F2405_家族従業者数（国勢調査結果）`",
           "`F2406_自営業主及び家族従業者数`",
           "`F2409_雇用者数（正規の職員・従業員）`",
           "`F2410_雇用者数（労働者派遣事業所の派遣社員）`",
           "`F2701_自市区町村で従業している就業者数`",
           "`F2702_県内他市区町村で従業している就業者数`",
           "`F2703_他県で従業している就業者数`",
           "`F2705_他市区町村への通勤者数`",
           "`F2801_従業地による就業者数`",
           "`F2802_他県に常住している就業者数`",
           "`F2803_他市区町村からの通勤者数`",
           "`G1201_公民館数`",
           "`G1401_図書館数`",
           "`H7701_テレビ放送受信契約数`",
           "`H770101_衛星放送受信契約数`",
           "`I5101_病院数`",
           "`I510110_精神科病院数`",
           "`I510120_一般病院数`",
           "`I510150_療養病床を有する病院数`",
           "`I5102_一般診療所数`",
           "`I510201_有床一般診療所数`",
           "`I5103_歯科診療所数`",
           "`I5211_病院病床数`",
           "`I5212_一般診療所病床数`",
           "`I5511_介護老人保健施設数（基本票）`",
           "`I5512_介護老人保健施設定員数（基本票）`",
           "`J2221_保護施設数（基本票）（医療保護施設を除く）`",
           "`J230127_介護老人福祉施設数（基本票）`",
           "`J230128_介護老人福祉施設定員数（基本票）`",
           "`J2304_老人福祉施設数（基本票）`",
           "`J230411_養護老人ホーム数（基本票）`",
           "`J230412_養護老人ホーム定員数（基本票）`",
           "`J230421_有料老人ホーム数（基本票）`",
           "`J230422_有料老人ホーム定員数（基本票）`",
           "`J230431_軽費老人ホーム数（基本票）`",
           "`J230432_軽費老人ホーム定員数（基本票）`",
           "`J250204_児童福祉施設等数（基本票）`",
           "`J250302_保育所等数（基本票）`",
           "`J250303_公営保育所等数（基本票）`",
           "`J2804_母子・父子福祉施設数（基本票）`",
           "`J2905_障害者支援施設等数（基本票）`",
           "`J2906_婦人保護施設数（基本票）`")


varsB <- c("exp_inoue_ratio_change",
           "`A4101_出生数`",
           "`A5104_転出者数`",
           "`A7103_住民基本台帳世帯数（日本人）`",
           "`E2501_小学校児童数`",
           "`F1110_非労働力人口・通学`",
           "`F2211_第2次産業就業者数`",
           "`F2403_雇人のある業主数`",
           "`H7701_テレビ放送受信契約数`",
           "`H770101_衛星放送受信契約数`",
           "`J230128_介護老人福祉施設定員数（基本票）`")

varsC <- c("exp_inoue_ratio_change",
           "`A7103_住民基本台帳世帯数（日本人）`",
           "`B1103_可住地面積`",
           "`C2109_事業所数（国・地方公共団体）（経済センサス‐基礎調査結果）`",
           "`E2501_小学校児童数`",
           "`F2403_雇人のある業主数`",
           "`G1401_図書館数`",
           "`H7701_テレビ放送受信契約数`",
           "`I5512_介護老人保健施設定員数（基本票）`",
           "`J230128_介護老人福祉施設定員数（基本票）`")


# モデルAの最近傍マッチング

model_a_n <- matchit(as.formula(paste("uc ~", paste(varsA, collapse = " + "))),
                     mehod = "nearest",
                     distance = "mahalanobis",
                     data=inoue)


# モデルBの最近傍マッチング

model_b_n <- matchit(as.formula(paste("uc ~", paste(varsB, collapse = " + "))),
                     mehod = "nearest",
                     distance = "mahalanobis",
                     data=inoue)

# モデルCの最近傍マッチング

model_c_n <- matchit(as.formula(paste("uc ~", paste(varsC, collapse = " + "))),
                     mehod = "nearest",
                     distance = "mahalanobis",
                     data=inoue)


```


　次に遺伝的マッチングをする。コードはチャンクの中に埋め込んでいるが、チャンクの中身を表示すると、膨大な計算の途中経過まで出力されてしまうため、やむを得ず非表示にしてある。マッチングのための様々なパラメータは以下の通り。

 
method = "genetic",

distance = "mahalanobis",

pop.size = 1000, 

nboots = 1000,

replace = TRUE,

verbose = TRUE,

estimand = "ATT"



```{r include = FALSE}

# 遺伝的マッチングの条件設定
pop.size <- 1000
nboots <- 1000

# モデルAの遺伝的マッチング

model_a_g <- matchit(as.formula(paste("uc ~", paste(varsA, collapse = " + "))),
                     method = "genetic",
                     distance = "mahalanobis",
                     pop.size = pop.size, 
                     nboots = nboots, 
                     replace = TRUE,
                     verbose = TRUE,
                     estimand = "ATT",
                     data=inoue)

# モデルBの遺伝的マッチング

model_b_g <- matchit(as.formula(paste("uc ~", paste(varsB, collapse = " + "))),
                     method = "genetic",
                     distance = "mahalanobis",
                     pop.size = pop.size, 
                     nboots = nboots, 
                     replace = TRUE,
                     verbose = TRUE,
                     estimand = "ATT",
                     data=inoue)


# モデルCの遺伝的マッチング

model_c_g <- matchit(as.formula(paste("uc ~", paste(varsC, collapse = " + "))),
                     method = "genetic",
                     distance = "mahalanobis",
                     pop.size = pop.size, 
                     nboots = nboots, 
                     replace = TRUE,
                     verbose = TRUE,
                     estimand = "ATT",
                     data=inoue)

```

## マッチングの結果を確認

　共変量のバランスを確かめる。いずれのモデルでも遺伝的マッチングのほうがAbsolute Standardized Mean Differencesの平均値が小さくなる。つまり、よりよいマッチングができている。

```{r}
# モデルA、最近傍
balance_a_n <- bal.tab(model_a_n,un=TRUE)
balance_a_n$Balance %>%
  as_tibble() %>%
  mutate(un_abs_mean_dif=abs(Diff.Un),
         adj_abs_mean_dif=abs(Diff.Adj)) %>%
  summarize(mean(un_abs_mean_dif),
            mean(adj_abs_mean_dif))

# モデルA、遺伝的
balance_a_g <- bal.tab(model_a_g,un=TRUE)
balance_a_g$Balance %>%
  as_tibble() %>%
  mutate(un_abs_mean_dif=abs(Diff.Un),
         adj_abs_mean_dif=abs(Diff.Adj)) %>%
  summarize(mean(un_abs_mean_dif),
            mean(adj_abs_mean_dif))

# モデルB、最近傍
balance_b_n <- bal.tab(model_b_n,un=TRUE)
balance_b_n$Balance %>%
  as_tibble() %>%
  mutate(un_abs_mean_dif=abs(Diff.Un),
         adj_abs_mean_dif=abs(Diff.Adj)) %>%
  summarize(mean(un_abs_mean_dif),
            mean(adj_abs_mean_dif))

# モデルB、遺伝的
balance_b_g <- bal.tab(model_b_g,un=TRUE)
balance_b_g$Balance %>%
  as_tibble() %>%
  mutate(un_abs_mean_dif=abs(Diff.Un),
         adj_abs_mean_dif=abs(Diff.Adj)) %>%
  summarize(mean(un_abs_mean_dif),
            mean(adj_abs_mean_dif))

# モデルC、最近傍
balance_c_n <- bal.tab(model_c_n,un=TRUE)
balance_c_n$Balance %>%
  as_tibble() %>%
  mutate(un_abs_mean_dif=abs(Diff.Un),
         adj_abs_mean_dif=abs(Diff.Adj)) %>%
  summarize(mean(un_abs_mean_dif),
            mean(adj_abs_mean_dif))


# モデルC、遺伝的
balance_c_g <- bal.tab(model_c_g,un=TRUE)
balance_c_g$Balance %>%
  as_tibble() %>%
  mutate(un_abs_mean_dif=abs(Diff.Un),
         adj_abs_mean_dif=abs(Diff.Adj)) %>%
  summarize(mean(un_abs_mean_dif),
            mean(adj_abs_mean_dif))


```


## 教団施設の効果を確認
　マッチングした市区町村を比べ、教団施設の有無によって井上氏の得票率変化量がどれだけ変わるかを見てみる。いずれのモデル、マッチング法でも0.07%ポイント前後で、有意な値となっている。


```{r}

# モデルA、最近傍
data_a_n <- get_matches(model_a_n) %>%
  as_tibble() %>%
  left_join(read_csv("data/提出用/votes.csv") %>%
              filter(year==2022) %>%
              dplyr::select(code,投票者数))

summary(lm(inoue_ratio_change ~ uc, data = data_a_n, weights = 投票者数))
confint(lm(inoue_ratio_change ~ uc, data = data_a_n, weights = 投票者数))

# モデルA、遺伝的
data_a_g <- get_matches(model_a_g) %>%
  as_tibble() %>%
  left_join(read_csv("data/提出用/votes.csv") %>%
              filter(year==2022) %>%
              dplyr::select(code,投票者数))

summary(lm(inoue_ratio_change ~ uc, data = data_a_g, weights = 投票者数))
confint(lm(inoue_ratio_change ~ uc, data = data_a_g, weights = 投票者数))

# モデルB、最近傍
data_b_n <- get_matches(model_b_n) %>%
  as_tibble() %>%
  left_join(read_csv("data/提出用/votes.csv") %>%
              filter(year==2022) %>%
              dplyr::select(code,投票者数))

summary(lm(inoue_ratio_change ~ uc, data = data_b_n, weights = 投票者数))
confint(lm(inoue_ratio_change ~ uc, data = data_b_n, weights = 投票者数))


# モデルB、遺伝的
data_b_g <- get_matches(model_b_g) %>%
  as_tibble() %>%
  left_join(read_csv("data/提出用/votes.csv") %>%
              filter(year==2022) %>%
              dplyr::select(code,投票者数))

summary(lm(inoue_ratio_change ~ uc, data = data_b_g, weights = 投票者数))
confint(lm(inoue_ratio_change ~ uc, data = data_b_g, weights = 投票者数))

# モデルC、最近傍
data_c_n <- get_matches(model_c_n) %>%
  as_tibble() %>%
  left_join(read_csv("data/提出用/votes.csv") %>%
              filter(year==2022) %>%
              dplyr::select(code,投票者数))

summary(lm(inoue_ratio_change ~ uc, data = data_c_n, weights = 投票者数))
confint(lm(inoue_ratio_change ~ uc, data = data_c_n, weights = 投票者数))


# モデルC、遺伝的
data_c_g <- get_matches(model_c_g) %>%
  as_tibble() %>%
  left_join(read_csv("data/提出用/votes.csv") %>%
              filter(year==2022) %>%
              dplyr::select(code,投票者数))

summary(lm(inoue_ratio_change ~ uc, data = data_c_g, weights = 投票者数))
confint(lm(inoue_ratio_change ~ uc, data = data_c_g, weights = 投票者数))


```

## 良いマッチングができていることをグラフで確認
　モデルAの遺伝的マッチングを例に、共変量のバランスがマッチングによって大幅に改善されていることを確認する。

```{r fig.width=6, fig.height=12}
love.plot(model_a_g,
          threshold = 0.1,
          abs = TRUE,
          colors=c("black","black"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("マッチングなし","マッチングあり"),
          title="Figure1 共変量のバランス（モデルA、遺伝的）") +
  theme(legend.position = c(0.8, 0.05),
        axis.text.y = element_text(size = 6)) # 縦軸のテキストサイズを小さくする

```

![Figure 1 共変量のバランス（モデルA、遺伝的）](figure-html/unnamed-chunk-9-1.png)

## 井上氏の得票率変化量の違いをグラフで確認
　モデルAで遺伝的マッチングをした各市区町村のペアにおいて、教団施設の有無によって井上氏の得票率変化量がどれだけ違うのかをグラフで確認する。下へ行くほど投票者数の少ない市区町村になっている。

```{r fig.width=6, fig.height=12}

data_a_g %>%
  select(subclass, uc, inoue_ratio_change, 投票者数) %>%
  group_by(subclass) %>%
  mutate(mean_vote = mean(投票者数)) %>%
  select(subclass, uc, inoue_ratio_change, mean_vote) %>%
  pivot_wider(names_from = uc, values_from = inoue_ratio_change) %>%
  rename(平均投票者数 = mean_vote, 教団施設あり = `1`, 教団施設なし = `0`) %>%
  ggplot(aes(x = 教団施設なし, y = 教団施設あり, size = 平均投票者数)) +
  geom_point(alpha = .1) +
  geom_abline(slope = 1, intercept = 0) +
  scale_size_continuous(breaks = c(10000, 50000, 100000),
                        labels = c("10,000", "50,000", "100,000")) +
  scale_x_continuous(breaks = seq(-0.3,0.7,by=0.2)) +
  scale_y_continuous(breaks = seq(-1.5,2, by = 0.2)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  labs(title = "マッチングした各自治体での得票率変化量",
       subtitle = "%ポイント。教団施設「あり」のほうが「なし」を上回るケースが多い")

```

## 付録

DID分析をする。

```{r appendix}

inoue2019_2022 <- read_csv("data/提出用/votes.csv") %>%
  select(-自民_党_の得票) %>% 
  left_join(church)

did <- inoue2019_2022 %>%
  mutate(inoue_ratio=井上得票*100/投票者数,
         exp_inoue_ratio=(自民得票計-井上得票)*100/投票者数,
         uc=ifelse(is.na(uc),0,1),
         year_dummy=ifelse(year==2022,1,0)) %>%
  select(-有権者数,-井上得票,-自民得票計) %>%
  select(year,code,pref_city,inoue_ratio,everything()) %>% 
  left_join(read_csv("data/提出用/mun_data.csv") %>%
              select(-地域),
            by=c("code"="area_code")) %>%
  select_if(negate(anyNA)) 

controls <- colnames(did)[9:128] %>%
  sapply(function(x) {
    x <- sub("^", "`", x)
    sub("$", "`", x)
    })

# DID分析

lm(as.formula(paste("inoue_ratio ~ year_dummy + uc + year_dummy:uc + exp_inoue_ratio +",
                    paste(controls, collapse = " + "))),
   data = did, weights = 投票者数) %>% 
  tidy() %>% 
  mutate(across(estimate:statistic, ~format(., scientific = FALSE, digits = 3))) %>%
  mutate(across(estimate:p.value, ~format(., scientific = TRUE, digits = 3))) %>% 
  gt()


inoue2013 <- read_csv("data/提出用/福元データ23.csv") %>%
  mutate(年=(as.double(as.double(str_sub(選挙回,-2,-1)))-19)*3+2001) %>%
  mutate(名前=str_replace(名前," ","")) %>%
  filter(名前=="井上義行") %>%
  rename(pref=都道府県名,
         city=市区町村名,
         井上得票=市区町村別得票数,
         party="党派・会派等",
         name=名前,
         year=年,
         code=JISコード) %>%
  select(year,
         code,
         pref,
         city,
         name,
         井上得票,
         投票者数,
         有権者数) %>% 
  mutate(code=as.character(ifelse(str_length(code)==4,str_c(0,code),code))) %>% 
  mutate(city=str_replace(city,"ケ","ヶ"))



did_chart <- bind_rows(inoue2013, read_csv("data/提出用/votes.csv") %>%
                         select(-自民_党_の得票)) %>%
  left_join(church) %>%
  group_by(year,uc) %>%
  summarize(井上得票=sum(井上得票),
            投票者数=sum(投票者数)) %>%
  mutate(inoue_ratio=井上得票*100/投票者数) %>%
  mutate(uc=ifelse(is.na(uc), "なし", "あり")) %>%
  ggplot(aes(year,inoue_ratio,color=uc))+geom_line() +
  scale_x_continuous(breaks = c(2013, 2019, 2022)) +
  scale_color_manual(name = "教団施設",
                     values = c("あり" = "red", "なし" = "blue")) +
    labs(title="3回の参院選における井上氏の得票率の推移",
         x="年",
         y="得票率（％）")

# Define the points and the slope for the new line
x1 <- 2019
y1 <- 0.19
slope <- 0.03266667
x2 <- 2022
y2 <- y1 + slope * (x2 - x1)

# Add the line segment to the plot
did_chart + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                         color = "black", linetype = "dotted")

```

以上。
