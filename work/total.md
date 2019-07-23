산업연관표 분석
================

작업순서

1.  ECOS류의 2015년 소분류 투입산출표를 통해 대분류 투입산출표 도출

2.  대분류 투입산출표(생산자가격\_총거래표)를 바탕으로 총투입계수 및 국산투입계수, 생산유발계수 등 도출

3.  투입계수를 바탕으로 특정 부문의 산업구조를 파악

4.  생산유발계수, 부가가치유발계수의 2010년, 2015년 연도별 비교 그래프(2015년 기준년 산업연관표 작성결과 당행 보도자료 참조)

1.기본 세팅
-----------

``` r
rm(list=ls())
setwd("c:/Users/user/Documents/2019IO")

# 라이브러리 로딩
packages = c('tidyverse', 'XLConnect' )
lapply(packages, require, character.only = T)
```

    ## Loading required package: tidyverse

    ## -- Attaching packages -------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.2.0     √ purrr   0.3.2
    ## √ tibble  2.1.3     √ dplyr   0.8.3
    ## √ tidyr   0.8.3     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Loading required package: XLConnect

    ## Loading required package: XLConnectJars

    ## XLConnect 0.2-15 by Mirai Solutions GmbH [aut],
    ##   Martin Studer [cre],
    ##   The Apache Software Foundation [ctb, cph] (Apache POI),
    ##   Graph Builder [ctb, cph] (Curvesapi Java library)

    ## http://www.mirai-solutions.com
    ## https://github.com/miraisolutions/xlconnect

    ## [[1]]
    ## [1] TRUE
    ## 
    ## [[2]]
    ## [1] TRUE

2. 대분류 투입산출표(생산자가격\_총거래표)를 바탕으로 총투입계수 및 국산투입계수, 생산유발계수 등 도출
------------------------------------------------------------------------------------------------------

``` r
# 대분류 투입산출표 로딩
raw <- loadWorkbook("c:/Users/user/Documents/2019IO/data/IO_tab_2015_major_cat_f.xlsx")

# 생산자 가격 총거래표 sheet 로딩
data <- readWorksheet(raw, sheet = 1, startRow = 7, startCol = 3, header = F)

# 각 열의 이름 지정
colname_sector <- readWorksheet(raw, sheet = 1, startRow = 6, endRow = 6, startCol = 3, header = F) 
colnames(data) <- colname_sector

# tbl형태로 데이터 변환
data_tbl <- as_tibble(data)


# 중간수요
data_1 <- select(data_tbl, '농림수산품' : '기타')
intermediate_demand <- matrix(unlist(data_1[-c(34:40), ]), nrow = 33, byrow = F)

# 부가가치
added_value <-  unlist(data_1[39, ])

# 총투입=산출액(자가공정산출액 포함)
x <- unlist(data_1[40, ])

# 최종수요
y <- select(data_tbl, "최종수요계")
y <- unlist(y[-c(34 : 40), ])

# 수입
m <- select(data_tbl, "수입")
m <- unlist(m[-c(34 : 40), ])


# 투입계수행렬
x_1 <- matrix(rep(x, times = 33), nrow = 33, byrow = T)
A <- intermediate_demand / x_1
colnames(A) <- colname_sector[1,  1 : 33]

# 부가가치율
added_value_rate <- added_value / x

# 생산유발계수 (1)형
I <- diag(x = 1, nrow = 33)
B <- solve(I - A)
```

3. 투입계수를 바탕으로 특정 부문의 산업구조를 파악
--------------------------------------------------

``` r
# 입력값 설정
n <- dim(A)[1] ## 투입계수 행렬의 크기 = 산업의 개수
target <- 17 ## 분석대상 산업, ex) 전력, 가스 및 증기 : 17


#파급경로 생성(단계별)
T1 <- cbind(rep(target,n),seq(1,n)) #1단계 파급경로 행렬(n*2)

T2 <- matrix(0,nrow=n^2,ncol=3)
tem <- matrix(0,nrow=n^2,ncol=2)
for (z in 1:n){
  for (i in 1:dim(T1)[1]){
    for(j in 1:dim(T1)[2]){
      tem[n*(z-1)+i,j] <- T1[i,j]
    }}}
T2_new <- rep(seq(1,n), each=n^1)
T2 <- cbind(tem,T2_new) #2단계 파급경로 행렬(n^2*3)

T3 <- matrix(0,nrow=n^3,ncol=4)
tem <- matrix(0,nrow=n^3,ncol=3)
for (z in 1:n){
  for (i in 1:dim(T2)[1]){
    for(j in 1:dim(T2)[2]){
      tem[n^2*(z-1)+i,j] <- T2[i,j]
    }}}
T3_new <- rep(seq(1,n), each=n^2)
T3 <- cbind(tem,T3_new) #3단계 파급경로 행렬(n^3*4)

T4 <- matrix(0,nrow=n^4,ncol=5)
tem <- matrix(0,nrow=n^4,ncol=4)
for (z in 1:n){
  for (i in 1:dim(T3)[1]){
    for(j in 1:dim(T3)[2]){
      tem[n^3*(z-1)+i,j] <- T3[i,j]
    }}}
T4_new <- rep(seq(1,n), each=n^3)
T4 <- cbind(tem,T4_new) #4단계 파급경로 행렬(n^4*5)


#파급효과 계산(단계별)  * 파급효과: 각 단계별 투입계수의 곱

V1 <- matrix(0,nrow=dim(T1)[1],ncol=1)
for (m in 1:dim(T1)[1]){
  V1[m,1] <- A[T1[m,2],T2[m,1]]
}

V2 <- matrix(0,nrow=dim(T2)[1],ncol=1)
for (m in 1:dim(T2)[1]){
  V2[m,1] <- A[T2[m,2],T2[m,1]]*A[T2[m,3],T2[m,2]]
}

V3 <- matrix(0,nrow=dim(T3)[1],ncol=1)
for (m in 1:dim(T3)[1]){
  V3[m,1] <- A[T3[m,2],T3[m,1]]*A[T3[m,3],T3[m,2]]*A[T3[m,4],T3[m,3]]
}

V4 <- matrix(0,nrow=dim(T4)[1],ncol=1)
for (m in 1:dim(T4)[1]){
  V4[m,1] <- A[T4[m,2],T4[m,1]]*A[T4[m,3],T4[m,2]]*A[T4[m,4],T4[m,3]]*A[T4[m,5],T4[m,4]]
}

#종합

V <- rbind(V1,V2,V3,V4)
x <- dim(V)

T <- matrix(0,ncol=5,nrow=x)

T[1:dim(V1)[1],1:2] <- T1

r2a<-1+dim(V1)[1]
r2z<-dim(V1)[1]+dim(V2)[1]
T[r2a:r2z,1:3] <- T2

r3a<-1+dim(V1)[1]+dim(V2)[1]
r3z<-dim(V1)[1]+dim(V2)[1]+dim(V3)[1]
T[r3a:r3z,1:4] <- T3

r4a<-1+dim(V1)[1]+dim(V2)[1]+dim(V3)[1]
r4z<-dim(V1)[1]+dim(V2)[1]+dim(V3)[1]+dim(V4)[1]
T[r4a:r4z,1:5] <- T4

final <- cbind(V,T) ## type : double
dtfm <- data.frame(final) ## type : data frame
colnames(dtfm)<-c("obj","s1","s2","s3","s4","s5")

#결과 내보내기
lookup<-arrange(dtfm,desc(obj))
lookup[1:100,]
```

    ##              obj s1 s2 s3 s4 s5
    ## 1   0.3442112525 17  2  0  0  0
    ## 2   0.1342739292 17 17  0  0  0
    ## 3   0.0462185973 17 17  2  0  0
    ## 4   0.0445473436 17  2 21  0  0
    ## 5   0.0347009549 17  6  0  0  0
    ## 6   0.0207454329 17  7  0  0  0
    ## 7   0.0181106983 17  6  2  0  0
    ## 8   0.0180294881 17 17 17  0  0
    ## 9   0.0158557798 17 26  0  0  0
    ## 10  0.0153447779 17 12  0  0  0
    ## 11  0.0145464749 17  2 26  0  0
    ## 12  0.0137477802 17  2 24  0  0
    ## 13  0.0119472901 17 24  0  0  0
    ## 14  0.0107329827 17  2 27  0  0
    ## 15  0.0104479491 17  2  7  0  0
    ## 16  0.0088980242 17 27  0  0  0
    ## 17  0.0087776323 17  2 13  0  0
    ## 18  0.0087469261 17  2 16  0  0
    ## 19  0.0082154086 17  2  6  0  0
    ## 20  0.0081063516 17  2 17  0  0
    ## 21  0.0076666289 17  2 21 21  0
    ## 22  0.0068589989 17  7  7  0  0
    ## 23  0.0068292031 17  2 21  6  0
    ## 24  0.0067220230 17 20  0  0  0
    ## 25  0.0065758377 17 11  0  0  0
    ## 26  0.0064830119 17  2 22  0  0
    ## 27  0.0062059527 17 17 17  2  0
    ## 28  0.0059815469 17 17  2 21  0
    ## 29  0.0059250837 17 10  0  0  0
    ## 30  0.0052984037 17  2 14  0  0
    ## 31  0.0050739481 17  2 10  0  0
    ## 32  0.0046661732 17  2 20  0  0
    ## 33  0.0046594336 17 17  6  0  0
    ## 34  0.0042876857 17  2  6  2  0
    ## 35  0.0040908926 17 13  0  0  0
    ## 36  0.0035734683 17 19  0  0  0
    ## 37  0.0035642142 17  2 21  6  2
    ## 38  0.0034543734 17  2  7  7  0
    ## 39  0.0029005341 17 12 12  0  0
    ## 40  0.0027902974 17  2 17  2  0
    ## 41  0.0027855708 17 17  7  0  0
    ## 42  0.0027814643 17 21  0  0  0
    ## 43  0.0025944924 17 22  0  0  0
    ## 44  0.0024453977 17 16  0  0  0
    ## 45  0.0024317946 17 17  6  2  0
    ## 46  0.0024208902 17 17 17 17  0
    ## 47  0.0023825166 17  2 21 14  0
    ## 48  0.0023784149 17  5  0  0  0
    ## 49  0.0023491172 17  2 21 27  0
    ## 50  0.0023438615 17  6  2 21  0
    ## 51  0.0022946055 17 11 11  0  0
    ## 52  0.0022677698 17  7  7  7  0
    ## 53  0.0022322166 17  7  6  0  0
    ## 54  0.0021511990 17  2 24 24  0
    ## 55  0.0021290179 17 17 26  0  0
    ## 56  0.0020604036 17 17 12  0  0
    ## 57  0.0019532123 17 17  2 26  0
    ## 58  0.0019128410 17  2 22  3  0
    ## 59  0.0018694654 17 24 24  0  0
    ## 60  0.0018459685 17 17  2 24  0
    ## 61  0.0018352481 17  2 13 13  0
    ## 62  0.0017428268 17  2 32  0  0
    ## 63  0.0016449508 17  2 23  0  0
    ## 64  0.0016063177 17  2 31  0  0
    ## 65  0.0016043984 17 12  9  0  0
    ## 66  0.0016042096 17 17 24  0  0
    ## 67  0.0015663032 17 25  0  0  0
    ## 68  0.0015071901 17  2 12  0  0
    ## 69  0.0014424808 17  2 14 14  0
    ## 70  0.0014411598 17 17  2 27  0
    ## 71  0.0014098485 17 23  0  0  0
    ## 72  0.0014028872 17 17  2  7  0
    ## 73  0.0013908404 17 26 23  0  0
    ## 74  0.0013686566 17  6  6  0  0
    ## 75  0.0013426287 17  9  0  0  0
    ## 76  0.0013194322 17  2 21 21 21
    ## 77  0.0012759905 17  2 26 23  0
    ## 78  0.0012027834 17 12  7  0  0
    ## 79  0.0011947727 17 17 27  0  0
    ## 80  0.0011786072 17 17  2 13  0
    ## 81  0.0011753106 17  2 21 21  6
    ## 82  0.0011744841 17 17  2 16  0
    ## 83  0.0011650831 17  2 19  0  0
    ## 84  0.0011650112 17  7  6  2  0
    ## 85  0.0011421089 17  2  7  7  7
    ## 86  0.0011242034 17  2  7  6  0
    ## 87  0.0011089296 17 10  9  0  0
    ## 88  0.0011031152 17 17  2  6  0
    ## 89  0.0010965405 17 18  0  0  0
    ## 90  0.0010929068 17  2  5  0  0
    ## 91  0.0010884717 17 17  2 17  0
    ## 92  0.0010884717 17  2 17 17  0
    ## 93  0.0010311756 17 12 11  0  0
    ## 94  0.0010294284 17 17  2 21 21
    ## 95  0.0009969500 17  2 18  0  0
    ## 96  0.0009958653 17  2 33  0  0
    ## 97  0.0009673285 17  2 30  0  0
    ## 98  0.0009654928 17  2  4  0  0
    ## 99  0.0009496323 17  2 10  9  0
    ## 100 0.0009209847 17 17  7  7  0

4-1. 생산유발계수의 2010년, 2015년 연도별 비교 그래프(2015년 기준년 산업연관표 작성결과 당행 보도자료 참조)
-----------------------------------------------------------------------------------------------------------

``` r
# 데이터 로딩, 한국은행 홈페이지 2015년 기준년 산업연관표 작성결과 통계요약표의 생산유발계수 sheet를 csv파일로 저장하여 이용 
production_inducement_tab <- read.csv('c:/Users/user/Documents/2019IO/data/production_inducement_tab.csv', header = T, stringsAsFactors = F)
```

    ## Warning in if (!header) rlabp <- FALSE: length > 1 이라는 조건이 있고, 첫번
    ## 째 요소만이 사용될 것입니다

    ## Warning in if (header) {: length > 1 이라는 조건이 있고, 첫번째 요소만이 사
    ## 용될 것입니다

``` r
# 데이터 개관
head(production_inducement_tab)
```

    ##           X.          X             X.1 X..1 X2000년.A. X..2 X2005년.B.
    ## 1 농림수산품                              ?       1.617   NA      1.733
    ## 2     광산품                              ?       1.617   NA      1.750
    ## 3 공 산 품2)                              ?       1.843   NA      1.904
    ## 4            소 비 재2)                   ?       2.040   NA      2.049
    ## 5                    ?        음식료품    ?       2.095   NA      2.078
    ## 6                    ?  섬유및가죽제품    ?       2.043   NA      2.037
    ##   X..3 X2010년.C. X..4 X2015년.D. X..5 차이.B.A. 차이.C.B. 차이.D.C.
    ## 1   NA      1.807   NA      1.799   NA     0.116     0.073    -0.007
    ## 2   NA      1.769   NA      1.836   NA     0.133     0.019     0.068
    ## 3   NA      1.921   NA      1.952   NA     0.061     0.017     0.030
    ## 4   NA      2.088   NA      2.069   NA     0.009     0.039    -0.019
    ## 5   NA      2.153   NA      2.163   NA    -0.017     0.075     0.010
    ## 6   NA      2.030   NA      1.961   NA    -0.006    -0.008    -0.069

``` r
# 데이터 정리
# 'X.','X','X.1' column을 통합하여 sector column을 생성
production_inducement_tab <- production_inducement_tab %>% unite(col = 'sector', X.,X,X.1, sep = "")  

# NA포함 column, 차이 변수 column을 제외하고 sector, 연도 column만을 선택 
production_inducement_tab <- production_inducement_tab %>% select(sector, starts_with('X20'))

# 연도 column의 이름 변경 
production_inducement_tab <- production_inducement_tab %>% rename('2000년' = X2000년.A.,
                                          '2005년' = X2005년.B.,
                                          '2010년' = X2010년.C.,
                                          '2015년' = X2015년.D.)

# 각주 행 삭제
production_inducement_tab <- production_inducement_tab[c(-45 : -48), ]

# sector column 산업 분류명 다듬기
production_inducement_tab$sector <- gsub('[?]', '', production_inducement_tab$sector) 
production_inducement_tab$sector <- gsub('2)', '', production_inducement_tab$sector)
production_inducement_tab$sector <- gsub('3)', '', production_inducement_tab$sector)

# 주요 산업 부분만 추리기
production_inducement_tab_major <- production_inducement_tab[c(2, 3, 11, 16, 17, 18, 21), ]

# 그래프 그리기 
ggplot(data = production_inducement_tab_major) +
  geom_point(mapping = aes(x = production_inducement_tab_major$'2010년', y = production_inducement_tab_major$'2015년', color = sector)) +
  xlab('2010년') + ylab('2015년') + coord_cartesian(xlim = c(1, 2.5), ylim = c(1, 2.5)) +
  geom_abline(intercept = 0, slope = 1, color = 'grey', size = 0.4, linetype = 'dashed')
```

![](total_files/figure-markdown_github/graph-1.png)

4-2. 부가가치유발계수의 2010년, 2015년 연도별 비교 그래프(2015년 기준년 산업연관표 작성결과 당행 보도자료 참조)
---------------------------------------------------------------------------------------------------------------

``` r
# 데이터 로딩, 한국은행 홈페이지 2015년 기준년 산업연관표 작성결과 통계요약표의 부가가치계수 sheet를 csv파일로 저장하여 이용 
add_value_tab <- read.csv('c:/Users/user/Documents/2019IO/data/add_value_tab.csv', header = T, stringsAsFactors = F)
```

    ## Warning in if (!header) rlabp <- FALSE: length > 1 이라는 조건이 있고, 첫번
    ## 째 요소만이 사용될 것입니다

    ## Warning in if (header) {: length > 1 이라는 조건이 있고, 첫번째 요소만이 사
    ## 용될 것입니다

``` r
# 'X.','X','X.1' column을 통합하여 sector column을 생성
add_value_tab <- add_value_tab %>% unite(col = 'sector', X.,X,X.1, sep = "")  

# NA포함 column, 차이 변수 column을 제외하고 sector, 연도 column만을 선택 
add_value_tab <- add_value_tab %>% select(sector, starts_with('X20'))

# 연도 column의 이름 변경 
add_value_tab <- add_value_tab %>% rename('2000년' = X2000년.A.,
                                          '2005년' = X2005년.B.,
                                          '2010년' = X2010년.C.,
                                          '2015년' = X2015년.D.)

# 각주 행 삭제
add_value_tab <- add_value_tab[c(-45 : -48), ]

# sector column 산업 분류명 다듬기
add_value_tab$sector <- gsub('[?]', '', add_value_tab$sector) # gsub function은 특정 문자열을 찾아 바꾸는 함수
add_value_tab$sector <- gsub('2)', '', add_value_tab$sector)
add_value_tab$sector <- gsub('3)', '', add_value_tab$sector)

# 주요 산업 부분만 추리기
add_value_tab_major <- add_value_tab[c(1, 5, 10, 11, 16, 18, 41), ]

# 그래프 그리기
ggplot(data = add_value_tab_major) +
  geom_point(mapping = aes(x = add_value_tab_major$'2010년', y = add_value_tab_major$'2015년', color = sector)) +
  xlab('2010년') + ylab('2015년') + coord_cartesian(xlim = c(0.4, 1), ylim = c(0.4, 1)) +
  geom_abline(intercept = 0, slope = 1, color = 'grey', size = 0.4, linetype = 'dashed')
```

![](total_files/figure-markdown_github/grpah2-1.png)
