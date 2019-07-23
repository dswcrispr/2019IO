## 초기 세팅

rm(list=ls())
setwd("c:/Users/user/Desktop/yeonsoo.lim/R/IO_analysis")

packages = c('tidyverse', 'XLConnect' )
lapply(packages, require, character.only = T)

# 투입산출표_생산자가격_통합대분류 로딩
raw <- loadWorkbook("c:/Users/user/Desktop/Yeonsoo.lim/R/IO_analysis/IO_tab_2015_major_cat_f.xlsx")

data <- readWorksheet(raw, sheet = 1, startRow = 7, startCol = 3, header = F)
# colname <- readWorksheet(raw, sheet = 1, startRow = 5, endRow = 5, startCol = 3, header = F)
colname_sector <- readWorksheet(raw, sheet = 1, startRow = 6, endRow = 6, startCol = 3, header = F) 

colnames(data) <- colname_sector

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

colSums(rbind(A, added_value_rate)) # 투입계수 + 부가가치율 = 1

# 생산유발계수 (1)형
I <- diag(x = 1, nrow = 33)
B <- solve(I - A)

# 생산유발계수 (2)형
m_star <- m / x
M_star <- diag(m_star, nrow = 33)
B_2 <-  solve(I - A + M_star)

# 생산유발계수 (4)형
data_d <- readWorksheet(raw, sheet = 3, startRow = 7, startCol = 3, header = F)
colname_d_sector <-  readWorksheet(raw, sheet = 3, startRow = 6, endRow = 6, startCol = 3, header = F)
colnames(data_d) <- colname_d_sector
data_d <- as_tibble(data_d)
data_d_1 <- select(data_d, '농림수산품' : '기타')
data_d_1 <- data_d_1[-34, ]
intermediate_demand_d <- matrix(unlist(data_d_1[-34, ]), nrow = 33, byrow = F)
A_d <- intermediate_demand_d / x_1 # A_d : 국산투입계수행렬
B_4 <- solve(I - A_d)

y_d <- select(data_d, "최종수요계")
y_d <- unlist(y_d[-c(34 : 40), ])

# 부가가치유발계수
A_v <- diag(added_value_rate, nrow = 33)
C_v <- A_v %*% B_4

# 수입유발계수
data_m <- readWorksheet(raw, sheet=2, startRow=7,startCol=3,endRow=39, endCol=35,header=FALSE)
intermediate_demand_m <- matrix(unlist(data_m),nrow=33,byrow=F)
A_m <- intermediate_demand_m/x_1 #A_m : 수입투입계수행렬
M <- A_m %*% B_4 # M: 수입유발계수행렬


#생산유발계수, 부가가치유발계수, 수입유발계수 관계

i <- matrix(1, ncol=33)
i%*%A_v%*%B_4+i%*%A_m%*%B_4

# 노동유발계수
# (1) 고용계수
hiring <- loadWorkbook("C:\\Users\\빵이\\Desktop\\r\\io\\부속표_고용표_통합대분류.xlsx")
hiring_1 <- readWorksheet(hiring, sheet=1, startRow=6,startCol=3,endRow=39,endCol=4,header=TRUE)
hiring_1 <- as_tibble(hiring_1)
l_e <- select(hiring_1,피용자수)
l_w <- select(hiring_1,취업자수)
l_e_star <- l_e/x #l_e_star : 고용계수

# (2) 취업계수
l_w_star <- l_w/x
E_W <- diag(unlist(l_w_star), nrow=33)%*%B_4 #취업유발계수

# (3) 노동계수(피용자 기준)
L_e <- diag(unlist(l_e_star),nrow=33)
E_e <- L_e%*%B_4

#최종수요 항목별 생산유발효과
Y_p <- B_4%*%y


