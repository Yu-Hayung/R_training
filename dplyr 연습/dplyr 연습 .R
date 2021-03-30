 # Practical / "dply"r psckage 
 # https://www.listendata.com/2016/08/dplyr-tutorial.html#Important-dplyr-Functions-to-remember

library("dplyr")
mydata = read.csv("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv")
mydata <- tbl_df(mydata)

# sample_n의 데이터 프레임 (또는 테이블)에서 임의의 행 선택 기능. 함수의 두 번째 매개 변수는 R에게 선택할 행 수를 알려줍니다.
sample_n(mydata,3)

# sample_frac의 기능은 행의 무작위 N %를 반환합니다. 아래 예에서는 행의 10 %를 무작위로 반환합니다.
sample_frac(mydata,0.1)

# 별개의 기능 중복을 제거하는 데 사용됩니다.
x1 = distinct(mydata)
x1

# 변수를 기반으로 중복 행 제거
x2 = distinct(mydata, Index, .keep_all= TRUE)
x2

# 몇 개의 변수 만 선택하라는 요청을 받았다고 가정합니다. 아래 코드는 "State"에서 "Y2008"까지의 열인 "Index"변수를 선택합니다.
mydata2 = select(mydata, Index, State:Y2008)
mydata2

# 변수 앞 의 마이너스 기호 는 R이 변수를 삭제하도록 지시합니다.
mydata3 <- select(mydata, -Index, -State)
mydata3

mydata4 = select(mydata, -c(Index,State))
mydata4

# 'Y'로 시작하는 변수 선택 또는 삭제
mydata5 = select(mydata, starts_with("Y"))
mydata33 = select(mydata, -starts_with("Y"))

mydata6 = select(mydata, contains("I"))
mydata6 

# 변수 재정렬 아래 코드는 변수 'State' 를 앞에두고 나머지 변수는 그 뒤에 있습니다.
mydat7 = select(mydata, State, everything())
mydat7

# rename () 함수/ 변수 명을 변경하는데 사용됩니다.
# 다음 코드에서는 'Index' 변수의 이름 을 'Index1'으로 변경 합니다.
mydata8 = rename(mydata, Index1=Index)
mydata8

# filter () 함수/ 논리 조건이 일치하는 데이터를 서브 세트하는 데 사용됩니다.
# 데이터의 하위 집합이 필요하다고 가정합니다. 행을 필터링하고 Index가 A와 같은 값만 유지하려고합니다.
mydata9 = filter(mydata, Index == "A")
mydata9

# 다중 선택 기준 %in %의 오퍼레이터는 여러 개의 항목을 선택하는데 사용될 수있다. 
# 다음 프로그램에서 R에게 'Index'열의 'A'와 'C'에 대해 행을 선택하도록 지시합니다.
mydata10 = filter(mydata6, Index %in% c("A", "C"))
mydata10

# 'I'는 논리 조건에서 OR을 나타냅니다. 두 가지 조건 중 하나를 의미합니다.
mydata11 = filter(mydata, Index %in% c("A", "C") & Y2002 >= 1300000 )
mydata11

# "!" 기호는 논리 조건을 반전하는 데 사용됩니다.
mydata12 = filter(mydata, Index %in% c("A", "C") | Y2002 >= 1300000)

# CONTAINS 조건/ grepl 함수는 패턴 매칭을 검색하는 데 사용된다. 
# 다음 코드에서는 열 상태 에 이름에 'Ar'이 포함 된 레코드를 찾습니다 .
mydata13 = filter(mydata, grepl("Ar", State))

#  선택한 변수 요약
# 아래 예에서는 변수 Y2015에 대한 평균과 중앙값을 계산합니다.
summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))

# mean 및 median 함수에 대한 추가 인수 (예 : na.rm = TRUE )를 추가하려면 아래 코드와 같이 할 수 있습니다.
summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))
summarise_at(mydata, vars(Y2005, Y2006), list(n=~n(), mean=mean, median=median))
# 도트 (.) 함수의 두 번째 인수로 지정된 각 변수를 나타낸다.
summarise_at(mydata, vars(Y2005, Y2006), list(~n(), ~mean(.), ~median(.)))


summarise_at(mydata, vars(Y2011, Y2012),funs(mean, median), na.rm = TRUE)

# 모든 숫자 변수 요약
# summarise_if의 기능은 조건부로 요약 할 수 있습니다.
summarise_at(mydata, vars(Y2011, Y2012), funs(n(), missing = sum(is.na(.)), mean(., na.rm = TRUE), median(.,na.rm = TRUE)))

# 비표준 함수를 적용하는 방법
# 원래 값에서 평균을 뺀 다음 분산을 계산한다고 가정합니다.
set.seed(222)
mydata100 <- data.frame(X1=sample(1:100,100), X2=runif(100))
summarise_at(mydata100, vars(X1,X2), function(x) var(x - mean(x)))
summarise_at(mydata100, vars(X1,X2), ~ var(. - mean(.)))
summarise_if(mydata100, is.numeric, funs(n(),mean,median))


numdata1 <- mydata[sapply(mydata,is.numeric)]
numdata1

summarise_all(numdata1, funs(n(),mean,median))


summarise_all(mydata["Index"], funs(nlevels(.), nmiss=sum(is.na(.))))


arrange(mydata, Index, Y2011)


arrange(mydata, desc(Index), Y2011)

dt = mydata %>% select(Index, State) %>% sample_n(10)
dt

t = mydata %>% group_by(Index) %>% summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))
t

t = mydata %>% filter(Index %in% c("A", "C","I")) %>% group_by(Index) %>%  do(head( . , 2))
t

t = mydata %>% select(Index, Y2015) %>%  filter(Index %in% c("A", "C","I")) %>%  group_by(Index) %>% do(arrange(.,desc(Y2015))) %>%  slice(3)
t

mydata15 = mutate(mydata, change=Y2015/Y2014)


mydata16 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))


out = mydata %>% group_by(Index) %>% filter(min_rank(desc(Y2015)) == 1) %>%  select(Index, State, Y2015)


out2 = mydata %>% group_by(Index) %>% mutate(Total=cumsum(Y2015)) %>%  select(Index, Y2015, Total)


df1 = data.frame(ID = c(1, 2, 3, 4, 5),
                 w = c('a', 'b', 'c', 'd', 'e'),
                 x = c(1, 1, 0, 0, 1),
                 y=rnorm(5),
                 z=letters[1:5])
df2 = data.frame(ID = c(1, 7, 3, 6, 8),
                 a = c('z', 'b', 'k', 'd', 'l'),
                 b = c(1, 2, 3, 0, 4),
                 c =rnorm(5),
                 d =letters[2:6])

df3 = inner_join(df1, df2, by = "ID")

inner_join(df1, df2, by = c("ID"="ID"))

left_join(df1, df2, by = "ID")

mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]

intersect(first, second)

x=data.frame(ID = 1:6, ID1= 1:6)
y=data.frame(ID = 1:6,  ID1 = 1:6)
union(x,y)
union_all(x,y)

setdiff(first, second)

df <- c(-10,2, NA)
if_else(df < 0, "negative", "positive", missing = "missing value")

df =data.frame(x = c(1,5,6,NA))
df %>% mutate(newvar=if_else(x<5, x+1, x+2,0))

