

q <- read.csv2(file= "dataset/Vazao_64215080.csv") %>%
mutate(
    Data = as.Date(Data, format = "%d/%m/%Y"),
    Year = format(Data, format="%Y")) %>%
group_by(Year)
head()
unique(q$Year)
