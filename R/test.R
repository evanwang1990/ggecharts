df2 <- data.frame(
  saleNum=c(10,20,30,40,50,60,70,15,25,35,45,55,65,75,25,35,45,55,65,75,85),
  seller=c(rep("Yellow",7), rep("Red",7), rep("White",7)),
  weekDay = c(rep(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),3)),
  stringsAsFactors =FALSE
)

df2$weekDay <- factor(df2$weekDay, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), ordered = T)
t <- e_bar(df2, seller, saleNum, weekDay) +
  e_title("This is a test", "just a test @2017-03-22", position = c("top", "center")) +
  e_legend() +
  e_tooltip(enterable = T) +
  e_toolbox() +
  e_markPoint("averageX")

t <- e_grid(width = 4L, height = 4L, {
  e_bar(df2, seller, saleNum, weekDay) +
    e_title("This is a test", "just a test @2017-03-22", position = c("top", "center")) +
    e_legend() +
    e_tooltip(enterable = T) +
    e_toolbox() +
    e_markPoint("averageX")
})



th <- htmlwidgets::createWidget("echarts", t, package = "ggecharts", elementId = createElmemtID("shine"))


