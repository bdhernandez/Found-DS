#load dataset 
data(diamonds)
#quick exploration of summary statistic for price by carat size
by(diamonds$price, diamonds$carat, summary)

#create price per carat variable for later use
price_per <- diamonds$price/diamonds$carat
summary(price_per)

#generate base-level box plots of price per carat by color
p1 <- qplot(x = color, y = price/carat, data = diamonds, geom = 'boxplot', 
            color = I('black'), fill = I('blue'),
            xlab = 'color', ylab= 'price per carat') +
  scale_y_continuous()
p1
ggsave('basebox.png')

#zoom in to IQR without losing data using coord_cartesian
p1 <- qplot(x = color, y = price/carat, data = diamonds, geom = 'boxplot',
            color = I('black'), fill = I('blue'),
            xlab = 'color', ylab= 'price per carat') +
  scale_y_continuous()+
  coord_cartesian(ylim = c(2000,6000))
p1
ggsave('zoombox.png')