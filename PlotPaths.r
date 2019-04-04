#    create and plot a few binomial tree paths

n   = 5000        # number of steps in a path
mu  = .1
sig = .3
T   = 1
S0  = 100

dt  = T/n
times = 1:(n+1)
for ( k in 1:(n+1) ){
  times[k] = dt*(k-1)       # so t[1] = 0, and t[n+1] = t_n = T
}

p1 =  binPath(n, mu, sig, S0, T)
p2 =  binPath(n, mu, sig, S0, T)
p3 =  binPath(n, mu, sig, S0, T)
p4 =  binPath(n, mu, sig, S0, T)
p5 =  binPath(n, mu, sig, S0, T)


title    = sprintf("Binomial tree paths, T = %5.2f, sig = %5.2f, mu = %5.2f", 
                   T,         sig,         mu)
subtitle = sprintf("Using %4d time steps of size %8.2e", n, dt)

color1  = "black"        # colors of the lines
color2  = "darkgreen"    # there are lots of colors
color3  = "blue"    # there are lots of colors
color4  = "red"    # there are lots of colors
color5  = "orange"    # there are lots of colors

type1   = 1              # line types, the first is solid (type = 1), 
type2   = 2              # the second line is a pattern
type3   = 3              # the second line is a pattern
type4   = 4              # the second line is a pattern
type5   = 5              # the second line is a pattern


legand1 = sprintf("first path")
legand2 = sprintf("second path")
legand3 = sprintf("third path")
legand4 = sprintf("fourth path")
legand5 = sprintf("fifth path")

legends = c( legand1, legand2, 
             legand3, legand4, 
             legand5)             # put the values into arrays ... 
colors  = c( color1, color2,
             color3, color4,
             color5)       # ... for the legend() function
types   = c( type1, type2,
             type3, type4,
             type5)

plot( times, p1,
      main = title,                  #  title at the top of the plot
      sub  = subtitle,               #  subtitle at the bottom
      xlab = "time (years)",               #  label of the horizontal axis
      ylab = "stock price",  #  label of the vertical axis
      type = "o",                    #  get rid of the little circles
      lty  = type1,                  #  line style 
      col  = color1,                 #  line color
      ylim = c(S0*.5, S0*2),
      panel.first = grid())          #  make a grid on the plot

lines( times, p2,                        #  plot the second curve, 
       type = "o",
       lty = type2, col = color2)     #  line type and color 
lines( times, p3,                        #  plot the second curve, 
       type = "o",
       lty = type3, col = color3)     #  line type and color 
lines( times, p4,                        #  plot the second curve, 
       type = "o",
       lty = type4, col = color4)     #  line type and color 
lines( times, p5,                        #  plot the second curve, 
       type = "o",
       lty = type5, col = color5)     #  line type and color 

legend( "topleft",                  #  where the legend box goes.
        legends,                     #  the text of the legends
        col = colors,                #  corresponding line color and type
        lty = types)


plot( times, p1,
      main = title,                  #  title at the top of the plot
      sub  = subtitle,               #  subtitle at the bottom
      xlab = "time (years)",               #  label of the horizontal axis
      ylab = "stock price",  #  label of the vertical axis
      type = "o",                    #  get rid of the little circles
      lty  = type1,                  #  line style 
      col  = color1,                 #  line color
      ylim = c(S0*.5, S0*2),
      panel.first = grid())          #  make a grid on the plot

lines( times, p2,                        #  plot the second curve, 
       type = "o",
       lty = type2, col = color2)     #  line type and color 
lines( times, p3,                        #  plot the second curve, 
       type = "o",
       lty = type3, col = color3)     #  line type and color 
lines( times, p4,                        #  plot the second curve, 
       type = "o",
       lty = type4, col = color4)     #  line type and color 
lines( times, p5,                        #  plot the second curve, 
       type = "o",
       lty = type5, col = color5)     #  line type and color 

legend( "topleft",                  #  where the legend box goes.
        legends,                     #  the text of the legends
        col = colors,                #  corresponding line color and type
        lty = types)