library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)

# Import data and do basic filter
# Here I just copy professor's code
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)
states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dest= rep("", 52)
for(i in 1:52){ 
  dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
}
x16 = ldply(dest, fread, colClasses = classes)  
M = x16
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029"           ,      "YEAR_ADT_030" ,
         "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090"   ,  "FRACTURE_092A"     ,      "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C"  )
x = M[,match(keep, colnames(M))]


# map of CA
cal = filter(x, STATE_CODE_001 == 06)
cal = filter(cal, LAT_016 > 1e+07)
cal = filter(cal, LONG_017 > 0)
ggplot(data = cal) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))
min2dec = function(x){
  substr(x,4,9) %>% return
}
cal$LONG_017 %>% min2dec %>% as.numeric %>% hist
min2dec = function(x){
  as.numeric(substr(x,1,3)) + as.numeric(substr(x,4,9))/6e+05 %>% return
}
min2dec(cal$LONG_017[1])
cal$LONG_017 %>% min2dec %>% as.numeric %>% hist
min2dec.la = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
cal = mutate(cal,lat = min2dec.la(LAT_016), lon = min2dec(LONG_017))
ggplot(data = cal) +geom_point(mapping = aes(y = lat, x = lon))
cal = filter(cal, lat < 50)
ggplot(data = cal) +geom_point(mapping = aes(y = lat, x = -lon))

# serveral initial plots

ggplot(data = cal) +geom_point(mapping = aes(y = lat, x = -lon, col =YEAR_BUILT_027))
ggplot(data = cal) +geom_point(mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027))
ggplot(data = cal, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = SUPERSTRUCTURE_COND_059)) +geom_point() + geom_smooth(method = "loess", span = .7)

# figure out where busy bridges are in CA.

(quantile(cal$ADT_029,probs = 0.75))
cal.busy = filter(cal, ADT_029 >= 23000)
ggplot(data = cal.busy) +geom_point(mapping = aes(y = lat, x = -lon, col =YEAR_BUILT_027))
# according to plot, we could clearly know busy bridges are distributed around several big citys,
# such as LA and SF, and busy highways, such as #5 and #99, which are at center area of the plot.


# rating based on standard below
# "DECK_COND_058" "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060" "CHANNEL_COND_061" "CULVERT_COND_062"  
# good = 5:9
# bad = 2:4
# fail = 0:1

cal = mutate(cal, cond = pmin(DECK_COND_058, SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                            na.rm = T))
rateIt = function(cond){
  rate = rep("good", length(cond))
  rate[cond <5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}
cal$rate = rateIt(cal$cond)
table(cal$cond)
table(cal$rate)

# Repeat same steps on busy bridges.
cal.busy = mutate(cal.busy, cond = pmin(DECK_COND_058, SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                              na.rm = T))
cal.busy$rate = rateIt(cal.busy$cond)
table(cal.busy$cond)
table(cal.busy$rate)
# we could know "good" ratio of busy bridges is significantly higher than normal bridges.

cal %>% group_by(YEAR_BUILT_027) %>% summarize(prop = mean(rate=="good")) %>%
  ggplot(mapping = aes(x = YEAR_BUILT_027, y = prop)) + geom_point()
cal %>% group_by(ADT_029) %>% summarize(prop = mean(rate=="good")) %>%
  ggplot(mapping = aes(x = ADT_029, y = prop)) + geom_point()

map = ggplot(data = cal, mapping = aes(y = lat, x = -lon))
map + geom_point(aes(col=rate))+ scale_colour_brewer(palette = "Spectral")  

ggplot(data = cal, mapping = aes(x = rate, y = log(ADT_029))) + geom_boxplot()
