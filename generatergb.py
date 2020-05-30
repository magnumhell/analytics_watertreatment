import matplotlib.colors as colors
import webcolors
list1=["#020845", "#090E4A", "#10154F", "#171C54", "#1E2359", "#252A5E", "#2C3164", "#333869", "#3A3E6E", "#414573", "#484C78", "#4F537D", "#565A83", "#5D6188", "#64688D", "#6B6E92", "#727597", "#797C9C", "#8083A2", "#878AA7", "#8E91AC", "#9598B1", "#9C9EB6", "#A3A5BB", "#AAACC1", "#B1B3C6", "#B8BACB", "#BFC1D0", "#C6C8D5", "#CDCEDA", "#D4D5E0", "#DBDCE5", "#E2E3EA", "#E9EAEF", "#F0F1F4", "#F7F8F9", "#FFFFFF"]
list2=["#FFFFFF", "#FFF7F7", "#FFF0F0", "#FFE9E9", "#FFE2E2", "#FFDBDB", "#FFD4D4", "#FFCDCD", "#FFC6C6", "#FFBFBF", "#FFB8B8", "#FFB1B1", "#FFAAAA", "#FFA2A2", "#FF9B9B", "#FF9494", "#FF8D8D", "#FF8686", "#FF7F7F", "#FF7878", "#FF7171", "#FF6A6A", "#FF6363", "#FF5C5C", "#FF5555", "#FF4D4D", "#FF4646", "#FF3F3F", "#FF3838", "#FF3131", "#FF2A2A", "#FF2323", "#FF1C1C", "#FF1515", "#FF0E0E", "#FF0707", "#FF0000"]
read="/Volumes/JORDAN 1/SUTD/Sembcorp/Correlation/rgblist.txt" 
	

for i in range(0,len(list1)-1):
	rgbstr=list1[i]
	rgb= webcolors.hex_to_rgb(rgbstr)
	print i,rgb
	with open(read,'a') as f:
		a,b,c=rgb
		str1=str(a)+','+str(b)+','+str(c)
		f.write(str1)
		f.write('\n')
# with open(read,'a') as f: