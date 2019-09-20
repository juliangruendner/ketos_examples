################################################################
# plot_wrapper.r                                               #
# farhad: width and height are changed                         #
# 12.02.2017 (wo) close_plot now without width/height          #
#                                                              #
################################################################
open_plot<-
function (graph_format,file,width,height) {
if (graph_format == "OSM") {
   pdf(file=paste(file,"pdf",sep="."),width=7,height=5)
   } else {
   windows(width=width,height=height)
   }
}

close_plot<-
function (graph_format,file) {
if (graph_format == "OSM") {
   dev.off()
   } else {
   savePlot(filename = file,type = graph_format)
   }
}
