library("ggplot2")
library("grid")
library("gridExtra")


line.plot <- function(data.list, type = 1) {
  summary.plt <- ggplot()
  
  for (pat.id in 1 : length(data.list)) {
    data.os.vfmd <- data.list[[pat.id]][, 1]
    data.od.vfmd <- data.list[[pat.id]][, 2]
    df.os.vfmd <- data.frame(index = 1 : length(data.os.vfmd), VFMD = data.os.vfmd)
    df.od.vfmd <- data.frame(index = 1 : length(data.od.vfmd), VFMD = data.od.vfmd)
    keep.index <- !is.na(df.os.vfmd$VFMD) & !is.na(df.od.vfmd$VFMD)
    df.os.vfmd <- df.os.vfmd[keep.index, ]
    df.od.vfmd <- df.od.vfmd[keep.index, ]
    
    data.os.iop <- data.list[[pat.id]][, 3]
    data.od.iop <- data.list[[pat.id]][, 4]
    df.os.iop <- data.frame(index = 1 : length(data.os.iop), IOP = data.os.iop)
    df.od.iop <- data.frame(index = 1 : length(data.od.iop), IOP = data.od.iop)
    keep.index <- !is.na(df.os.iop$IOP) & !is.na(df.od.iop$IOP)
    df.os.iop <- df.os.iop[keep.index, ]
    df.od.iop <- df.od.iop[keep.index, ]
    
    if (type == 1) { # Plot the VFMD and IOP for each patient
      data.plot.os.vfmd <- ggplot(data = df.os.vfmd, aes(x = index, y = VFMD)) + geom_line() + geom_point()
      data.plot.od.vfmd <- ggplot(data = df.od.vfmd, aes(x = index, y = VFMD)) + geom_line() + geom_point()
      data.plot.os.iop <- ggplot(data = df.os.iop, aes(x = index, y = IOP)) + geom_line() + geom_point()
      data.plot.od.iop <- ggplot(data = df.od.iop, aes(x = index, y = IOP)) + geom_line() + geom_point()
      ggsave(paste("patient_", pat.id, ".jpeg", sep = ""), 
             arrangeGrob(data.plot.os.vfmd, data.plot.od.vfmd, data.plot.os.iop, data.plot.od.iop))
    } else { # Plot the change of VFMD over time for all the patients in one figure
      summary.plt <- summary.plt + 
        # geom_line(data = df.os, aes(x = index, y = VFMD)) + geom_line(data = df.od, aes(x = index, y = VFMD)) +
        geom_point(data = df.os, aes(x = index, y = VFMD)) + geom_point(data = df.od, aes(x = index, y = VFMD))
    }
  }
  return(summary.plt)
}
