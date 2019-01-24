# Create nice R figures with minimal margins
# in landscape format suitable for slides and papers
savepdf <- function(file, width=16, height=10)
{
  fname <<- paste("figs/",file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54, pointsize=10, bg='white')
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}
# Crop pdf to remove all white space
endpdf <- function()
{
  #dev.off()
  crop::dev.off.crop(fname)
}

# Histograms
gghist <- function(data, mapping, ...)
{
  x <- data[[as.character(mapping$x[2])]]
  bw <- 0.2*bw.nrd0(x) + 0.8*bw.SJ(x)
  p <- ggplot(data, mapping) +
    geom_density(col=NA, fill="#cc5900", bw=bw)
  return(p)
}

# Function to produce very basic table, no lines or headings
baretable <- function(tbl, digits = 0,
                      include.colnames=FALSE, include.rownames=FALSE,
                      hline.after=NULL,
                      size = getOption("xtable.size", NULL),
                      add.to.row =  getOption("xtable.add.to.row", NULL),
                      ...) {
  tbl %>%
    xtable::xtable(digits = digits, ...) %>%
    print(
      include.colnames = include.colnames,
      include.rownames = include.rownames,
      hline.after = hline.after,
      comment = FALSE,
      latex.environments = NULL,
      floating = FALSE,
      size=size,
      add.to.row=add.to.row,
      sanitize.text.function = function(x) {
        x
      }
    )
}

