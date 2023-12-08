# pdf save ----------------------------------------------------------------

pdf_save = function(fig, 
                    path,
                    width,
                    height,
                    scale = 1.5,
                    pointsize = 12) {
  
  cairo_pdf(path,
            width = width * 0.393701 * scale,
            height = height * 0.393701 * scale,
            pointsize = pointsize)
  
  print( fig )
  
  dev.off()
  
}