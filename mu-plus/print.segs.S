"print.segs"<- function(segs)
{
  cat("Segment Level:", attributes(segs)$level, "\n")
  print.matrix(segs)
  invisible()
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
