
stripRemover <- function(ggp, what="x") {
  require(gridExtra)
  
  zeroGrob <- function() {
    g0 <- grob(name="NULL")
    class(g0) <- c("zeroGrob",class(g0))
    g0
  }
  
  g <- ggplotGrob(ggp)
  
  g$grobs <- lapply(g$grob, function(gr) {
    if (any(grepl(paste0("strip.text.", what),names(gr$children)))) {
      gr$children[[grep("strip.background",names(gr$children))]] <- zeroGrob()
      gr$children[[grep("strip.text",names(gr$children))]] <- zeroGrob()
    }
    return(gr)
  }
  )
  
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}
