# File         spaceFillLayout
# By           Daniel B. Carr
# Copyright    2005, 2006, 2007 class use permitted
# Revised      2007,  reorder 4-tuples   xmin, xmax, ymin, ymax
#                      Indexed over rows instead of columns
#              More annotation
# Add          spaceFillDrawBox()
                                     
spaceFillLayout = function(clusSummary,xyPixels=c(1284,1024),drawHeight=-1){

# Input
# clusSummary      hclust() output list
# xyPixels     Target screen size for width and height  	
# drawHeight	 Which boxes have merge heights less than this 

# Output
# points           Locations for plotting the items
# pointsBoxes      Boxes containing the points: left, right, bottom, top
# boxes            Box sides for recursively splitting the space
#                  left, right, bottom, top
# drawBox          Logical to draw boxes for merges less than drawHeight 			

#1  compute vector of group sizes at the merges
 
mat = clusSummary$merge
n = nrow(mat)
clusSize = rep(0,n)
for(i in 1:n){
   j = mat[i,1]
   k = mat[i,2]  # if k <0  both are singletons
   # if 1 singleton it must be j   
   if(k < 0) clusSize[i] =   2 else    
      if(j < 0) clusSize[i] =  1+ clusSize[k] else
          clusSize[i] = clusSize[j]+clusSize[k]
}


#2 allocate storage 

n = nrow(mat)
node.rect = matrix(0,nrow=n,ncol=4)     # location for box sides
p.rect = matrix(0,nrow=n+1,ncol=4)      # box sides for points
p.point = matrix(0,nrow=n+1,ncol=2)     # points (x,y) average of box sides
dis = clusSummary$height
notdraw = rep(T,n)
contrast = c(-1,1,1,-1)
cont = matrix(c(.5,.5,0,0,0,0,.5,.5),nrow=2,byrow=T)
node.rect[n,] = c(1,xyPixels[1],1,xyPixels[2])

#3_______________main loop____________________

# y is  fifo stack
#     It starts with the final merge
#         There are n-1 merge pairs
#         n of these are single points
#         n-2 refer to former merges     
#     Each step 
#          Processes single points producing
#                Bounding box indexed by the points data order
#                Box center   indexed by the points data order
#          Stores bounding boxes for 0 to 2 constituent merge lines
#          index by the merge line(s)                


y = n             # y is a fifo stack
for (i in 1:n){
   node = y[1]          # merge matrix index
   leaf = mat[node,]    # get indices of the two merged clusters
   j = leaf[1]
   k = leaf[2]
   
   if(j < 0) jsize = 1 else jsize = clusSize[j]  # < 0 means a point index
   if(k < 0) ksize = 1 else ksize = clusSize[k]  # < 0 means a point index

   r = node.rect[node,]     # get rectangle for the cluster 
   r1 = r         # copy 1  of the four bounds
   r2 = r         # copy 2  of the four bounds
   y = y[-1]      # remove node from stack 
   if(dis[node] < drawHeight & notdraw[node])notdraw[node] = F  # height is below a bound


# maybe use total clusSize[node],
   splitRatio = jsize/(jsize+ksize)

   # below same as max(x)-min(x) - max(y) - min(y) > 0
   if(r%*%contrast > 0) {               # split  x
      r1[2] = r1[1]+ (r[2]-r[1])*splitRatio   # upper bound left box
      r2[1] = r1[2]                           # lower bound right box
   } else {                             # split y
     r1[4] = r1[3]+ (r[4]-r[3])*splitRatio    # upper bound lower box 
     r2[3] = r1[4]                            # lower bound upper box
   }

   if(jsize==1){
       j = abs(j)
       p.point[j,] = cont%*%r1                # average of x and y bounds
       if(notdraw[node]) p.rect[j,] = r1  
   } else {
       notdraw[j] = notdraw[node]
       node.rect[j,] = r1
   }

   if(ksize==1){
       k = abs(k)
       p.point[k,] = cont%*% r2
       if(notdraw[node]) p.rect[k,] = r2
   } else {
       notdraw[k] = notdraw[node]
       node.rect[k,] = r2
   }

   y = c(y,leaf[leaf>0])         
}

return(list(points=p.point,pointBoxes=p.rect,
            clusterBoxes=node.rect,merge.mat=mat,drawBox=!notdraw)) 	
}


spaceFillDrawBox = function(spaceFill,nclust=5,
   clusFill=NA,clusBorder="black",
   pointFill=NA,pointBorder="red"){

   rectFind = function(mat,nclust){
      nr=nrow(mat)
      nclust=min(nclust,nr)
      subs = mat[nr,]
      while(length(subs)<nclust){
         nr=nr-1
         subs=c(subs,mat[nr,])
         subs = subs[subs<nr]
      }
      return(subs)
   }

   subs= rectFind(spaceFill$merge.mat,nclust)

   if(any(subs>0)){
      csubs=abs(subs[subs>0])
      boxes = spaceFill$clusterBoxes
      rect(boxes[csubs,1],boxes[csubs,3],boxes[csubs,2],boxes[csubs,4],
      col=clusFill,border=clusBorder)
   }

   if(any(subs<0)){
      psubs=abs(subs[subs<0])
      boxes = spaceFill$pointBoxes
      rect(boxes[psubs,1],boxes[psubs,3],boxes[psubs,2],boxes[psubs,4],
      col=pointFill,border=pointBorder)
   }
}









