File:       Nested Polygons
By:         Daniel B. Carr 
Date:       Oct. 19, 2013

Note:       For those interested in deeper understanding
            Nothing is due

Topic:      How R fills polygons with color.

		There are established ways to draw nested
            polygons. This include polygons with lakes
            (holes) and polygons with islands in lakes. 

            When the net traversal of a nested polygon
            boundary is clockwise R fills the region. 
            When the net traversal is counterclockwise 
            R does not fill the region.
        
            When polygon boundaries are stacked
            (separated by NAs) the the clockwise
            direction distinction does not apply.  
            R increases the index to the color vector
            for each polygon encountered.

Sections:   1. Constructing points for a unit circle
            2. Lake example
            3. Island in lake example  
            4. Stacked clockwise and counterclockwise polygons
               Direction doesn't matter
            5. Separate colors for stacked polygons
            6. R closes polygons 

1. Constructing points for a unit circle___________
 
## Run 

angle <- seq(0,2*pi,length=50) # clockwise

circleX <- cos(angle)
circleY <- sin(angle)

## End_____________________________________________

2. Lake example____________________________________

## Run

polygonX <- c(3*circleX, rev(circleX))
polygonY <- c(3*circleY, rev(circleY))

windows()
plot(polygonX,polygonY,type='n',las=1)
polygon(polygonX, polygonY, col="red",
  border=NA, density=-1) # A negative value mean fill
                         # the polygon with color
lines(3*circleX, 3*circleY, col='black')
lines(circleX,circleY,col='black')

## End____________________________________________

3.  Island in a lake example______________________

## Run

polygonX2 <- c(3*circleX, rev(circleX), .5*circleX)
polygonY2 <- c(3*circleY, rev(circleY), .5*circleY)

windows()
plot(polygonX2,polygonY2,type='n',las=1)
polygon(polygonX2, polygonY2, col="red",
  border=NA,density=-1)
lines(3*circleX,3*circleY,col='black')
lines(circleX,circleY,col='black')
lines(.5*circleX,.5*circleY,col="black")

## End_____________________________________________

4.  Stacked clockwise and counterclockwise_________
    polygon example

Since the polygons are stacked (separated by NAs)
and not nested R fills both polygons. 

## Run

polygonsX <- c(3*circleX,NA,rev(circleX))
polygonsY <- c(3*circleY,NA,rev(circleY))

windows()
plot(polygonsX,polygonsY,type='n',las=1)
polygon(polygonsX, polygonsY, col="red",
  border=NA,density=-1)

## End____________________________________________

5. Separate colors for stacked polygons___________

For stacked polygons R in increases an implicit
counting integer subscript to the color vector
when it encounters an NA in either of the polygon
x and y coordinate vectors. 

The length of the color vector can match the
number of polygons.  This allows us to draw a
whole choropleth map with a single call to polygon().
If the color vector is of length one, the color
will be recycle to match the number of polygons.    

## Run

polygonsX <- c(3*circleX, NA, rev(circleX))
polygonsY <- c(3*circleY, NA, rev(circleY))

windows()
myColors <- c("red", rgb(0,.5,1))


plot(polygonsX,polygonsY,type='n',las=1)
polygon(polygonsX, polygonsY, col=myColors,
   border=NA ,density=-1)

## End________________________________________

Does different focal lengths for red this
mostly blue color make it a bit difficult
to maintain focus?

6. R closes polygons__________________________ 

R will connect the last point of a polygon
to the first point in order to close a polygon. Some
of my scripts exploit this feature. However in the
nested polygon context we need to include the last
point of a circle to close the circle.    
 

