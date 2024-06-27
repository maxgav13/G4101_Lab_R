Angles <- function(trd1,plg1,trd2,plg2,ans0){
#Angles calculates the angles between two lines, between two planes,
#the line which is the intersection of two planes, or the plane
#containing two apparent dips
#
#   [ans1,ans2] = Angles(trd1,plg1,trd2,plg2,ans0) operates on
#   two lines or planes with trend/plunge or strike/dip equal to
#   trd1/plg1 and trd2/plg2
#
#   ans0 is a character that tells the function what to calculate:
#
#       ans0 = 'a' -> the orientation of a plane given two apparent dips
#       ans0 = 'l' -> the angle between two lines
#
#       In the above two cases, the user sends the trend and plunge of two
#       lines
#
#       ans0 = 'i' -> the intersection of two planes
#       ans0 = 'p' -> the angle between two planes
#
#       In the above two cases the user sends the strike and dip of two
#       planes following the right hand rule
#
#   NOTE: Input/Output angles are in radians
#
#   Angles uses functions SphToCart, CartToSph and Pole
#
#MATLAB script written by Nestor Cardozo for the book Structural
#Geology Algorithms by Allmendinger, Cardozo, & Fisher, 2011. If you use
#this script, please cite this as "Cardozo in Allmendinger et al. (2011)"

#If planes have been entered
if (ans0 == 'i' | ans0 == 'p'){
    k <- 1
#Else if lines have been entered
} else if (ans0 == 'a' | ans0 == 'l'){
    k <- 0
}

#Calculate the direction cosines of the lines or poles to planes
res1 <- SphToCartD(trd1,plg1,k)
res2 <- SphToCartD(trd2,plg2,k)

#If angle between 2 lines or between the poles to 2 planes
if (ans0 == 'l' | ans0 == 'p'){
    # Use dot product = Sum of the products of the direction cosines
    ans1 <- (acos(res1$cn*res2$cn + res1$ce*res2$ce + res1$cd*res2$cd))*180/pi
    ans2 <- (pi)*180/pi - ans1
}

#If intersection of two planes or pole to a plane containing two
#apparent dips
if (ans0 == 'a' | ans0 == 'i'){
    #If the 2 planes or apparent dips are parallel return an error
    if (trd1 == trd2 && plg1 == plg2){
        stop('lines or planes are parallel')
    #Else use cross product
    } else {
        cn <- res1$ce*res2$cd - res1$cd*res2$ce
        ce <- res1$cd*res2$cn - res1$cn*res2$cd
        cd <- res1$cn*res2$ce - res1$ce*res2$cn
        #Make sure the vector points down into the lower hemisphere
        if (cd < 0.0){
            cn <- -cn
            ce <- -ce
            cd <- -cd
        }
        #Convert vector to unit vector by dividing it by its length
        r <- sqrt(cn*cn+ce*ce+cd*cd)
        # Calculate line of intersection or pole to plane
        res <- CartToSphD(cn/r,ce/r,cd/r)
        #If intersection of two planes
        if (ans0 == 'i'){
            ans1 <- res[1]
            ans2 <- res[2]
        #Else if plane containing two dips, calculate plane from its pole
        } else if (ans0 == 'a'){
            res <-  Pole(res[1]*pi/180,res[2]*pi/180,0)
            ans1 = res[1]*180/pi
            ans2 = res[2]*180/pi
        }
    }
}
  return(list(ans1=ans1,ans2=ans2))
}
