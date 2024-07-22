Pole <- function(trd,plg,k){
#Pole returns the pole to a plane or the plane which correspond to a pole
#
#   k is an integer that tells the program what to calculate.
#
#   If k = 0, [trd1,plg1] = Pole(trd,plg,k) returns the strike
#   (trd1) and dip (plg1) of a plane, given the trend (trd)
#   and plunge (plg) of its pole.
#
#   If k = 1, [trd1,plg1] = Pole(trd,plg,k) returns the trend
#   (trd1) and plunge (plg1) of a pole, given the strike (trd)
#   and dip (plg) of its plane.
#
#   NOTE: Input/Output angles are in radians. Input/Output strike
#   and dip are in right hand rule
#
#   Pole uses functions ZeroTwoPi, SphToCart and CartToSph
#
#MATLAB script written by Nestor Cardozo for the book Structural
#Geology Algorithms by Allmendinger, Cardozo, & Fisher, 2011. If you use
#this script, please cite this as "Cardozo in Allmendinger et al. (2011)"

out <- c(trd, plg)
#Some constants
east <- pi/2.0

#Calculate plane given its pole
if (k == 0){
  # trd = GMisc::rads(trd)
  # plg = GMisc::rads(plg)
    if (plg >= 0.0){
        out[2] <- east - plg
        dipaz <- trd - pi
    } else {
        out[2] <- east + plg
        dipaz <- trd
    }
    #Calculate trd1 and make sure it is between 0 and 2*pi
    out[1] <- ZeroTwoPi(dipaz - east)
    names(out) = c('strike','dip')
#Else calculate pole given its plane
} else if (k == 1){
    cned <- SphToCart(trd,plg,k)
    out <- CartToSph(cn = cned[1], ce = cned[2], cd = cned[3])
    names(out) = c('trd','plg')
}
  return(out)
}
