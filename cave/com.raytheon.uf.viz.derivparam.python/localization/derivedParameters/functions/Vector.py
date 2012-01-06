##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

from numpy import arctan2
from numpy import hypot
from numpy import cos
from numpy import sin
from numpy import degrees
from numpy import radians
from numpy import abs
from numpy import isscalar, empty, float32

def execute(u, v, legacyOption=None):
    """ Make a vector from u,v or mag, dir.
      
      @param legacyOption
                 controls how the function operates on the given u and v values.
                 When the legacyOption is specified and is a constant scalar value
                 the u and v input values are assumed to be speed and direction.

                 positive :: assume meteorological direction
                 negative :: assume mathematical direction
      
                 abs(legacyOption) == 1 :: assume degrees
                 abs(legacyOption) != 1 :: assume radians
                 
                 abs(legacyOption) == 1 || 2 :: assume first two args are speed and
                 direction
      
                 Otherwise if the legacy option is not a constant, the first two
                 inputs are components that determine the direction, and the third
                 is the speed to use.
      @return: tuple of (magnitude,direction,u,v)
    
    """
    # If either u or v is a single number, expand it to match an entire Grid
    if isscalar(u) and not(isscalar(v)):
        tmp = empty(v.shape, float32)
        tmp.fill(u)
        u = tmp
        
    if isscalar(v) and not(isscalar(u)):
        tmp = empty(u.shape, float32)
        tmp.fill(v)
        v = tmp
    
    # These will be parsed by legacy opts
    polar=False
    mathematicalDirection=False
    useRadians=False
    speed = None
    
    # Handle the crazy legacy options
    if legacyOption != None:
        if type(legacyOption) == float:
            mathematicalDirection = legacyOption < 0
            useRadians = abs(legacyOption) != 1
            polar = abs(legacyOption) == 1 or abs(legacyOption) == 2
        elif type(legacyOption) == bool:
            polar = legacyOption
        else:
            speed = legacyOption
    
    completeRevolution = 360 if not useRadians else radians(360.0)
    
    if (polar):
        mag = u 
        dir = v
        
        # replace all negative angles with their corresponding positive values
        negDirMask = dir < 0
        dir[negDirMask] = (- dir[negDirMask]) % completeRevolution
        dir[negDirMask] = completeRevolution - dir[negDirMask]
        
        dir[dir > completeRevolution] %= completeRevolution
        theta = radians(dir) if not useRadians else dir 
        u = mag * sin(theta)
        v = mag * cos(theta)
    else:
        u = - u if not mathematicalDirection else u
        v = - v if not mathematicalDirection else v
        mag = 0 if speed != None else hypot(u, v)
        theta = arctan2(u, v)
        dir = degrees(theta) if not useRadians else theta
        dir[dir < 0] += completeRevolution
    
    u = - u if not mathematicalDirection else u
    v = - v if not mathematicalDirection else v
    
    dir[dir == completeRevolution] = 0
    
    if speed == None:
        return (mag, dir, u, v)
    else:
        return execute(speed,dir,polar=True)

def test():
    
    from numpy import array
    
    angles = array([0., 45., 90., 135., 180., 225., 270., 315., 360., - 675., 405.])
    magnitudes = angles.copy()
    magnitudes[:] = 2
    
    targetDir = array([0., 45., 90., 135., 180., 225., 270., 315., 0., 45., 45.])
    targetU = array([ 0., 1.41, 2., 1.41, 0., - 1.41, - 2., - 1.41, - 0., 1.41, 1.41])
    targetV = array([ 2., 1.41, 0., - 1.41, - 2., - 1.41, 0., 1.41, 2., 1.41, 1.41])
    
    # perform regular mathematical vector tests
    
    (mag, dir, u, v) = execute(magnitudes, angles, polar=True, mathematicalDirection=True)
    
    if not (all(dir == targetDir) and all(u.round(2) == targetU) and all(v.round(2) == targetV)):
        print dir
        print targetDir
        print u.round(2)
        print targetU
        print v.round(2)
        print targetV
        raise Exception
    
    (mag, dir, u, v) = execute(u, v, mathematicalDirection=True)
    
    if not (all(dir.round(0) == targetDir) and all(mag == magnitudes)):
        raise Exception
    
    # perform meteorological vector tests
    
    (mag,dir,u,v) = execute(mag,dir,polar=True)
    
    if not (all(mag == magnitudes) and
            all(dir.round(0) == targetDir) and
            all(u.round(2) == -targetU) and
            all(v.round(2) == -targetV)):
        print mag
        print magnitudes
        print mag == magnitudes
        print dir
        print targetDir
        print dir == targetDir
        print -u.round(2)
        print targetU
        print u.round(2) == -targetU
        print -v.round(2)
        print targetV
        print v.round(2) == -targetV
        raise Exception
    
    (mag,dir,u,v) = execute(u,v)       
    
    if not (all(mag == magnitudes) and
            all(dir.round(0) == targetDir) and
            all(u.round(2) == -targetU) and
            all(v.round(2) == -targetV)):
        print mag
        print magnitudes
        print mag == magnitudes
        print dir
        print targetDir
        print dir.round(0) == targetDir
        print u.round(2)
        print -targetU
        print u.round(2) == -targetU
        print v.round(2)
        print -targetV
        print v.round(2) == -targetV
        raise Exception
    
    # make sure the goofy legacy option works as expected
    
    (mag,dir,u,v) = execute(mag,dir,1.0)
    
    if not(all(u.round(2) == -targetU) and
           all(v.round(2) == -targetV) and
           all(mag == magnitudes) and
           all(dir.round(0) == targetDir)):
        raise Exception
    
    (mag,dir,u,v) = execute(mag,dir,-1.0)
    
    if not(all(u.round(2) == targetU) and
           all(v.round(2) == targetV) and
           all(mag == magnitudes) and
           all(dir.round(0) == targetDir)):
        print mag,dir,u,v
        raise Exception
    
    otherMag = magnitudes.copy()
    otherMag[:] = 7
    (mag,dir,u,v) = execute(-targetU,-targetV,otherMag)
    
    if not(u.round(2)[0] == 0 and
           v.round(2)[0] == -7 and
           all(mag == otherMag) and
           all(dir.round(0) == targetDir)):
        print u.round(2)[0] == 0
        print v.round(2)[0] == -7
        print mag == otherMag
        print dir.round(0)
        print targetDir
        print dir.round(0) == targetDir
        raise Exception
    
    print "Vector Test Complete"