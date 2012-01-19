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

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12 Feb 2010     #4502         jelkins        Initial Creation.

class CPointer:
    """ Simple C Pointer class
    
    Helps track "pointers" for those functions that have been
    ported from C and have an insane amount of pointer logic
      
    """

    def __init__(self, target=None, position=0):
        """ Construct a CPointer
        
        @param target
                    value must be an object that assigns itself by 
                    reference such as lists or class instances and
                    must be subscriptable
        @param position
                    the offset from the initial value
        """
        self.target = target
        self.position = position
        
    def copy(self, other):
        """ Obtain a copy of another pointer
        """ 
        self.target = other.target
        self.position = other.position    
    def __iadd__(self, offset):
        """ Move the pointer position
        """
        self.position += offset
        return self
    def __isub__(self, offset):
        return self.__iadd__(- offset)
    def __getitem__(self, index):
        """ Get the value of the target at the given index
        """
        return self.target[self.position + index]
    def __setitem__(self, index, value):
        """ Set the value of the target at the given index
        """
        self.target[self.position + index] = value
    def __cmp__(self, other):
        """ Compare with another pointer
        """
        if id(self.target) == id(other.target):
            return self.position.__cmp__(other.position)
        else:
            return - 1

def test():
    """ Unit Test
    
    """
    
    object = [1, 2, 3]
    
    pointer = CPointer()
    pointer2 = CPointer()
    
    # pointer = &object
    pointer.target = object
    
    # pointer2 = pointer
    pointer2.copy(pointer)
    
    # pointer++
    pointer += 1
    
    if not(pointer > pointer2):
        raise Exception
    
    # *pointer2 = *pointer
    pointer2[0] = pointer[0]
    
    # *pointer += 4
    pointer[0] += 4
    
    if not(object == [2, 6, 3]):
        raise Exception
    
    # pointer[1] = 7
    pointer[1] = 7
    
    if not(object == [2, 6, 7] and pointer[1] == 7):
        raise Exception
    
    print "CPointer Test Complete"