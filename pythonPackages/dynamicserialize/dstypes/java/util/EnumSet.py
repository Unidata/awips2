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

##
# NOTE: Please do not ever use this class unless you really must. It is not
# designed to be directly accessed from client code. Hide its use from end-
# users as best as you can.
##

##
# IMPLEMENTATION DETAILS:
# This class is an attempt to simulate Java's EnumSet class. When creating
# a new instance of this class, you must specify the name of the Java enum
# contained within as this is needed for serialization. Do not append the
# "dynamicserialize.dstypes" portion of the Python package to the supplied
# class name as Java won't know what class that is when deserializing. 
#
# Since Python has no concept of enums, this class cannot provide the value-
# checking that Java class does. Be very sure that you add only valid enum
# values to your EnumSet. 
##

import collections


class EnumSet(collections.MutableSet):
    
    def __init__(self, enumClassName, iterable=[]):
        self.__enumClassName = enumClassName
        self.__set = set(iterable)
        
    def __repr__(self):
        return "EnumSet({0})".format(list(self.__set))

    def __len__(self):
        return len(self.__set)
    
    def __contains__(self, key):
        return key in self.__set
    
    def __iter__(self):
        return iter(self.__set)
    
    def add(self, value):
        self.__set.add(value)
    
    def discard(self, value):
        self.__set.discard(value)
        
    def getEnumClass(self):
        return self.__enumClassName
