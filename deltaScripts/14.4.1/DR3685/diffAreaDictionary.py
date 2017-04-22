#!/usr/bin/env python

import imp, sys

DICT_KEYS = ['fullStateName', 
            'partOfState', 
            'stateAbbr',
            'ugcCityString',
            'ugcCode',
            'ugcName',
            'ugcTimeZone',
            ]

def diffCities(cityStringA, cityStringB):
    cityListA = cityListB = []
    cityListAMixed = cityListBMixed = []
    
    if cityStringA is not None:
        cityListAMixed = cityStringA.strip('.').split('...')
        cityListA = cityStringA.strip('.').upper().split('...')
    if cityStringB is not None:
        cityListBMixed = cityStringB.strip('.').split('...')
        cityListB = cityStringB.strip('.').upper().split('...')
    
    added = []
    for city in set(cityListB).difference(set(cityListA)):
        added.append(cityListBMixed[cityListB.index(city)])

    removed = []
    for city in set(cityListA).difference(set(cityListB)):
        removed.append(cityListAMixed[cityListA.index(city)])
        
    if len(added) > 0:
        print "    added cities:", list(added)
    if len(removed) > 0:
        print "  removed cities:", list(removed)

def printEntry(dict):
    for key in DICT_KEYS:
        if dict.has_key(key):
            print "   ",key+':',dict[key]

def main():
    if len(sys.argv) < 3:
        print "Usage:"
        print sys.argv[0], "fileA, fileB"
        print "    fileA: path to old AreaDictionary.py file"
        print "    fileB: path to new AreaDictionary.py file"
        print "Example:"
        print sys.argv[0], "/awips2/edex/data/utility/cave_static/site/OAX/gfe/userPython/textUtilities/regular/AreaDictionary.py /awips2/edex/data/utility/cave_static/configured/OAX/gfe/userPython/textUtilities/regular/AreaDictionary.py"
        sys.exit(1)
    
    modA = imp.load_source('modA', sys.argv[1])
    modB = imp.load_source('modB', sys.argv[2])
    
    dictA = modA.AreaDictionary
    dictB = modB.AreaDictionary
    
    keys = set() 
    keys.update(dictA.keys())
    keys.update(dictB.keys())
    keys = list(keys)
    keys.sort()
    
    for key in keys:
        if not dictA.has_key(key):
            print '\n'+key+": added"
            printEntry(dictB[key])
        elif not dictB.has_key(key):
            print '\n'+key+": removed"
            printEntry(dictA[key])
        else:
            differs = False
            dataA = dictA[key]
            dataB = dictB[key]
            for key1 in DICT_KEYS:
                valueA = valueB = None
                
                if dataA.has_key(key1):
                   valueA = dataA[key1]

                if dataB.has_key(key1):
                   valueB = dataB[key1]
                
                if str(valueA).upper() != str(valueB).upper():
                    if not differs:
                        differs = True
                        print '\n'+key+": differs"

                    if key1 == 'ugcCityString':
                        diffCities(valueA, valueB)
                    else:
                        print "   ", key1,"old:", valueA
                        print "   ", " "*len(key1),"new:", valueB
    
if __name__ == "__main__":
    main()