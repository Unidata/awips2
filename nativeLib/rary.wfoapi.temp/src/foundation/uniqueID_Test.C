//-------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// uniqueID_Test.C
//
// Test program for the routine getUniqueIdentifer.
//
// Author:  Gerry Murray
//-------------------------------------------------------------------

#include "UniqueID.H"
#include "Dict.H"

#include <iostream>
#include <stdlib.h>

int main (void)
    {
    // Try to generate a million unique IDs.  Check each
    // ID to see if it has already been generated.  If so,
    // then the unique ID routine is ineffective, so let the
    // user know a non-unique id was generated, and stop the program.
    // If the id has not been generated, then add it to the table.
    Dict<UniqueID,int> prevGeneratedIDs;
    for (int i = 0; i < 1000000; i++)
        {
        UniqueID uniqueID;
        int prevInstanceIndex;
        if (prevGeneratedIDs.map (uniqueID, prevInstanceIndex))
            {
            std::cout << "An id was generated twice." << std::endl
                 << "First Instance: " << prevInstanceIndex << std::endl
                 << "Second Instance: " << i << std::endl;
            exit (0);
            }
        else
            prevGeneratedIDs.add (uniqueID, i);
        }

    // We generated a million ids without a duplicate.  Report our success!
    std::cout << "Generated a million IDs without a duplicate.  Cool."
         << std::endl;
    }
