// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// setTest.C
// Set Class Test Program
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const setTest_C_Id =
 "$Id: .setTest.C__temp27950,v 1.2 2003/05/06 23:11:59 fluke Exp $";
#endif

// -- module ------------------------------------------------------------------
// The setTest program fully exercises the functions available
// in the Set class.
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>
#include "commonDefs.h"
#include "Set.H"
#include "SetP.H"
#include "TextString.H"

bool setTest();
bool setPtrTest();

// -- global ------------------------------------------------------------------
// main()
// Main driver for testing set classes.  Program returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
int main()
    {
    std::cout << "Set Test Program " << std::endl;

    std::cout << "Testing Float set class" << std::endl;
    if(!setTest())
        {
        std::cout << "..Set test failed" << std::endl;
        exit(1);
        }
    
    std::cout << "Testing pointer dictionary class" << std::endl;
    if (!setPtrTest())
        {
        std::cout << "..Ptr Dictionary test failed" << std::endl;
        }

    std::cout << "Testing Completed Successfully" << std::endl;
    exit(0);
    }


// -- global ------------------------------------------------------------------
// setTest()
// Tests set containing floats.  Returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
bool setTest()
    {
    Set<TextString> fs;

    // check size
    if (fs.size() != 0)
        {
        std::cout << "Non-zero size() returned on zero size set" << std::endl;
        return false;
        }

    // assignment operator on empty set
    Set<TextString> fs1;
    fs1 = fs;
    if (fs1.size() != 0)
        {
        std::cout << "operator= fail on empty set" << std::endl;
        return false;
        }

    // copy constructor on empty set
    Set<TextString> fs2(fs1);
    if (fs2.size() != 0)
        {
        std::cout << "copy constructor fail on empty set" << std::endl;
        return false;
        }

    // try to find a key with an empty set
    if (fs.map("AKey"))
        {
        std::cout << "map() incorrect when key does not exist" << std::endl;
        return false;
        }

    // add elements
    fs.emptySet();
    if ( fs.size() != 0 )
        {
        std::cout << "size incorrect after emptyset" << std::endl;
        return false;
        }
    
    fs.add("Zero");
    fs.add("One");
    fs.add("Two");
    fs.add("Three");
    fs.add("Four");
    fs.add("Five");
    fs.add("Six");
    fs.add("Seven");
    fs.add("Eight");
    fs.add("Nine");
    fs.add("Ten");
    // check size
    if (fs.size() != 11)
        {
        std::cout << "size incorrect after adding elements" << std::endl;
        }

    // check for existing key and get value
    if ( !fs.map("Seven") )
        {
       std::cout << "map failing" << std::endl;
       return false;
       }

    // removal test
    fs.remove("Nineteen");
    if (fs.size() != 11)
        {
        std::cout << "size() incorrect after attempted removal of non-existant"
          << std::endl;
        return false;
        }
    TextString nine("Nine"), ten("Ten");
    fs.remove(nine);
    if (fs.size() != 10 || fs.map(nine))
        {
        std::cout << "remove() fail on key that exists" << std::endl;
        return false;
        }
    
    if ( !fs.map(ten) )
        {
        std::cout << "map() or data incorrect after add() modification" 
             << std::endl;
        return false;
        }

    fs1.emptySet();
    fs1.add( "thirty");
    fs1.add( "thirty one" );
    fs1.add( "thirty two" );
    fs1.add( "thirty three" );
    fs1.add( "thirty four" );
    fs1.add( "thrity five" );

    if ( fs.mutuallyExclusive( fs1 ) == false )
        {
        std::cout << "test for mutually exclusiveness failing" << std::endl;
        return false;
        }

    if ( fs.mutuallyInclusive( fs1 ) == true )
        {
        std::cout << "test for mutually inclusuveness failing" << std::endl;
        return false;
        }

    fs1.setUnion( fs );
    if ( fs1.isRhsSubset( fs ) == false )
        {
        std::cout << "rhs is not a subset of fs1" << std::endl;
        return false;
        }

    if ( fs1.isRhsProperSubset( fs ) == false )
        {
        std::cout << "rhs is not a proper subset of fs1" << std::endl;
        return false;
        }

    if ( ( fs1.mutuallyExclusive( fs ) == true ) ||
         ( fs1.mutuallyInclusive( fs ) == true ) )
        {
        std::cout << "Test for both mutually exclusive and inclusive failing"
             << std::endl;
        return false;
        }

    fs1.setIntersection( fs );

    if ( fs.isRhsSubset( fs1 ) == false )
        {
        std::cout << "setIntersection failing" << std::endl;
        return false;
        }

    fs1.add( "thirty");
    fs1.add( "thirty one" );
    fs1.add( "thirty two" );
    fs1.add( "thirty three" );
    fs1.add( "thirty four" );
    fs1.add( "thrity five" );

    fs1.setMinus( fs );
    if ( fs1.mutuallyExclusive( fs ) == false )
        {
        std::cout << "test for set minus failing" << std::endl;
        return false;
        }

    return true;
    }



// -- global ------------------------------------------------------------------
// setPtrTest()
// Tests set containing pointers.  Returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
bool setPtrTest()
    {
    SetP<TextString*> fs, fs1;

    // check size
    if (fs.size() != 0)
        {
        std::cout << "Non-zero size() returned on zero size set" << std::endl;
        return false;
        }

    // assignment operator on empty set
    fs1 = fs;
    if (fs1.size() != 0)
        {
        std::cout << "operator= fail on empty set" << std::endl;
        return false;
        }

    // copy constructor on empty set
    SetP<TextString*> fs2(fs1);
    if (fs2.size() != 0)
        {
        std::cout << "copy constructor fail on empty set" << std::endl;
        return false;
        }

    // try to find a key with an empty set
    TextString* akey = new TextString( "AKey" );
    if ( fs.map( akey ) )
        {
        std::cout << "map() incorrect when key does not exist" << std::endl;
        return false;
        }
    delete akey;

    // add elements
    fs.emptySet();
    if ( fs.size() != 0 )
        {
        std::cout << "size incorrect after emptyset" << std::endl;
        return false;
        }
    TextString* zero     = new TextString( "Zero" );
    TextString* one      = new TextString( "One" );
    TextString* two      = new TextString( "Two" );
    TextString* three    = new TextString( "Three" );
    TextString* four     = new TextString( "Four" );
    TextString* five     = new TextString( "Five" );
    TextString* six      = new TextString( "Six" );
    TextString* seven    = new TextString( "Seven" );
    TextString* eight    = new TextString( "Eight" );
    TextString* nine     = new TextString( "Nine" );
    TextString* ten      = new TextString( "Ten" );
    TextString* nineteen = new TextString( "nineteen" );
    
    fs.add( zero );
    fs.add( one );
    fs.add( two );
    fs.add( three );
    fs.add( four );
    fs.add( five );
    fs.add( six );
    fs.add( seven );
    fs.add( eight );
    fs.add( nine );
    fs.add( ten );

    // check size
    if (fs.size() != 11)
        {
        std::cout << "size incorrect after adding elements" << std::endl;
        }

    // check for existing key and get value
    if (!fs.map( seven ))
        {
       std::cout << "map failing" << std::endl;
       return false;
       }


    // removal test
    fs.remove( nineteen );
    if (fs.size() != 11)
        {
        std::cout << "size() incorrect after attempted removal of non-existant"
          << std::endl;
        return false;
        }

    fs.remove( nine );
    if (fs.size() != 10 || fs.map(nine))
        {
        std::cout << "remove() fail on key that exists" << std::endl;
        return false;
        }

    if ( !fs.map( ten ) )
        {
        std::cout <<"map() or data incorrect after add(new Textstring) modification" 
            << std::endl;
        return false;
        }

    fs1.emptySet();
    TextString* thirty      = new TextString( "Thirty" );
    TextString* thirtyone   = new TextString( "Thirtyone" );
    TextString* thirtytwo   = new TextString( "Thirtytwo" );
    TextString* thirtythree = new TextString( "Thirtythree" );
    TextString* thirtyfour  = new TextString( "Thirtyfour" );
    TextString* thirtyfive  = new TextString( "Thirtyfive" );

    fs1.add( thirty );
    fs1.add( thirtyone );
    fs1.add( thirtytwo );
    fs1.add( thirtythree );
    fs1.add( thirtyfour );
    fs1.add( thirtyfive );


    if ( fs1.size() != 6 )
        {
        std::cout << "fs1's size is incorrect." << std::endl;
        return false;
        }

    if ( fs.mutuallyExclusive( fs1 ) == false )
        {
        std::cout << "test for mutually exclusiveness failing" << std::endl;
        return false;
        }

    if ( fs.mutuallyInclusive( fs1 ) == true )
        {
        std::cout << "test for mutually inclusuveness failing" << std::endl;
        return false;
        }

    fs1.setUnion( fs );

    if ( fs1.isRhsSubset( fs ) == false )
        {
        std::cout << "rhs is not a subset of fs1" << std::endl;
        return false;
        }

    if ( fs1.isRhsProperSubset( fs ) == false )
        {
        std::cout << "rhs is not a proper subset of fs1" << std::endl;
        return false;
        }

    if ( ( fs1.mutuallyExclusive( fs ) == true ) ||
         ( fs1.mutuallyInclusive( fs ) == true ) )
        {
        std::cout << "Test for both mutually exclusive and inclusive failing"
             << std::endl;
        return false;
        }
            
    fs1.setIntersection( fs );
    
    if ( !( fs == fs1 ) )
        {
        std::cout << "setIntersection failing" << std::endl;
        return false;
        }

    fs1.add( thirty );
    fs1.add( thirtyone );
    fs1.add( thirtytwo );
    fs1.add( thirtythree );
    fs1.add( thirtyfour );
    fs1.add( thirtyfive );

    fs1.setMinus( fs );
    if ( fs1.mutuallyExclusive( fs ) == false )
        {
        std::cout << "test for set minus failing" << std::endl;
        return false;
        }

    delete zero;     
    delete one;     
    delete two;   
    delete three;    
    delete four;    
    delete five;     
    delete six;   
    delete seven;   
    delete eight;    
    delete nine;     
    delete ten;
    delete nineteen;


    delete thirty;
    delete thirtyone;
    delete thirtytwo;
    delete thirtythree;
    delete thirtyfour;
    delete thirtyfive;
        
    return true;
    }


