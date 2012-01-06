// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// dictOfSetTest.C
// DictOfSet Class Test Program
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const dictOfSetTest_C_Id =
 "$Id: .dictOfSetTest.C__temp27950,v 1.2 2003/05/06 23:11:56 fluke Exp $";
#endif

// -- module ------------------------------------------------------------------
// The dictOfSetTest program fully exercises the functions available
// in the DictOfSSet class.
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>
#include "commonDefs.h"
#include "DictOfSet.H"
#include "DictOfSetKP.H"
#include "DictOfSetPV.H"
#include "DictOfSetPP.H"
#include "TextString.H"
#include "Set.H"
#include "SetP.H"

bool dictOfSetTest();
bool dictOfSetKPTest();
bool dictOfSetPVTest();
bool dictOfSetPPTest();


// -- global ------------------------------------------------------------------
// main()
// Main driver for testing DictOfSet classes.  Program returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
int main()
    {
    std::cout << "DictOfSet Test Program " << std::endl;

    std::cout << "Testing..." << std::endl;
    std::cout << "Testing DictOfSet." << std::endl;
    if(!dictOfSetTest())
        {
        std::cout << "DictOfSet test failed" << std::endl;
        exit(1);
        }
    std::cout << "Testing DictOfSetKP." << std::endl;
    if(!dictOfSetKPTest())
        {
        std::cout << "DictOfSetKP test failed" << std::endl;
        exit(1);
        }
    std::cout << "Testing DictOfSetPV" << std::endl;
    if(!dictOfSetPVTest())
        {
        std::cout << "DictOfSetPV test failed" << std::endl;
        exit(1);
        }
    std::cout << "Testing DictOfSetPP." << std::endl;
    if(!dictOfSetPPTest())
        {
        std::cout << "DictOfSetPP test failed" << std::endl;
        exit(1);
        }
    
    std::cout << "Testing Completed Successfully" << std::endl;
    exit(0);
    }


// -- global ------------------------------------------------------------------
// dictOfSetTest()
// Tests DictOfSet containing TextStrings.  Returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
bool dictOfSetTest()
    {
    DictOfSet<TextString,TextString> fs;

    // check size
    if (fs.size() != 0)
        {
        std::cout << "Non-zero size() returned on zero size dictOfSet" << std::endl;
        return false;
        }

    // assignment operator on empty set
    DictOfSet<TextString,TextString> fs1;
    fs1 = fs;
    if (fs1.size() != 0)
        {
        std::cout << "operator= fail on empty DictOfSet" << std::endl;
        return false;
        }

    // copy constructor on empty set
    DictOfSet<TextString,TextString> fs2(fs1);
    if (fs2.size() != 0)
        {
        std::cout << "copy constructor fail on empty DictOfSet" << std::endl;
        return false;
        }

    // try to find a key with an empty set
    if (fs.map("AKey") || fs.map("AKey","AKey"))
        {
        std::cout << "map() incorrect when key does not exist" << std::endl;
        return false;
        }

    // Add elements
    fs.add("Zero","Zero");
    fs.add("One","One");
    fs.add("Two","Two");
    fs.add("Three","Three");
    fs.add("Four","Four");
    fs.add("Five","Five");
    fs.add("Six","Six");
    fs.add("Seven","Seven");
    fs.add("Eight","Eight");
    fs.add("Nine","Nine");
    fs.add("Ten","Ten");

    
    // check size
    if ((fs.size() != 11) || (fs.size("Zero") != 1))
        {
        std::cout << "size incorrect after adding elements" << std::endl;
        }

    // Checking assignment constructor on a set with elements
    fs2 = fs;
    if ( ( fs2.size() != 11 ) || ( fs2.size( "Zero" ) != 1 ) )
        std::cout << "Size incorrect after assignment constructor on a set with"
             << " elements." << std::endl;



    // Checking copy constructor on a set with elements
    DictOfSet<TextString,TextString> fs3(fs);
    if ( ( fs3.size() != 11 ) || ( fs3.size( "Zero" ) != 1 ) )
        std::cout << "Size incorrect after copy constructor on a set with"
             << " elements." << std::endl;


    // Trying remove with just a key
    fs3.remove( "Ten" );
    if ( fs3.size() != 10 )
        {
        std::cout << "Removal of set failing." << std::endl;
        }


    
    // check for existing key and get value
    if ( !fs.map( "Seven" ) ||  !fs.map( "Seven","Seven" ) )
        {
        std::cout << "map failing" << std::endl;
        return false;
        }

    Set<TextString> set;
    
    // Checking map with a Set object
    if ( !fs.map( "Seven", set ) && !set.map( "Seven" ) )
        {
        std::cout << "Map failing with a set. " << std::endl;
        }

    // Order by K test
    fs.orderByK();

    // Key test
    TextString ts1 = fs.key( 1 );
    if ( ts1 != "Five" )
        std::cout << "Error in key test." << std::endl;

    // Value tests
    set = fs.value( 1 );

    if ( ( set[0] != ts1 ) || ( fs.value( "Five", 0 ) != ts1 ) )
        std::cout << "Error in value tests." << std::endl;


    // Trying to add a set
    fs3.add( "Test", set );
    if ( ( fs3.size() != 11 ) || !fs3.map( "Test" ) )
        {
        std::cout << "Add failing when trying to add a set" << std::endl;
        }

    // Testing orderByK on a set
    fs.orderByK( "Ten" );
    
    // removal tests
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

    fs1.add( "thirty","thirty");
    fs1.add( "thirty one","thirty one");
    fs1.add( "thirty two","thirty two" );
    fs1.add( "thirty three","thirty three" );
    fs1.add( "thirty four","thirty four" );
    fs1.add( "thrity five","thrity five" );

    fs1.map("thirty", set );
    if ( fs.mutuallyExclusive( "Zero", set ) == false )
        {
        std::cout << "test for mutually exclusiveness failing" << std::endl;
        return false;
        }

    fs1.map("thirty", set );
    if ( fs.mutuallyInclusive( "Zero",set  ) == true )
        {
        std::cout << "test for mutually inclusuveness failing" << std::endl;
        return false;
        }

    fs.map( "Zero", set );
    fs1.setUnion( "thirty",set );
    fs.map("Zero", set );
    if ( fs1.isRhsSubset( "thirty",set ) == false )
        {
        std::cout << "rhs is not a subset of fs1" << std::endl;
        return false;
        }

    fs.map("Zero", set );
    if ( fs1.isRhsProperSubset( "thirty",set ) == false )
        {
        std::cout << "rhs is not a proper subset of fs1" << std::endl;
        return false;
        }
    
    fs.map("Zero", set );
    if ( ( fs1.mutuallyExclusive( "thirty",set ) == true ) ||
         ( fs1.mutuallyInclusive( "thirty",set ) == true ) )
        {
        std::cout << "Test for both mutually exclusive and inclusive failing"
             << std::endl;
        return false;
        }
    
    fs.map("Zero", set );
    fs1.setIntersection( "thirty",set );

    fs1.map("thirty", set );
    if ( fs.isRhsSubset( "Zero", set ) == false )
        {
        std::cout << "setIntersection failing" << std::endl;
        return false;
        }

    fs1.add( "thirty","thirty");
    fs1.add( "thirty","thirty one" );
    fs1.add( "thirty","thirty two" );
    fs1.add( "thirty","thirty three" );
    fs1.add( "thirty","thirty four" );
    fs1.add( "thirty","thrity five" );


    fs.map("Zero", set );
    fs1.setMinus( "thirty", set);

    fs.map("Zero", set );
    if ( fs1.mutuallyExclusive( "thirty",set ) == false )
        {
        std::cout << "test for set minus failing" << std::endl;
        return false;
        }


    if ( ( fs == fs1 ) || ( fs != fs ) )
        {
        std::cout << "Error in equality operators." << std::endl;
        }
            
    return true;
    }



// -- global ------------------------------------------------------------------
// dictOfSetKPTest()
// Tests DictOfSet containing pointer TextStrings.  Returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
bool dictOfSetKPTest()
    {
    DictOfSetKP<TextString,TextString*> fs;

    // check size
    if (fs.size() != 0)
        {
        std::cout << "Non-zero size() returned on zero size dictOfSet" << std::endl;
        return false;
        }

    // assignment operator on empty set
    DictOfSetKP<TextString,TextString*> fs1;
    fs1 = fs;
    if (fs1.size() != 0)
        {
        std::cout << "operator= fail on empty DictOfSet" << std::endl;
        return false;
        }

    // copy constructor on empty set
    DictOfSetKP<TextString,TextString*> fs2(fs1);
    if (fs2.size() != 0)
        {
        std::cout << "copy constructor fail on empty DictOfSet" << std::endl;
        return false;
        }

    TextString aKey("Akey");
    // try to find a key with an empty set
    if (fs.map("AKey") || fs.map("AKey",&aKey))
        {
        std::cout << "map() incorrect when key does not exist" << std::endl;
        return false;
        }

    // Add elements
    TextString zero("Zero");
    TextString one("One");
    TextString two("Two");
    TextString three("Three");
    TextString four("Four");
    TextString five("Five");
    TextString six("Six");
    TextString seven("Seven");
    TextString eight("Eight");
    TextString nine("Nine");
    TextString ten("Ten");
    
    fs.add("Zero", &zero);
    fs.add("One",  &one);
    fs.add("Two",  &two);
    fs.add("Three",&three);
    fs.add("Four", &four);
    fs.add("Five", &five);
    fs.add("Six",  &six);
    fs.add("Seven",&seven);
    fs.add("Eight",&eight);
    fs.add("Nine", &nine);
    fs.add("Ten",  &ten);

    
    // check size
    if ((fs.size() != 11) || (fs.size("Zero") != 1))
        {
        std::cout << "size incorrect after adding elements" << std::endl;
        }

    // Checking assignment constructor on a set with elements
    fs2 = fs;
    if ( ( fs2.size() != 11 ) || ( fs2.size( "Zero" ) != 1 ) )
        std::cout << "Size incorrect after assignment constructor on a set with"
             << " elements." << std::endl;



    // Checking copy constructor on a set with elements
    DictOfSetKP<TextString,TextString*> fs3(fs);
    if ( ( fs3.size() != 11 ) || ( fs3.size( "Zero" ) != 1 ) )
        std::cout << "Size incorrect after copy constructor on a set with"
             << " elements." << std::endl;


    // Trying remove with just a key
    fs3.remove( ten );
    if ( fs3.size() != 10 )
        {
        std::cout << "Removal of set failing." << std::endl;
        }


    
    // check for existing key and get value
    if ( !fs.map( "Seven" ) ||  !fs.map( "Seven", &seven ) )
        {
        std::cout << "map failing" << std::endl;
        return false;
        }

    SetP<TextString*> set;
    
    // Checking map with a Set object
    if ( !fs.map( "Seven", set ) && !set.map( &seven ) )
        {
        std::cout << "Map failing with a set. " << std::endl;
        }

    // Order by K test
    fs.orderByK();

    // Key test
    TextString ts1 = fs.key( 1 );
    if ( ts1 != "Five" )
        std::cout << "Error in key test." << std::endl;

    // Value tests
    set = fs.value( 1 );

    if ( ( *set[0] != ts1 ) || ( *fs.value( "Five", 0 ) != ts1 ) )
        std::cout << "Error in value tests." << std::endl;


    // Trying to add a set
    fs3.add( "Test", set );
    if ( ( fs3.size() != 11 ) || !fs3.map( "Test" ) )
        {
        std::cout << "Add failing when trying to add a set" << std::endl;
        }

    // Testing orderByK on a set
    fs.orderByK( "Ten" );
    
    // removal tests
    fs.remove("Nineteen");
    if (fs.size() != 11)
        {
        std::cout << "size() incorrect after attempted removal of non-existant"
          << std::endl;
        return false;
        }

    TextString nine1("Nine");
    TextString ten1("Ten");
    fs.remove( nine1 );
    if (fs.size() != 10 || fs.map( nine1 ))
        {
        std::cout << "remove() fail on key that exists" << std::endl;
        return false;
        }
    
    if ( !fs.map( ten1 ) )
        {
        std::cout << "map() or data incorrect after add() modification" 
             << std::endl;
        return false;
        }

    TextString thirty("thirty");
    TextString thirtyOne("thirty one");
    TextString thirtyTwo("thirty two");
    TextString thirtyThree("thirty three");
    TextString thirtyFour("thirty four");
    TextString thirtyFive("thirty five");
    
    fs1.add( "thirty",       &thirty);
    fs1.add( "thirty one",   &thirtyOne);
    fs1.add( "thirty two",   &thirtyTwo );
    fs1.add( "thirty three", &thirtyThree );
    fs1.add( "thirty four",  &thirtyFour);
    fs1.add( "thrity five",  &thirtyFive);

    fs1.map("thirty", set );
    if ( fs.mutuallyExclusive( "Zero", set ) == false )
        {
        std::cout << "test for mutually exclusiveness failing" << std::endl;
        return false;
        }

    fs1.map("thirty", set );
    if ( fs.mutuallyInclusive( "Zero",set  ) == true )
        {
        std::cout << "test for mutually inclusuveness failing" << std::endl;
        return false;
        }

    fs.map( "Zero", set );
    fs1.setUnion( "thirty",set );
    fs.map("Zero", set );
    if ( fs1.isRhsSubset( "thirty",set ) == false )
        {
        std::cout << "rhs is not a subset of fs1" << std::endl;
        return false;
        }

    fs.map("Zero", set );
    if ( fs1.isRhsProperSubset( "thirty",set ) == false )
        {
        std::cout << "rhs is not a proper subset of fs1" << std::endl;
        return false;
        }
    
    fs.map("Zero", set );
    if ( ( fs1.mutuallyExclusive( "thirty",set ) == true ) ||
         ( fs1.mutuallyInclusive( "thirty",set ) == true ) )
        {
        std::cout << "Test for both mutually exclusive and inclusive failing"
             << std::endl;
        return false;
        }
    
    fs.map("Zero", set );
    fs1.setIntersection( "thirty",set );

    fs1.map("thirty", set );
    if ( fs.isRhsSubset( "Zero", set ) == false )
        {
        std::cout << "setIntersection failing" << std::endl;
        return false;
        }

    fs1.add( "thirty", &thirty);
    fs1.add( "thirty", &thirtyOne);
    fs1.add( "thirty", &thirtyTwo);
    fs1.add( "thirty", &thirtyThree);
    fs1.add( "thirty", &thirtyFour);
    fs1.add( "thirty", &thirtyFive);


    fs.map("Zero", set );
    fs1.setMinus( "thirty", set);

    fs.map("Zero", set );
    if ( fs1.mutuallyExclusive( "thirty",set ) == false )
        {
        std::cout << "test for set minus failing" << std::endl;
        return false;
        }


    if ( ( fs == fs1 ) || ( fs != fs ) )
        {
        std::cout << "Error in equality operators." << std::endl;
        }
            
    return true;
    }



// -- global ------------------------------------------------------------------
// dictOfSetPVTest()
// Tests DictOfSet containing TextStrings.  Returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
bool dictOfSetPVTest()
    {
    DictOfSetPV<TextString*,TextString> fs;

    // check size
    if (fs.size() != 0)
        {
        std::cout << "Non-zero size() returned on zero size dictOfSet" << std::endl;
        return false;
        }

    // assignment operator on empty set
    DictOfSetPV<TextString*,TextString> fs1;
    fs1 = fs;
    if (fs1.size() != 0)
        {
        std::cout << "operator= fail on empty DictOfSet" << std::endl;
        return false;
        }

    // copy constructor on empty set
    DictOfSetPV<TextString*,TextString> fs2(fs1);
    if (fs2.size() != 0)
        {
        std::cout << "copy constructor fail on empty DictOfSet" << std::endl;
        return false;
        }

    TextString aKey("Akey");
    // try to find a key with an empty set
    if (fs.map( &aKey) || fs.map( &aKey,"AKey"))
        {
        std::cout << "map() incorrect when key does not exist" << std::endl;
        return false;
        }

    // Add elements
    TextString zero("Zero");
    TextString one("One");
    TextString two("Two");
    TextString three("Three");
    TextString four("Four");
    TextString five("Five");
    TextString six("Six");
    TextString seven("Seven");
    TextString eight("Eight");
    TextString nine("Nine");
    TextString ten("Ten");

    fs.add(&zero,"Zero");
    fs.add(&one,"One");
    fs.add(&two,"Two");
    fs.add(&three,"Three");
    fs.add(&four,"Four");
    fs.add(&five,"Five");
    fs.add(&six,"Six");
    fs.add(&seven,"Seven");
    fs.add(&eight,"Eight");
    fs.add(&nine,"Nine");
    fs.add(&ten,"Ten");

    
    // check size
    if ((fs.size() != 11) || (fs.size( &zero ) != 1))
        {
        std::cout << "size incorrect after adding elements" << std::endl;
        }

    // Checking assignment constructor on a set with elements
    fs2 = fs;
    if ( ( fs2.size() != 11 ) || ( fs2.size( &zero ) != 1 ) )
        std::cout << "Size incorrect after assignment constructor on a set with"
             << " elements." << std::endl;



    // Checking copy constructor on a set with elements
    DictOfSetPV<TextString*,TextString> fs3(fs);
    if ( ( fs3.size() != 11 ) || ( fs3.size( &zero ) != 1 ) )
        std::cout << "Size incorrect after copy constructor on a set with"
             << " elements." << std::endl;


    // Trying remove with just a key
    fs3.remove( &ten );
    if ( fs3.size() != 10 )
        {
        std::cout << "Removal of set failing." << std::endl;
        }


    
    // check for existing key and get value
    if ( !fs.map( &seven ) ||  !fs.map( &seven,"Seven" ) )
        {
        std::cout << "map failing" << std::endl;
        return false;
        }

    Set<TextString> set;
    
    // Checking map with a Set object
    if ( !fs.map( &seven, set ) && !set.map( "Seven" ) )
        {
        std::cout << "Map failing with a set. " << std::endl;
        }

    // Order by K test
    fs.orderByK();

    // Key test
    TextString *ts1 = fs.key( 1 );
    if ( *ts1 != "One" )
        std::cout << "Error in key test." << std::endl;


    // Value tests
    set = fs.value( 1 );

    if ( ( set[0] != *ts1 ) || ( fs.value( &one, 0 ) != *ts1 ) )
        std::cout << "Error in value tests." << std::endl;


    // Trying to add a set
    TextString test("Test");
    fs3.add( &test, set );
    if ( ( fs3.size() != 11 ) || !fs3.map( &test ) )
        {
        std::cout << "Add failing when trying to add a set" << std::endl;
        }

    // Testing orderByK on a set
    fs.orderByK( &ten );
    
    // removal tests
    TextString nineteen("Nineteen");
    fs.remove(&nineteen);
    if (fs.size() != 11)
        {
        std::cout << "size() incorrect after attempted removal of non-existant"
          << std::endl;
        return false;
        }
    
    fs.remove( &nine);
    if (fs.size() != 10 || fs.map( &nine))
        {
        std::cout << "remove() fail on key that exists" << std::endl;
        return false;
        }
    
    if ( !fs.map( &ten ) )
        {
        std::cout << "map() or data incorrect after add() modification" 
             << std::endl;
        return false;
        }
    
    TextString thirty("thirty");
    TextString thirtyOne("thirty one");
    TextString thirtyTwo("thirty two");
    TextString thirtyThree("thirty three");
    TextString thirtyFour("thirty four");
    TextString thirtyFive("thirty five");

    fs1.add( &thirty,"thirty");
    fs1.add( &thirtyOne,"thirty one");
    fs1.add( &thirtyTwo,"thirty two" );
    fs1.add( &thirtyThree,"thirty three" );
    fs1.add( &thirtyFour,"thirty four" );
    fs1.add( &thirtyFive,"thrity five" );

    fs1.map( &thirty, set );
    if ( fs.mutuallyExclusive( &zero, set ) == false )
        {
        std::cout << "test for mutually exclusiveness failing" << std::endl;
        return false;
        }

    fs1.map( &thirty, set );
    if ( fs.mutuallyInclusive( &zero, set  ) == true )
        {
        std::cout << "test for mutually inclusuveness failing" << std::endl;
        return false;
        }

    fs.map( &zero, set );
    fs1.setUnion( &thirty,set );
    fs.map( &zero, set );
    if ( fs1.isRhsSubset( &thirty, set ) == false )
        {
        std::cout << "rhs is not a subset of fs1" << std::endl;
        return false;
        }

    fs.map( &zero, set );
    if ( fs1.isRhsProperSubset( &thirty,set ) == false )
        {
        std::cout << "rhs is not a proper subset of fs1" << std::endl;
        return false;
        }
    
    fs.map( &zero, set );
    if ( ( fs1.mutuallyExclusive( &thirty,set ) == true ) ||
         ( fs1.mutuallyInclusive( &thirty,set ) == true ) )
        {
        std::cout << "Test for both mutually exclusive and inclusive failing"
             << std::endl;
        return false;
        }
    
    fs.map(&zero, set );
    fs1.setIntersection( &thirty,set );

    fs1.map(&thirty, set );
    if ( fs.isRhsSubset( &zero, set ) == false )
        {
        std::cout << "setIntersection failing" << std::endl;
        return false;
        }

    fs1.add( &thirty,"thirty");
    fs1.add( &thirty,"thirty one" );
    fs1.add( &thirty,"thirty two" );
    fs1.add( &thirty,"thirty three" );
    fs1.add( &thirty,"thirty four" );
    fs1.add( &thirty,"thrity five" );


    fs.map(&zero, set );
    fs1.setMinus( &thirty, set);

    fs.map(&zero, set );
    if ( fs1.mutuallyExclusive( &thirty,set ) == false )
        {
        std::cout << "test for set minus failing" << std::endl;
        return false;
        }


    if ( ( fs == fs1 ) || ( fs != fs ) )
        {
        std::cout << "Error in equality operators." << std::endl;
        }
            
    return true;
    }


// -- global ------------------------------------------------------------------
// dictOfSetPPTest()
// Tests DictOfSet containing TextStrings.  Returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
bool dictOfSetPPTest()
    {
    DictOfSetPP<TextString*,TextString*> fs;

    // check size
    if (fs.size() != 0)
        {
        std::cout << "Non-zero size() returned on zero size dictOfSet" << std::endl;
        return false;
        }

    // assignment operator on empty set
    DictOfSetPP<TextString*,TextString*> fs1;
    fs1 = fs;
    if (fs1.size() != 0)
        {
        std::cout << "operator= fail on empty DictOfSet" << std::endl;
        return false;
        }

    // copy constructor on empty set
    DictOfSetPP<TextString*,TextString*> fs2(fs1);
    if (fs2.size() != 0)
        {
        std::cout << "copy constructor fail on empty DictOfSet" << std::endl;
        return false;
        }

    // try to find a key with an empty set
    TextString aKey("AKey");
    if (fs.map( &aKey ) || fs.map( &aKey, &aKey ))
        {
        std::cout << "map() incorrect when key does not exist" << std::endl;
        return false;
        }

    // Add elements
    TextString zero("Zero");
    TextString one("One");
    TextString two("Two");
    TextString three("Three");
    TextString four("Four");
    TextString five("Five");
    TextString six("Six");
    TextString seven("Seven");
    TextString eight("Eight");
    TextString nine("Nine");
    TextString ten("Ten");

    fs.add(&zero, &zero);
    fs.add(&one,&one);
    fs.add(&two,&two);
    fs.add(&three,&three);
    fs.add(&four,&four);
    fs.add(&five,&five);
    fs.add(&six,&six);
    fs.add(&seven,&seven);
    fs.add(&eight,&eight);
    fs.add(&nine,&nine);
    fs.add(&ten,&ten);

    
    // check size
    if ((fs.size() != 11) || (fs.size( &zero ) != 1))
        {
        std::cout << "size incorrect after adding elements" << std::endl;
        }

    // Checking assignment constructor on a set with elements
    fs2 = fs;
    if ( ( fs2.size() != 11 ) || ( fs2.size( &zero ) != 1 ) )
        std::cout << "Size incorrect after assignment constructor on a set with"
             << " elements." << std::endl;



    // Checking copy constructor on a set with elements
    DictOfSetPP<TextString*,TextString*> fs3(fs);
    if ( ( fs3.size() != 11 ) || ( fs3.size( &zero ) != 1 ) )
        std::cout << "Size incorrect after copy constructor on a set with"
             << " elements." << std::endl;


    // Trying remove with just a key
    fs3.remove( &ten );
    if ( fs3.size() != 10 )
        {
        std::cout << "Removal of set failing." << std::endl;
        }


    
    // check for existing key and get value
    if ( !fs.map( &seven ) ||  !fs.map( &seven,&seven ) )
        {
        std::cout << "map failing" << std::endl;
        return false;
        }

    SetP<TextString*> set;
    
    // Checking map with a Set object
    if ( !fs.map( &seven, set ) && !set.map( &seven ) )
        {
        std::cout << "Map failing with a set. " << std::endl;
        }

    // Order by K test
    fs.orderByK();

    // Key test
    TextString *ts1 = fs.key( 1 );
    if ( *ts1 != "One" )
        std::cout << "Error in key test." << std::endl;

    // Value tests
    set = fs.value( 1 );

    
    if ( ( *set[0] != *ts1 ) || ( *fs.value( &one, 0 ) != *ts1 ) )
        std::cout << "Error in value tests." << std::endl;


    // Trying to add a set
    TextString test("Test");
    fs3.add( &test, set );
    if ( ( fs3.size() != 11 ) || !fs3.map( &test ) )
        {
        std::cout << "Add failing when trying to add a set" << std::endl;
        }

    // Testing orderByK on a set
    fs.orderByK( &ten );
    
    // removal tests
    TextString nineteen("Nineteen");
    fs.remove( &nineteen );
    if (fs.size() != 11)
        {
        std::cout << "size() incorrect after attempted removal of non-existant"
          << std::endl;
        return false;
        }
    
    fs.remove( &nine);
    if (fs.size() != 10 || fs.map( &nine))
        {
        std::cout << "remove() fail on key that exists" << std::endl;
        return false;
        }
    
    if ( !fs.map( &ten ) )
        {
        std::cout << "map() or data incorrect after add() modification" 
             << std::endl;
        return false;
        }
    
    TextString thirty("thirty");
    TextString thirtyOne("thirty one");
    TextString thirtyTwo("thirty two");
    TextString thirtyThree("thirty three");
    TextString thirtyFour("thirty four");
    TextString thirtyFive("thirty five");

    fs1.add( &thirty, &thirty);
    fs1.add( &thirtyOne,&thirtyOne);
    fs1.add( &thirtyTwo, &thirtyTwo );
    fs1.add( &thirtyThree, &thirtyThree);
    fs1.add( &thirtyFour, &thirtyFour);
    fs1.add( &thirtyFive, &thirtyFive);

    fs1.map( &thirty, set );
    if ( fs.mutuallyExclusive( &zero, set ) == false )
        {
        std::cout << "test for mutually exclusiveness failing" << std::endl;
        return false;
        }

    fs1.map( &thirty, set );
    if ( fs.mutuallyInclusive( &zero, set  ) == true )
        {
        std::cout << "test for mutually inclusuveness failing" << std::endl;
        return false;
        }

    fs.map( &zero, set );
    fs1.setUnion( &thirty,set );
    fs.map( &zero, set );
    if ( fs1.isRhsSubset( &thirty, set ) == false )
        {
        std::cout << "rhs is not a subset of fs1" << std::endl;
        return false;
        }

    fs.map( &zero, set );
    if ( fs1.isRhsProperSubset( &thirty,set ) == false )
        {
        std::cout << "rhs is not a proper subset of fs1" << std::endl;
        return false;
        }
    
    fs.map( &zero, set );
    if ( ( fs1.mutuallyExclusive( &thirty,set ) == true ) ||
         ( fs1.mutuallyInclusive( &thirty,set ) == true ) )
        {
        std::cout << "Test for both mutually exclusive and inclusive failing"
             << std::endl;
        return false;
        }
    
    fs.map(&zero, set );
    fs1.setIntersection( &thirty,set );

    fs1.map(&thirty, set );
    if ( fs.isRhsSubset( &zero, set ) == false )
        {
        std::cout << "setIntersection failing" << std::endl;
        return false;
        }

    fs1.add( &thirty, &thirty);
    fs1.add( &thirty, &thirtyOne);
    fs1.add( &thirty, &thirtyTwo);
    fs1.add( &thirty, &thirtyThree);
    fs1.add( &thirty, &thirtyFour);
    fs1.add( &thirty, &thirtyFive);


    fs.map(&zero, set );
    fs1.setMinus( &thirty, set);

    fs.map(&zero, set );
    if ( fs1.mutuallyExclusive( &thirty,set ) == false )
        {
        std::cout << "test for set minus failing" << std::endl;
        return false;
        }


    if ( ( fs == fs1 ) || ( fs != fs ) )
        {
        std::cout << "Error in equality operators." << std::endl;
        }
            
    return true;
    }
