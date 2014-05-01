// filename: ShefExRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ShefEx table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ShefExRecord extends DbRecord
{
    private String extremum;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ShefExRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ShefExRecord(ShefExRecord origRecord)
    {
        setExtremum(origRecord.getExtremum());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ShefEx record

    //-----------------------------------------------------------------
    public String getExtremum()
    {
        return extremum;
    }

    public void setExtremum(String extremum)
    {
        this.extremum = extremum ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE extremum = '" + extremum + "'" 
                ;
        return outString;
    } // end toString()
//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getExtremum() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ShefExRecord class

