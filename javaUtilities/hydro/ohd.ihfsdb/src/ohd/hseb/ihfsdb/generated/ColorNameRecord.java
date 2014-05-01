// filename: ColorNameRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ColorName table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ColorNameRecord extends DbRecord
{
    private String color_name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ColorNameRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ColorNameRecord(ColorNameRecord origRecord)
    {
        setColor_name(origRecord.getColor_name());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ColorName record

    //-----------------------------------------------------------------
    public String getColor_name()
    {
        return color_name;
    }

    public void setColor_name(String color_name)
    {
        this.color_name = color_name ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE color_name = '" + color_name + "'" 
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
                getColor_name() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ColorNameRecord class

