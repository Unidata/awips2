// filename: ShefQcRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ShefQc table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ShefQcRecord extends DbRecord
{
    private String shef_qual_code;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ShefQcRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ShefQcRecord(ShefQcRecord origRecord)
    {
        setShef_qual_code(origRecord.getShef_qual_code());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ShefQc record

    //-----------------------------------------------------------------
    public String getShef_qual_code()
    {
        return shef_qual_code;
    }

    public void setShef_qual_code(String shef_qual_code)
    {
        this.shef_qual_code = shef_qual_code ;
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
                "WHERE shef_qual_code = '" + shef_qual_code + "'" 
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
                getShef_qual_code() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ShefQcRecord class

