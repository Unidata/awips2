// filename: DamTypesRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              DamTypes table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class DamTypesRecord extends DbRecord
{
    private String type;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public DamTypesRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public DamTypesRecord(DamTypesRecord origRecord)
    {
        setType(origRecord.getType());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a DamTypes record

    //-----------------------------------------------------------------
    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE type = '" + type + "'" 
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
                getType() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of DamTypesRecord class

