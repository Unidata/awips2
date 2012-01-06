// filename: VTECcauseRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              VTECcause table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class VTECcauseRecord extends DbRecord
{
    private String immed_cause;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public VTECcauseRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public VTECcauseRecord(VTECcauseRecord origRecord)
    {
        setImmed_cause(origRecord.getImmed_cause());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a VTECcause record

    //-----------------------------------------------------------------
    public String getImmed_cause()
    {
        return immed_cause;
    }

    public void setImmed_cause(String immed_cause)
    {
        this.immed_cause = immed_cause ;
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
                "WHERE immed_cause = '" + immed_cause + "'" 
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
                getImmed_cause() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of VTECcauseRecord class

