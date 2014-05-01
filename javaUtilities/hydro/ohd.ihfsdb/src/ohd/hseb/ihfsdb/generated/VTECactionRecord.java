// filename: VTECactionRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              VTECaction table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class VTECactionRecord extends DbRecord
{
    private String action;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public VTECactionRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public VTECactionRecord(VTECactionRecord origRecord)
    {
        setAction(origRecord.getAction());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a VTECaction record

    //-----------------------------------------------------------------
    public String getAction()
    {
        return action;
    }

    public void setAction(String action)
    {
        this.action = action ;
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
                "WHERE action = '" + action + "'" 
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
                getAction() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of VTECactionRecord class

