// filename: CoopCommsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              CoopComms table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CoopCommsRecord extends DbRecord
{
    private String comm;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CoopCommsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CoopCommsRecord(CoopCommsRecord origRecord)
    {
        setComm(origRecord.getComm());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a CoopComms record

    //-----------------------------------------------------------------
    public String getComm()
    {
        return comm;
    }

    public void setComm(String comm)
    {
        this.comm = comm ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE comm = '" + comm + "'" 
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
                getComm() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CoopCommsRecord class

