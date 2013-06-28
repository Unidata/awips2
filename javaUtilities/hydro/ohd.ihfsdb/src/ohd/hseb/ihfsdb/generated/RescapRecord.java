// filename: RescapRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Rescap table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RescapRecord extends DbRecord
{
    private String lid;

    private double elev;

    private double storage;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RescapRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RescapRecord(RescapRecord origRecord)
    {
        setLid(origRecord.getLid());
        setElev(origRecord.getElev());
        setStorage(origRecord.getStorage());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Rescap record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public double getElev()
    {
        return elev;
    }

    public void setElev(double elev)
    {
        this.elev = elev ;
    }

    public double getStorage()
    {
        return storage;
    }

    public void setStorage(double storage)
    {
        this.storage = storage ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE lid = '" + lid + "'" 
                 + " AND elev = '" + elev + "'" 
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
                getLid() + " " +
                getElev() + " " +
                getStorage() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RescapRecord class

