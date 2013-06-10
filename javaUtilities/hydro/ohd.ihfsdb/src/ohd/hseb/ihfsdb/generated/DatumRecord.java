// filename: DatumRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Datum table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class DatumRecord extends DbRecord
{
    private String lid;

    private long ddate;

    private double elev;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public DatumRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public DatumRecord(DatumRecord origRecord)
    {
        setLid(origRecord.getLid());
        setDdate(origRecord.getDdate());
        setElev(origRecord.getElev());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Datum record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public long getDdate()
    {
        return ddate;
    }

    public void setDdate(long ddate)
    {
        this.ddate = ddate ;
    }

    public double getElev()
    {
        return elev;
    }

    public void setElev(double elev)
    {
        this.elev = elev ;
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
                 + " AND ddate = '" +  getDateStringFromLongTime(ddate) + "'" 
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
                getDateStringFromLongTime(getDdate()) + " " +
                getElev() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of DatumRecord class

