// filename: RatingShiftRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RatingShift table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RatingShiftRecord extends DbRecord
{
    private String lid;

    private long date;

    private double shift_amount;

    private String active;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RatingShiftRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RatingShiftRecord(RatingShiftRecord origRecord)
    {
        setLid(origRecord.getLid());
        setDate(origRecord.getDate());
        setShift_amount(origRecord.getShift_amount());
        setActive(origRecord.getActive());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RatingShift record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public long getDate()
    {
        return date;
    }

    public void setDate(long date)
    {
        this.date = date ;
    }

    public double getShift_amount()
    {
        return shift_amount;
    }

    public void setShift_amount(double shift_amount)
    {
        this.shift_amount = shift_amount ;
    }

    public String getActive()
    {
        return active;
    }

    public void setActive(String active)
    {
        this.active = active ;
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
                 + " AND date = '" +  getDateStringFromLongTime(date) + "'" 
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
                getDateStringFromLongTime(getDate()) + " " +
                getShift_amount() + " " +
                getActive() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RatingShiftRecord class

