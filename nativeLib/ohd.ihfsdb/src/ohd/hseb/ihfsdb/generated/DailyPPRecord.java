// filename: DailyPPRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              DailyPP table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class DailyPPRecord extends DbRecord
{
    private String lid;

    private String ts;

    private long obstime;

    private double value;

    private String qc;

    private long postingtime;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public DailyPPRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public DailyPPRecord(DailyPPRecord origRecord)
    {
        setLid(origRecord.getLid());
        setTs(origRecord.getTs());
        setObstime(origRecord.getObstime());
        setValue(origRecord.getValue());
        setQc(origRecord.getQc());
        setPostingtime(origRecord.getPostingtime());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a DailyPP record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getTs()
    {
        return ts;
    }

    public void setTs(String ts)
    {
        this.ts = ts ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public double getValue()
    {
        return value;
    }

    public void setValue(double value)
    {
        this.value = value ;
    }

    public String getQc()
    {
        return qc;
    }

    public void setQc(String qc)
    {
        this.qc = qc ;
    }

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
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
                 + " AND ts = '" + ts + "'" 
                 + " AND obstime = '" +  getDateTimeStringFromLongTime(obstime) + "'" 
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
                getTs() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getValue() + " " +
                getQc() + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of DailyPPRecord class

