// filename: RiverStatusRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RiverStatus table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RiverStatusRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private float probability;

    private long validtime;

    private long basistime;

    private double value;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RiverStatusRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RiverStatusRecord(RiverStatusRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setTs(origRecord.getTs());
        setExtremum(origRecord.getExtremum());
        setProbability(origRecord.getProbability());
        setValidtime(origRecord.getValidtime());
        setBasistime(origRecord.getBasistime());
        setValue(origRecord.getValue());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RiverStatus record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public short getDur()
    {
        return dur;
    }

    public void setDur(short dur)
    {
        this.dur = dur ;
    }

    public String getTs()
    {
        return ts;
    }

    public void setTs(String ts)
    {
        this.ts = ts ;
    }

    public String getExtremum()
    {
        return extremum;
    }

    public void setExtremum(String extremum)
    {
        this.extremum = extremum ;
    }

    public float getProbability()
    {
        return probability;
    }

    public void setProbability(float probability)
    {
        this.probability = probability ;
    }

    public long getValidtime()
    {
        return validtime;
    }

    public void setValidtime(long validtime)
    {
        this.validtime = validtime ;
    }

    public long getBasistime()
    {
        return basistime;
    }

    public void setBasistime(long basistime)
    {
        this.basistime = basistime ;
    }

    public double getValue()
    {
        return value;
    }

    public void setValue(double value)
    {
        this.value = value ;
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
                 + " AND pe = '" + pe + "'" 
                 + " AND ts = '" + ts + "'" 
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
                getPe() + " " +
                getDur() + " " +
                getTs() + " " +
                getExtremum() + " " +
                getProbability() + " " +
                getDateTimeStringFromLongTime(getValidtime()) + " " +
                getDateTimeStringFromLongTime(getBasistime()) + " " +
                getValue() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RiverStatusRecord class

