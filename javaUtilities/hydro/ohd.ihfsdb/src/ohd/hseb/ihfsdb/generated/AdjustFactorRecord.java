// filename: AdjustFactorRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              AdjustFactor table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class AdjustFactorRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private double divisor;

    private double base;

    private double multiplier;

    private double adder;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public AdjustFactorRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public AdjustFactorRecord(AdjustFactorRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setTs(origRecord.getTs());
        setExtremum(origRecord.getExtremum());
        setDivisor(origRecord.getDivisor());
        setBase(origRecord.getBase());
        setMultiplier(origRecord.getMultiplier());
        setAdder(origRecord.getAdder());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a AdjustFactor record

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

    public double getDivisor()
    {
        return divisor;
    }

    public void setDivisor(double divisor)
    {
        this.divisor = divisor ;
    }

    public double getBase()
    {
        return base;
    }

    public void setBase(double base)
    {
        this.base = base ;
    }

    public double getMultiplier()
    {
        return multiplier;
    }

    public void setMultiplier(double multiplier)
    {
        this.multiplier = multiplier ;
    }

    public double getAdder()
    {
        return adder;
    }

    public void setAdder(double adder)
    {
        this.adder = adder ;
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
                 + " AND dur = '" + dur + "'" 
                 + " AND ts = '" + ts + "'" 
                 + " AND extremum = '" + extremum + "'" 
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
                getDivisor() + " " +
                getBase() + " " +
                getMultiplier() + " " +
                getAdder() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of AdjustFactorRecord class

