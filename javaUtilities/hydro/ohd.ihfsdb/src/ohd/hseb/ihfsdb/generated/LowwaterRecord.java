// filename: LowwaterRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Lowwater table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LowwaterRecord extends DbRecord
{
    private String lid;

    private long lwdat;

    private int q;

    private String lwrem;

    private double stage;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LowwaterRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LowwaterRecord(LowwaterRecord origRecord)
    {
        setLid(origRecord.getLid());
        setLwdat(origRecord.getLwdat());
        setQ(origRecord.getQ());
        setLwrem(origRecord.getLwrem());
        setStage(origRecord.getStage());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Lowwater record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public long getLwdat()
    {
        return lwdat;
    }

    public void setLwdat(long lwdat)
    {
        this.lwdat = lwdat ;
    }

    public int getQ()
    {
        return q;
    }

    public void setQ(int q)
    {
        this.q = q ;
    }

    public String getLwrem()
    {
        return lwrem;
    }

    public void setLwrem(String lwrem)
    {
        this.lwrem = lwrem ;
    }

    public double getStage()
    {
        return stage;
    }

    public void setStage(double stage)
    {
        this.stage = stage ;
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
                 + " AND lwdat = '" +  getDateStringFromLongTime(lwdat) + "'" 
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
                getDateStringFromLongTime(getLwdat()) + " " +
                getQ() + " " +
                getLwrem() + " " +
                getStage() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LowwaterRecord class

