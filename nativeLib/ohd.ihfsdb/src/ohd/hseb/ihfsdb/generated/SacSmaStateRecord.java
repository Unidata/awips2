// filename: SacSmaStateRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              SacSmaState table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class SacSmaStateRecord extends DbRecord
{
    private String basin_id;

    private String source;

    private long validtime;

    private long basistime;

    private long postingtime;

    private double uztwc;

    private double uzfwc;

    private double lztwc;

    private double lzfsc;

    private double lzfpc;

    private double adimc;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public SacSmaStateRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public SacSmaStateRecord(SacSmaStateRecord origRecord)
    {
        setBasin_id(origRecord.getBasin_id());
        setSource(origRecord.getSource());
        setValidtime(origRecord.getValidtime());
        setBasistime(origRecord.getBasistime());
        setPostingtime(origRecord.getPostingtime());
        setUztwc(origRecord.getUztwc());
        setUzfwc(origRecord.getUzfwc());
        setLztwc(origRecord.getLztwc());
        setLzfsc(origRecord.getLzfsc());
        setLzfpc(origRecord.getLzfpc());
        setAdimc(origRecord.getAdimc());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a SacSmaState record

    //-----------------------------------------------------------------
    public String getBasin_id()
    {
        return basin_id;
    }

    public void setBasin_id(String basin_id)
    {
        this.basin_id = basin_id ;
    }

    public String getSource()
    {
        return source;
    }

    public void setSource(String source)
    {
        this.source = source ;
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

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

    public double getUztwc()
    {
        return uztwc;
    }

    public void setUztwc(double uztwc)
    {
        this.uztwc = uztwc ;
    }

    public double getUzfwc()
    {
        return uzfwc;
    }

    public void setUzfwc(double uzfwc)
    {
        this.uzfwc = uzfwc ;
    }

    public double getLztwc()
    {
        return lztwc;
    }

    public void setLztwc(double lztwc)
    {
        this.lztwc = lztwc ;
    }

    public double getLzfsc()
    {
        return lzfsc;
    }

    public void setLzfsc(double lzfsc)
    {
        this.lzfsc = lzfsc ;
    }

    public double getLzfpc()
    {
        return lzfpc;
    }

    public void setLzfpc(double lzfpc)
    {
        this.lzfpc = lzfpc ;
    }

    public double getAdimc()
    {
        return adimc;
    }

    public void setAdimc(double adimc)
    {
        this.adimc = adimc ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE basin_id = '" + basin_id + "'" 
                 + " AND source = '" + source + "'" 
                 + " AND validtime = '" +  getDateTimeStringFromLongTime(validtime) + "'" 
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
                getBasin_id() + " " +
                getSource() + " " +
                getDateTimeStringFromLongTime(getValidtime()) + " " +
                getDateTimeStringFromLongTime(getBasistime()) + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getUztwc() + " " +
                getUzfwc() + " " +
                getLztwc() + " " +
                getLzfsc() + " " +
                getLzfpc() + " " +
                getAdimc() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of SacSmaStateRecord class

