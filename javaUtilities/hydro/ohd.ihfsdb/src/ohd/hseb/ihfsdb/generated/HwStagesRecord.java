// This is a view record !
// filename: HwStagesRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              HwStages table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class HwStagesRecord extends DbRecord
{
    private String lid;

    private double fs;

    private double wstg;

    private double ms;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public HwStagesRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public HwStagesRecord(HwStagesRecord origRecord)
    {
        setLid(origRecord.getLid());
        setFs(origRecord.getFs());
        setWstg(origRecord.getWstg());
        setMs(origRecord.getMs());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a HwStages record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public double getFs()
    {
        return fs;
    }

    public void setFs(double fs)
    {
        this.fs = fs ;
    }

    public double getWstg()
    {
        return wstg;
    }

    public void setWstg(double wstg)
    {
        this.wstg = wstg ;
    }

    public double getMs()
    {
        return ms;
    }

    public void setMs(double ms)
    {
        this.ms = ms ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getLid() + " " +
                getFs() + " " +
                getWstg() + " " +
                getMs() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of HwStagesRecord class

