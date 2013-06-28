// filename: RpfParamsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RpfParams table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RpfParamsRecord extends DbRecord
{
    private int obshrs;

    private int fcsthrs;

    private String missval;

    private String misscat;

    private String misstim;

    private int rvsexphrs;

    private int flsexphrs;

    private int flwexphrs;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RpfParamsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RpfParamsRecord(RpfParamsRecord origRecord)
    {
        setObshrs(origRecord.getObshrs());
        setFcsthrs(origRecord.getFcsthrs());
        setMissval(origRecord.getMissval());
        setMisscat(origRecord.getMisscat());
        setMisstim(origRecord.getMisstim());
        setRvsexphrs(origRecord.getRvsexphrs());
        setFlsexphrs(origRecord.getFlsexphrs());
        setFlwexphrs(origRecord.getFlwexphrs());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RpfParams record

    //-----------------------------------------------------------------
    public int getObshrs()
    {
        return obshrs;
    }

    public void setObshrs(int obshrs)
    {
        this.obshrs = obshrs ;
    }

    public int getFcsthrs()
    {
        return fcsthrs;
    }

    public void setFcsthrs(int fcsthrs)
    {
        this.fcsthrs = fcsthrs ;
    }

    public String getMissval()
    {
        return missval;
    }

    public void setMissval(String missval)
    {
        this.missval = missval ;
    }

    public String getMisscat()
    {
        return misscat;
    }

    public void setMisscat(String misscat)
    {
        this.misscat = misscat ;
    }

    public String getMisstim()
    {
        return misstim;
    }

    public void setMisstim(String misstim)
    {
        this.misstim = misstim ;
    }

    public int getRvsexphrs()
    {
        return rvsexphrs;
    }

    public void setRvsexphrs(int rvsexphrs)
    {
        this.rvsexphrs = rvsexphrs ;
    }

    public int getFlsexphrs()
    {
        return flsexphrs;
    }

    public void setFlsexphrs(int flsexphrs)
    {
        this.flsexphrs = flsexphrs ;
    }

    public int getFlwexphrs()
    {
        return flwexphrs;
    }

    public void setFlwexphrs(int flwexphrs)
    {
        this.flwexphrs = flwexphrs ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getObshrs() + " " +
                getFcsthrs() + " " +
                getMissval() + " " +
                getMisscat() + " " +
                getMisstim() + " " +
                getRvsexphrs() + " " +
                getFlsexphrs() + " " +
                getFlwexphrs() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RpfParamsRecord class

