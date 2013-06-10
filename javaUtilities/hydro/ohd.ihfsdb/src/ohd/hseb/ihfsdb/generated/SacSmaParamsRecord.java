// filename: SacSmaParamsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              SacSmaParams table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class SacSmaParamsRecord extends DbRecord
{
    private String basin_id;

    private String source;

    private long validtime;

    private long postingtime;

    private double uztwm;

    private double uzfwm;

    private double uzk;

    private double pctim;

    private double adimp;

    private double riva;

    private double zperc;

    private double rexp;

    private double lztwm;

    private double lzfsm;

    private double lzfpm;

    private double lzsk;

    private double lzpk;

    private double pfree;

    private double rserv;

    private double side;

    private double peadj;

    private double pxadj;

    private double efc;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public SacSmaParamsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public SacSmaParamsRecord(SacSmaParamsRecord origRecord)
    {
        setBasin_id(origRecord.getBasin_id());
        setSource(origRecord.getSource());
        setValidtime(origRecord.getValidtime());
        setPostingtime(origRecord.getPostingtime());
        setUztwm(origRecord.getUztwm());
        setUzfwm(origRecord.getUzfwm());
        setUzk(origRecord.getUzk());
        setPctim(origRecord.getPctim());
        setAdimp(origRecord.getAdimp());
        setRiva(origRecord.getRiva());
        setZperc(origRecord.getZperc());
        setRexp(origRecord.getRexp());
        setLztwm(origRecord.getLztwm());
        setLzfsm(origRecord.getLzfsm());
        setLzfpm(origRecord.getLzfpm());
        setLzsk(origRecord.getLzsk());
        setLzpk(origRecord.getLzpk());
        setPfree(origRecord.getPfree());
        setRserv(origRecord.getRserv());
        setSide(origRecord.getSide());
        setPeadj(origRecord.getPeadj());
        setPxadj(origRecord.getPxadj());
        setEfc(origRecord.getEfc());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a SacSmaParams record

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

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

    public double getUztwm()
    {
        return uztwm;
    }

    public void setUztwm(double uztwm)
    {
        this.uztwm = uztwm ;
    }

    public double getUzfwm()
    {
        return uzfwm;
    }

    public void setUzfwm(double uzfwm)
    {
        this.uzfwm = uzfwm ;
    }

    public double getUzk()
    {
        return uzk;
    }

    public void setUzk(double uzk)
    {
        this.uzk = uzk ;
    }

    public double getPctim()
    {
        return pctim;
    }

    public void setPctim(double pctim)
    {
        this.pctim = pctim ;
    }

    public double getAdimp()
    {
        return adimp;
    }

    public void setAdimp(double adimp)
    {
        this.adimp = adimp ;
    }

    public double getRiva()
    {
        return riva;
    }

    public void setRiva(double riva)
    {
        this.riva = riva ;
    }

    public double getZperc()
    {
        return zperc;
    }

    public void setZperc(double zperc)
    {
        this.zperc = zperc ;
    }

    public double getRexp()
    {
        return rexp;
    }

    public void setRexp(double rexp)
    {
        this.rexp = rexp ;
    }

    public double getLztwm()
    {
        return lztwm;
    }

    public void setLztwm(double lztwm)
    {
        this.lztwm = lztwm ;
    }

    public double getLzfsm()
    {
        return lzfsm;
    }

    public void setLzfsm(double lzfsm)
    {
        this.lzfsm = lzfsm ;
    }

    public double getLzfpm()
    {
        return lzfpm;
    }

    public void setLzfpm(double lzfpm)
    {
        this.lzfpm = lzfpm ;
    }

    public double getLzsk()
    {
        return lzsk;
    }

    public void setLzsk(double lzsk)
    {
        this.lzsk = lzsk ;
    }

    public double getLzpk()
    {
        return lzpk;
    }

    public void setLzpk(double lzpk)
    {
        this.lzpk = lzpk ;
    }

    public double getPfree()
    {
        return pfree;
    }

    public void setPfree(double pfree)
    {
        this.pfree = pfree ;
    }

    public double getRserv()
    {
        return rserv;
    }

    public void setRserv(double rserv)
    {
        this.rserv = rserv ;
    }

    public double getSide()
    {
        return side;
    }

    public void setSide(double side)
    {
        this.side = side ;
    }

    public double getPeadj()
    {
        return peadj;
    }

    public void setPeadj(double peadj)
    {
        this.peadj = peadj ;
    }

    public double getPxadj()
    {
        return pxadj;
    }

    public void setPxadj(double pxadj)
    {
        this.pxadj = pxadj ;
    }

    public double getEfc()
    {
        return efc;
    }

    public void setEfc(double efc)
    {
        this.efc = efc ;
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
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getUztwm() + " " +
                getUzfwm() + " " +
                getUzk() + " " +
                getPctim() + " " +
                getAdimp() + " " +
                getRiva() + " " +
                getZperc() + " " +
                getRexp() + " " +
                getLztwm() + " " +
                getLzfsm() + " " +
                getLzfpm() + " " +
                getLzsk() + " " +
                getLzpk() + " " +
                getPfree() + " " +
                getRserv() + " " +
                getSide() + " " +
                getPeadj() + " " +
                getPxadj() + " " +
                getEfc() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of SacSmaParamsRecord class

