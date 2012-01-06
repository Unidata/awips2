// This is a view record !
// filename: StatProfRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              StatProf table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class StatProfRecord extends DbRecord
{
    private String lid;

    private String name;

    private String primary_pe;

    private String stream;

    private double fs;

    private double wstg;

    private double fq;

    private double action_flow;

    private double zd;

    private double mile;

    private String reach;

    private String proximity;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public StatProfRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public StatProfRecord(StatProfRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setPrimary_pe(origRecord.getPrimary_pe());
        setStream(origRecord.getStream());
        setFs(origRecord.getFs());
        setWstg(origRecord.getWstg());
        setFq(origRecord.getFq());
        setAction_flow(origRecord.getAction_flow());
        setZd(origRecord.getZd());
        setMile(origRecord.getMile());
        setReach(origRecord.getReach());
        setProximity(origRecord.getProximity());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a StatProf record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

    public String getPrimary_pe()
    {
        return primary_pe;
    }

    public void setPrimary_pe(String primary_pe)
    {
        this.primary_pe = primary_pe ;
    }

    public String getStream()
    {
        return stream;
    }

    public void setStream(String stream)
    {
        this.stream = stream ;
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

    public double getFq()
    {
        return fq;
    }

    public void setFq(double fq)
    {
        this.fq = fq ;
    }

    public double getAction_flow()
    {
        return action_flow;
    }

    public void setAction_flow(double action_flow)
    {
        this.action_flow = action_flow ;
    }

    public double getZd()
    {
        return zd;
    }

    public void setZd(double zd)
    {
        this.zd = zd ;
    }

    public double getMile()
    {
        return mile;
    }

    public void setMile(double mile)
    {
        this.mile = mile ;
    }

    public String getReach()
    {
        return reach;
    }

    public void setReach(String reach)
    {
        this.reach = reach ;
    }

    public String getProximity()
    {
        return proximity;
    }

    public void setProximity(String proximity)
    {
        this.proximity = proximity ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getLid() + " " +
                getName() + " " +
                getPrimary_pe() + " " +
                getStream() + " " +
                getFs() + " " +
                getWstg() + " " +
                getFq() + " " +
                getAction_flow() + " " +
                getZd() + " " +
                getMile() + " " +
                getReach() + " " +
                getProximity() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of StatProfRecord class

