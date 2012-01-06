// This is a view record !
// filename: FpInfoRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FpInfo table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FpInfoRecord extends DbRecord
{
    private String lid;

    private String name;

    private String county;

    private String state;

    private String hsa;

    private String primary_back;

    private String secondary_back;

    private String stream;

    private double bf;

    private double wstg;

    private double fs;

    private double fq;

    private double action_flow;

    private String pe;

    private String use_latest_fcst;

    private String proximity;

    private String reach;

    private String group_id;

    private int ordinal;

    private double chg_threshold;

    private String rec_type;

    private int backhrs;

    private int forwardhrs;

    private double adjustendhrs;

    private double minor_stage;

    private double moderate_stage;

    private double major_stage;

    private double minor_flow;

    private double moderate_flow;

    private double major_flow;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FpInfoRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FpInfoRecord(FpInfoRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setCounty(origRecord.getCounty());
        setState(origRecord.getState());
        setHsa(origRecord.getHsa());
        setPrimary_back(origRecord.getPrimary_back());
        setSecondary_back(origRecord.getSecondary_back());
        setStream(origRecord.getStream());
        setBf(origRecord.getBf());
        setWstg(origRecord.getWstg());
        setFs(origRecord.getFs());
        setFq(origRecord.getFq());
        setAction_flow(origRecord.getAction_flow());
        setPe(origRecord.getPe());
        setUse_latest_fcst(origRecord.getUse_latest_fcst());
        setProximity(origRecord.getProximity());
        setReach(origRecord.getReach());
        setGroup_id(origRecord.getGroup_id());
        setOrdinal(origRecord.getOrdinal());
        setChg_threshold(origRecord.getChg_threshold());
        setRec_type(origRecord.getRec_type());
        setBackhrs(origRecord.getBackhrs());
        setForwardhrs(origRecord.getForwardhrs());
        setAdjustendhrs(origRecord.getAdjustendhrs());
        setMinor_stage(origRecord.getMinor_stage());
        setModerate_stage(origRecord.getModerate_stage());
        setMajor_stage(origRecord.getMajor_stage());
        setMinor_flow(origRecord.getMinor_flow());
        setModerate_flow(origRecord.getModerate_flow());
        setMajor_flow(origRecord.getMajor_flow());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FpInfo record

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

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getHsa()
    {
        return hsa;
    }

    public void setHsa(String hsa)
    {
        this.hsa = hsa ;
    }

    public String getPrimary_back()
    {
        return primary_back;
    }

    public void setPrimary_back(String primary_back)
    {
        this.primary_back = primary_back ;
    }

    public String getSecondary_back()
    {
        return secondary_back;
    }

    public void setSecondary_back(String secondary_back)
    {
        this.secondary_back = secondary_back ;
    }

    public String getStream()
    {
        return stream;
    }

    public void setStream(String stream)
    {
        this.stream = stream ;
    }

    public double getBf()
    {
        return bf;
    }

    public void setBf(double bf)
    {
        this.bf = bf ;
    }

    public double getWstg()
    {
        return wstg;
    }

    public void setWstg(double wstg)
    {
        this.wstg = wstg ;
    }

    public double getFs()
    {
        return fs;
    }

    public void setFs(double fs)
    {
        this.fs = fs ;
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

    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public String getUse_latest_fcst()
    {
        return use_latest_fcst;
    }

    public void setUse_latest_fcst(String use_latest_fcst)
    {
        this.use_latest_fcst = use_latest_fcst ;
    }

    public String getProximity()
    {
        return proximity;
    }

    public void setProximity(String proximity)
    {
        this.proximity = proximity ;
    }

    public String getReach()
    {
        return reach;
    }

    public void setReach(String reach)
    {
        this.reach = reach ;
    }

    public String getGroup_id()
    {
        return group_id;
    }

    public void setGroup_id(String group_id)
    {
        this.group_id = group_id ;
    }

    public int getOrdinal()
    {
        return ordinal;
    }

    public void setOrdinal(int ordinal)
    {
        this.ordinal = ordinal ;
    }

    public double getChg_threshold()
    {
        return chg_threshold;
    }

    public void setChg_threshold(double chg_threshold)
    {
        this.chg_threshold = chg_threshold ;
    }

    public String getRec_type()
    {
        return rec_type;
    }

    public void setRec_type(String rec_type)
    {
        this.rec_type = rec_type ;
    }

    public int getBackhrs()
    {
        return backhrs;
    }

    public void setBackhrs(int backhrs)
    {
        this.backhrs = backhrs ;
    }

    public int getForwardhrs()
    {
        return forwardhrs;
    }

    public void setForwardhrs(int forwardhrs)
    {
        this.forwardhrs = forwardhrs ;
    }

    public double getAdjustendhrs()
    {
        return adjustendhrs;
    }

    public void setAdjustendhrs(double adjustendhrs)
    {
        this.adjustendhrs = adjustendhrs ;
    }

    public double getMinor_stage()
    {
        return minor_stage;
    }

    public void setMinor_stage(double minor_stage)
    {
        this.minor_stage = minor_stage ;
    }

    public double getModerate_stage()
    {
        return moderate_stage;
    }

    public void setModerate_stage(double moderate_stage)
    {
        this.moderate_stage = moderate_stage ;
    }

    public double getMajor_stage()
    {
        return major_stage;
    }

    public void setMajor_stage(double major_stage)
    {
        this.major_stage = major_stage ;
    }

    public double getMinor_flow()
    {
        return minor_flow;
    }

    public void setMinor_flow(double minor_flow)
    {
        this.minor_flow = minor_flow ;
    }

    public double getModerate_flow()
    {
        return moderate_flow;
    }

    public void setModerate_flow(double moderate_flow)
    {
        this.moderate_flow = moderate_flow ;
    }

    public double getMajor_flow()
    {
        return major_flow;
    }

    public void setMajor_flow(double major_flow)
    {
        this.major_flow = major_flow ;
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
                getCounty() + " " +
                getState() + " " +
                getHsa() + " " +
                getPrimary_back() + " " +
                getSecondary_back() + " " +
                getStream() + " " +
                getBf() + " " +
                getWstg() + " " +
                getFs() + " " +
                getFq() + " " +
                getAction_flow() + " " +
                getPe() + " " +
                getUse_latest_fcst() + " " +
                getProximity() + " " +
                getReach() + " " +
                getGroup_id() + " " +
                getOrdinal() + " " +
                getChg_threshold() + " " +
                getRec_type() + " " +
                getBackhrs() + " " +
                getForwardhrs() + " " +
                getAdjustendhrs() + " " +
                getMinor_stage() + " " +
                getModerate_stage() + " " +
                getMajor_stage() + " " +
                getMinor_flow() + " " +
                getModerate_flow() + " " +
                getMajor_flow() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FpInfoRecord class

