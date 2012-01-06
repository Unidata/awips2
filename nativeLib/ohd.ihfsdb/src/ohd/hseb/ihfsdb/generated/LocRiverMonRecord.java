// This is a view record !
// filename: LocRiverMonRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocRiverMon table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocRiverMonRecord extends DbRecord
{
    private String lid;

    private String name;

    private String county;

    private String state;

    private String hsa;

    private String stream;

    private double bankfull;

    private double action_stage;

    private double flood_stage;

    private double flood_flow;

    private double action_flow;

    private String primary_pe;

    private String proximity;

    private String reach;

    private double mile;

    private double minor;

    private double moderate;

    private double major;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocRiverMonRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocRiverMonRecord(LocRiverMonRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setCounty(origRecord.getCounty());
        setState(origRecord.getState());
        setHsa(origRecord.getHsa());
        setStream(origRecord.getStream());
        setBankfull(origRecord.getBankfull());
        setAction_stage(origRecord.getAction_stage());
        setFlood_stage(origRecord.getFlood_stage());
        setFlood_flow(origRecord.getFlood_flow());
        setAction_flow(origRecord.getAction_flow());
        setPrimary_pe(origRecord.getPrimary_pe());
        setProximity(origRecord.getProximity());
        setReach(origRecord.getReach());
        setMile(origRecord.getMile());
        setMinor(origRecord.getMinor());
        setModerate(origRecord.getModerate());
        setMajor(origRecord.getMajor());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocRiverMon record

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

    public String getStream()
    {
        return stream;
    }

    public void setStream(String stream)
    {
        this.stream = stream ;
    }

    public double getBankfull()
    {
        return bankfull;
    }

    public void setBankfull(double bankfull)
    {
        this.bankfull = bankfull ;
    }

    public double getAction_stage()
    {
        return action_stage;
    }

    public void setAction_stage(double action_stage)
    {
        this.action_stage = action_stage ;
    }

    public double getFlood_stage()
    {
        return flood_stage;
    }

    public void setFlood_stage(double flood_stage)
    {
        this.flood_stage = flood_stage ;
    }

    public double getFlood_flow()
    {
        return flood_flow;
    }

    public void setFlood_flow(double flood_flow)
    {
        this.flood_flow = flood_flow ;
    }

    public double getAction_flow()
    {
        return action_flow;
    }

    public void setAction_flow(double action_flow)
    {
        this.action_flow = action_flow ;
    }

    public String getPrimary_pe()
    {
        return primary_pe;
    }

    public void setPrimary_pe(String primary_pe)
    {
        this.primary_pe = primary_pe ;
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

    public double getMile()
    {
        return mile;
    }

    public void setMile(double mile)
    {
        this.mile = mile ;
    }

    public double getMinor()
    {
        return minor;
    }

    public void setMinor(double minor)
    {
        this.minor = minor ;
    }

    public double getModerate()
    {
        return moderate;
    }

    public void setModerate(double moderate)
    {
        this.moderate = moderate ;
    }

    public double getMajor()
    {
        return major;
    }

    public void setMajor(double major)
    {
        this.major = major ;
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
                getStream() + " " +
                getBankfull() + " " +
                getAction_stage() + " " +
                getFlood_stage() + " " +
                getFlood_flow() + " " +
                getAction_flow() + " " +
                getPrimary_pe() + " " +
                getProximity() + " " +
                getReach() + " " +
                getMile() + " " +
                getMinor() + " " +
                getModerate() + " " +
                getMajor() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocRiverMonRecord class

