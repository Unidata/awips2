// filename: FloodcatRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Floodcat table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FloodcatRecord extends DbRecord
{
    private String lid;

    private double minor_stage;

    private double moderate_stage;

    private double major_stage;

    private double minor_flow;

    private double moderate_flow;

    private double major_flow;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FloodcatRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FloodcatRecord(FloodcatRecord origRecord)
    {
        setLid(origRecord.getLid());
        setMinor_stage(origRecord.getMinor_stage());
        setModerate_stage(origRecord.getModerate_stage());
        setMajor_stage(origRecord.getMajor_stage());
        setMinor_flow(origRecord.getMinor_flow());
        setModerate_flow(origRecord.getModerate_flow());
        setMajor_flow(origRecord.getMajor_flow());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Floodcat record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
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
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE lid = '" + lid + "'" 
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
                getMinor_stage() + " " +
                getModerate_stage() + " " +
                getMajor_stage() + " " +
                getMinor_flow() + " " +
                getModerate_flow() + " " +
                getMajor_flow() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FloodcatRecord class

