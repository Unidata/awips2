// filename: DPARadarRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              DPARadar table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class DPARadarRecord extends DbRecord
{
    private String radid;

    private long obstime;

    private short minoff;

    private float maxvalh;

    private float maxvald;

    private float s1_bias_value;

    private long producttime;

    private short nisolbin;

    private short noutint;

    private short noutrep;

    private float areared;

    private float biscanr;

    private int block_bins_reject;

    private int clutter_bins_rej;

    private int bins_smoothed;

    private float scan_bins_filled;

    private float high_elev_angle;

    private float scan_rain_area;

    private short nbadscan;

    private short nhourout;

    private short volcovpat;

    private short opermode;

    private String missper;

    private short supplmess;

    private String grid_filename;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public DPARadarRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public DPARadarRecord(DPARadarRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setObstime(origRecord.getObstime());
        setMinoff(origRecord.getMinoff());
        setMaxvalh(origRecord.getMaxvalh());
        setMaxvald(origRecord.getMaxvald());
        setS1_bias_value(origRecord.getS1_bias_value());
        setProducttime(origRecord.getProducttime());
        setNisolbin(origRecord.getNisolbin());
        setNoutint(origRecord.getNoutint());
        setNoutrep(origRecord.getNoutrep());
        setAreared(origRecord.getAreared());
        setBiscanr(origRecord.getBiscanr());
        setBlock_bins_reject(origRecord.getBlock_bins_reject());
        setClutter_bins_rej(origRecord.getClutter_bins_rej());
        setBins_smoothed(origRecord.getBins_smoothed());
        setScan_bins_filled(origRecord.getScan_bins_filled());
        setHigh_elev_angle(origRecord.getHigh_elev_angle());
        setScan_rain_area(origRecord.getScan_rain_area());
        setNbadscan(origRecord.getNbadscan());
        setNhourout(origRecord.getNhourout());
        setVolcovpat(origRecord.getVolcovpat());
        setOpermode(origRecord.getOpermode());
        setMissper(origRecord.getMissper());
        setSupplmess(origRecord.getSupplmess());
        setGrid_filename(origRecord.getGrid_filename());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a DPARadar record

    //-----------------------------------------------------------------
    public String getRadid()
    {
        return radid;
    }

    public void setRadid(String radid)
    {
        this.radid = radid ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public short getMinoff()
    {
        return minoff;
    }

    public void setMinoff(short minoff)
    {
        this.minoff = minoff ;
    }

    public float getMaxvalh()
    {
        return maxvalh;
    }

    public void setMaxvalh(float maxvalh)
    {
        this.maxvalh = maxvalh ;
    }

    public float getMaxvald()
    {
        return maxvald;
    }

    public void setMaxvald(float maxvald)
    {
        this.maxvald = maxvald ;
    }

    public float getS1_bias_value()
    {
        return s1_bias_value;
    }

    public void setS1_bias_value(float s1_bias_value)
    {
        this.s1_bias_value = s1_bias_value ;
    }

    public long getProducttime()
    {
        return producttime;
    }

    public void setProducttime(long producttime)
    {
        this.producttime = producttime ;
    }

    public short getNisolbin()
    {
        return nisolbin;
    }

    public void setNisolbin(short nisolbin)
    {
        this.nisolbin = nisolbin ;
    }

    public short getNoutint()
    {
        return noutint;
    }

    public void setNoutint(short noutint)
    {
        this.noutint = noutint ;
    }

    public short getNoutrep()
    {
        return noutrep;
    }

    public void setNoutrep(short noutrep)
    {
        this.noutrep = noutrep ;
    }

    public float getAreared()
    {
        return areared;
    }

    public void setAreared(float areared)
    {
        this.areared = areared ;
    }

    public float getBiscanr()
    {
        return biscanr;
    }

    public void setBiscanr(float biscanr)
    {
        this.biscanr = biscanr ;
    }

    public int getBlock_bins_reject()
    {
        return block_bins_reject;
    }

    public void setBlock_bins_reject(int block_bins_reject)
    {
        this.block_bins_reject = block_bins_reject ;
    }

    public int getClutter_bins_rej()
    {
        return clutter_bins_rej;
    }

    public void setClutter_bins_rej(int clutter_bins_rej)
    {
        this.clutter_bins_rej = clutter_bins_rej ;
    }

    public int getBins_smoothed()
    {
        return bins_smoothed;
    }

    public void setBins_smoothed(int bins_smoothed)
    {
        this.bins_smoothed = bins_smoothed ;
    }

    public float getScan_bins_filled()
    {
        return scan_bins_filled;
    }

    public void setScan_bins_filled(float scan_bins_filled)
    {
        this.scan_bins_filled = scan_bins_filled ;
    }

    public float getHigh_elev_angle()
    {
        return high_elev_angle;
    }

    public void setHigh_elev_angle(float high_elev_angle)
    {
        this.high_elev_angle = high_elev_angle ;
    }

    public float getScan_rain_area()
    {
        return scan_rain_area;
    }

    public void setScan_rain_area(float scan_rain_area)
    {
        this.scan_rain_area = scan_rain_area ;
    }

    public short getNbadscan()
    {
        return nbadscan;
    }

    public void setNbadscan(short nbadscan)
    {
        this.nbadscan = nbadscan ;
    }

    public short getNhourout()
    {
        return nhourout;
    }

    public void setNhourout(short nhourout)
    {
        this.nhourout = nhourout ;
    }

    public short getVolcovpat()
    {
        return volcovpat;
    }

    public void setVolcovpat(short volcovpat)
    {
        this.volcovpat = volcovpat ;
    }

    public short getOpermode()
    {
        return opermode;
    }

    public void setOpermode(short opermode)
    {
        this.opermode = opermode ;
    }

    public String getMissper()
    {
        return missper;
    }

    public void setMissper(String missper)
    {
        this.missper = missper ;
    }

    public short getSupplmess()
    {
        return supplmess;
    }

    public void setSupplmess(short supplmess)
    {
        this.supplmess = supplmess ;
    }

    public String getGrid_filename()
    {
        return grid_filename;
    }

    public void setGrid_filename(String grid_filename)
    {
        this.grid_filename = grid_filename ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE radid = '" + radid + "'" 
                 + " AND obstime = '" +  getDateTimeStringFromLongTime(obstime) + "'" 
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
                getRadid() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getMinoff() + " " +
                getMaxvalh() + " " +
                getMaxvald() + " " +
                getS1_bias_value() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getNisolbin() + " " +
                getNoutint() + " " +
                getNoutrep() + " " +
                getAreared() + " " +
                getBiscanr() + " " +
                getBlock_bins_reject() + " " +
                getClutter_bins_rej() + " " +
                getBins_smoothed() + " " +
                getScan_bins_filled() + " " +
                getHigh_elev_angle() + " " +
                getScan_rain_area() + " " +
                getNbadscan() + " " +
                getNhourout() + " " +
                getVolcovpat() + " " +
                getOpermode() + " " +
                getMissper() + " " +
                getSupplmess() + " " +
                getGrid_filename() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of DPARadarRecord class

