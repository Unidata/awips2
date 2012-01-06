// filename: LocDataLimitsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocDataLimits table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocDataLimitsRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String monthdaystart;

    private String monthdayend;

    private double gross_range_min;

    private double gross_range_max;

    private double reason_range_min;

    private double reason_range_max;

    private double roc_max;

    private double alert_upper_limit;

    private double alert_roc_limit;

    private double alarm_upper_limit;

    private double alarm_roc_limit;

    private double alert_lower_limit;

    private double alarm_lower_limit;

    private double alert_diff_limit;

    private double alarm_diff_limit;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocDataLimitsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocDataLimitsRecord(LocDataLimitsRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setMonthdaystart(origRecord.getMonthdaystart());
        setMonthdayend(origRecord.getMonthdayend());
        setGross_range_min(origRecord.getGross_range_min());
        setGross_range_max(origRecord.getGross_range_max());
        setReason_range_min(origRecord.getReason_range_min());
        setReason_range_max(origRecord.getReason_range_max());
        setRoc_max(origRecord.getRoc_max());
        setAlert_upper_limit(origRecord.getAlert_upper_limit());
        setAlert_roc_limit(origRecord.getAlert_roc_limit());
        setAlarm_upper_limit(origRecord.getAlarm_upper_limit());
        setAlarm_roc_limit(origRecord.getAlarm_roc_limit());
        setAlert_lower_limit(origRecord.getAlert_lower_limit());
        setAlarm_lower_limit(origRecord.getAlarm_lower_limit());
        setAlert_diff_limit(origRecord.getAlert_diff_limit());
        setAlarm_diff_limit(origRecord.getAlarm_diff_limit());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocDataLimits record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public short getDur()
    {
        return dur;
    }

    public void setDur(short dur)
    {
        this.dur = dur ;
    }

    public String getMonthdaystart()
    {
        return monthdaystart;
    }

    public void setMonthdaystart(String monthdaystart)
    {
        this.monthdaystart = monthdaystart ;
    }

    public String getMonthdayend()
    {
        return monthdayend;
    }

    public void setMonthdayend(String monthdayend)
    {
        this.monthdayend = monthdayend ;
    }

    public double getGross_range_min()
    {
        return gross_range_min;
    }

    public void setGross_range_min(double gross_range_min)
    {
        this.gross_range_min = gross_range_min ;
    }

    public double getGross_range_max()
    {
        return gross_range_max;
    }

    public void setGross_range_max(double gross_range_max)
    {
        this.gross_range_max = gross_range_max ;
    }

    public double getReason_range_min()
    {
        return reason_range_min;
    }

    public void setReason_range_min(double reason_range_min)
    {
        this.reason_range_min = reason_range_min ;
    }

    public double getReason_range_max()
    {
        return reason_range_max;
    }

    public void setReason_range_max(double reason_range_max)
    {
        this.reason_range_max = reason_range_max ;
    }

    public double getRoc_max()
    {
        return roc_max;
    }

    public void setRoc_max(double roc_max)
    {
        this.roc_max = roc_max ;
    }

    public double getAlert_upper_limit()
    {
        return alert_upper_limit;
    }

    public void setAlert_upper_limit(double alert_upper_limit)
    {
        this.alert_upper_limit = alert_upper_limit ;
    }

    public double getAlert_roc_limit()
    {
        return alert_roc_limit;
    }

    public void setAlert_roc_limit(double alert_roc_limit)
    {
        this.alert_roc_limit = alert_roc_limit ;
    }

    public double getAlarm_upper_limit()
    {
        return alarm_upper_limit;
    }

    public void setAlarm_upper_limit(double alarm_upper_limit)
    {
        this.alarm_upper_limit = alarm_upper_limit ;
    }

    public double getAlarm_roc_limit()
    {
        return alarm_roc_limit;
    }

    public void setAlarm_roc_limit(double alarm_roc_limit)
    {
        this.alarm_roc_limit = alarm_roc_limit ;
    }

    public double getAlert_lower_limit()
    {
        return alert_lower_limit;
    }

    public void setAlert_lower_limit(double alert_lower_limit)
    {
        this.alert_lower_limit = alert_lower_limit ;
    }

    public double getAlarm_lower_limit()
    {
        return alarm_lower_limit;
    }

    public void setAlarm_lower_limit(double alarm_lower_limit)
    {
        this.alarm_lower_limit = alarm_lower_limit ;
    }

    public double getAlert_diff_limit()
    {
        return alert_diff_limit;
    }

    public void setAlert_diff_limit(double alert_diff_limit)
    {
        this.alert_diff_limit = alert_diff_limit ;
    }

    public double getAlarm_diff_limit()
    {
        return alarm_diff_limit;
    }

    public void setAlarm_diff_limit(double alarm_diff_limit)
    {
        this.alarm_diff_limit = alarm_diff_limit ;
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
                 + " AND pe = '" + pe + "'" 
                 + " AND dur = '" + dur + "'" 
                 + " AND monthdaystart = '" + monthdaystart + "'" 
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
                getPe() + " " +
                getDur() + " " +
                getMonthdaystart() + " " +
                getMonthdayend() + " " +
                getGross_range_min() + " " +
                getGross_range_max() + " " +
                getReason_range_min() + " " +
                getReason_range_max() + " " +
                getRoc_max() + " " +
                getAlert_upper_limit() + " " +
                getAlert_roc_limit() + " " +
                getAlarm_upper_limit() + " " +
                getAlarm_roc_limit() + " " +
                getAlert_lower_limit() + " " +
                getAlarm_lower_limit() + " " +
                getAlert_diff_limit() + " " +
                getAlarm_diff_limit() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocDataLimitsRecord class

