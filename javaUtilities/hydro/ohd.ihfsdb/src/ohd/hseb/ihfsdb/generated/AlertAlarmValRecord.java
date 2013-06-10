// filename: AlertAlarmValRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              AlertAlarmVal table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class AlertAlarmValRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private float probability;

    private long validtime;

    private long basistime;

    private double value;

    private double suppl_value;

    private String shef_qual_code;

    private int quality_code;

    private short revision;

    private String product_id;

    private long producttime;

    private long postingtime;

    private long action_time;

    private String aa_categ;

    private String aa_check;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public AlertAlarmValRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public AlertAlarmValRecord(AlertAlarmValRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setTs(origRecord.getTs());
        setExtremum(origRecord.getExtremum());
        setProbability(origRecord.getProbability());
        setValidtime(origRecord.getValidtime());
        setBasistime(origRecord.getBasistime());
        setValue(origRecord.getValue());
        setSuppl_value(origRecord.getSuppl_value());
        setShef_qual_code(origRecord.getShef_qual_code());
        setQuality_code(origRecord.getQuality_code());
        setRevision(origRecord.getRevision());
        setProduct_id(origRecord.getProduct_id());
        setProducttime(origRecord.getProducttime());
        setPostingtime(origRecord.getPostingtime());
        setAction_time(origRecord.getAction_time());
        setAa_categ(origRecord.getAa_categ());
        setAa_check(origRecord.getAa_check());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a AlertAlarmVal record

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

    public String getTs()
    {
        return ts;
    }

    public void setTs(String ts)
    {
        this.ts = ts ;
    }

    public String getExtremum()
    {
        return extremum;
    }

    public void setExtremum(String extremum)
    {
        this.extremum = extremum ;
    }

    public float getProbability()
    {
        return probability;
    }

    public void setProbability(float probability)
    {
        this.probability = probability ;
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

    public double getValue()
    {
        return value;
    }

    public void setValue(double value)
    {
        this.value = value ;
    }

    public double getSuppl_value()
    {
        return suppl_value;
    }

    public void setSuppl_value(double suppl_value)
    {
        this.suppl_value = suppl_value ;
    }

    public String getShef_qual_code()
    {
        return shef_qual_code;
    }

    public void setShef_qual_code(String shef_qual_code)
    {
        this.shef_qual_code = shef_qual_code ;
    }

    public int getQuality_code()
    {
        return quality_code;
    }

    public void setQuality_code(int quality_code)
    {
        this.quality_code = quality_code ;
    }

    public short getRevision()
    {
        return revision;
    }

    public void setRevision(short revision)
    {
        this.revision = revision ;
    }

    public String getProduct_id()
    {
        return product_id;
    }

    public void setProduct_id(String product_id)
    {
        this.product_id = product_id ;
    }

    public long getProducttime()
    {
        return producttime;
    }

    public void setProducttime(long producttime)
    {
        this.producttime = producttime ;
    }

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

    public long getAction_time()
    {
        return action_time;
    }

    public void setAction_time(long action_time)
    {
        this.action_time = action_time ;
    }

    public String getAa_categ()
    {
        return aa_categ;
    }

    public void setAa_categ(String aa_categ)
    {
        this.aa_categ = aa_categ ;
    }

    public String getAa_check()
    {
        return aa_check;
    }

    public void setAa_check(String aa_check)
    {
        this.aa_check = aa_check ;
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
                 + " AND ts = '" + ts + "'" 
                 + " AND extremum = '" + extremum + "'" 
                 + " AND probability = '" + probability + "'" 
                 + " AND validtime = '" +  getDateTimeStringFromLongTime(validtime) + "'" 
                 + " AND basistime = '" +  getDateTimeStringFromLongTime(basistime) + "'" 
                 + " AND aa_categ = '" + aa_categ + "'" 
                 + " AND aa_check = '" + aa_check + "'" 
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
                getTs() + " " +
                getExtremum() + " " +
                getProbability() + " " +
                getDateTimeStringFromLongTime(getValidtime()) + " " +
                getDateTimeStringFromLongTime(getBasistime()) + " " +
                getValue() + " " +
                getSuppl_value() + " " +
                getShef_qual_code() + " " +
                getQuality_code() + " " +
                getRevision() + " " +
                getProduct_id() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getDateTimeStringFromLongTime(getAction_time()) + " " +
                getAa_categ() + " " +
                getAa_check() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of AlertAlarmValRecord class

