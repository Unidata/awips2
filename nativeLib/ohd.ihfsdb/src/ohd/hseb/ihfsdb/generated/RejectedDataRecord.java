// filename: RejectedDataRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RejectedData table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RejectedDataRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private float probability;

    private long validtime;

    private long basistime;

    private long postingtime;

    private double value;

    private short revision;

    private String shef_qual_code;

    private String product_id;

    private long producttime;

    private int quality_code;

    private String reject_type;

    private String userid;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RejectedDataRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RejectedDataRecord(RejectedDataRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setTs(origRecord.getTs());
        setExtremum(origRecord.getExtremum());
        setProbability(origRecord.getProbability());
        setValidtime(origRecord.getValidtime());
        setBasistime(origRecord.getBasistime());
        setPostingtime(origRecord.getPostingtime());
        setValue(origRecord.getValue());
        setRevision(origRecord.getRevision());
        setShef_qual_code(origRecord.getShef_qual_code());
        setProduct_id(origRecord.getProduct_id());
        setProducttime(origRecord.getProducttime());
        setQuality_code(origRecord.getQuality_code());
        setReject_type(origRecord.getReject_type());
        setUserid(origRecord.getUserid());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RejectedData record

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

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

    public double getValue()
    {
        return value;
    }

    public void setValue(double value)
    {
        this.value = value ;
    }

    public short getRevision()
    {
        return revision;
    }

    public void setRevision(short revision)
    {
        this.revision = revision ;
    }

    public String getShef_qual_code()
    {
        return shef_qual_code;
    }

    public void setShef_qual_code(String shef_qual_code)
    {
        this.shef_qual_code = shef_qual_code ;
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

    public int getQuality_code()
    {
        return quality_code;
    }

    public void setQuality_code(int quality_code)
    {
        this.quality_code = quality_code ;
    }

    public String getReject_type()
    {
        return reject_type;
    }

    public void setReject_type(String reject_type)
    {
        this.reject_type = reject_type ;
    }

    public String getUserid()
    {
        return userid;
    }

    public void setUserid(String userid)
    {
        this.userid = userid ;
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
                 + " AND postingtime = '" +  getDateTimeStringFromLongTime(postingtime) + "'" 
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
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getValue() + " " +
                getRevision() + " " +
                getShef_qual_code() + " " +
                getProduct_id() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getQuality_code() + " " +
                getReject_type() + " " +
                getUserid() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RejectedDataRecord class

