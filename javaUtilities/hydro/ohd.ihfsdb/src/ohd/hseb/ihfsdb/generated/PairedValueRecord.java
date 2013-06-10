// filename: PairedValueRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PairedValue table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PairedValueRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private float probability;

    private long validtime;

    private long basistime;

    private int ref_value;

    private double value;

    private String shef_qual_code;

    private int quality_code;

    private short revision;

    private String product_id;

    private long producttime;

    private long postingtime;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PairedValueRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PairedValueRecord(PairedValueRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setTs(origRecord.getTs());
        setExtremum(origRecord.getExtremum());
        setProbability(origRecord.getProbability());
        setValidtime(origRecord.getValidtime());
        setBasistime(origRecord.getBasistime());
        setRef_value(origRecord.getRef_value());
        setValue(origRecord.getValue());
        setShef_qual_code(origRecord.getShef_qual_code());
        setQuality_code(origRecord.getQuality_code());
        setRevision(origRecord.getRevision());
        setProduct_id(origRecord.getProduct_id());
        setProducttime(origRecord.getProducttime());
        setPostingtime(origRecord.getPostingtime());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PairedValue record

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

    public int getRef_value()
    {
        return ref_value;
    }

    public void setRef_value(int ref_value)
    {
        this.ref_value = ref_value ;
    }

    public double getValue()
    {
        return value;
    }

    public void setValue(double value)
    {
        this.value = value ;
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
                 + " AND ref_value = '" + ref_value + "'" 
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
                getRef_value() + " " +
                getValue() + " " +
                getShef_qual_code() + " " +
                getQuality_code() + " " +
                getRevision() + " " +
                getProduct_id() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PairedValueRecord class

