// filename: VTECeventRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              VTECevent table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class VTECeventRecord extends DbRecord
{
    private String geoid;

    private String product_id;

    private long producttime;

    private String productmode;

    private String action;

    private String office_id;

    private String phenom;

    private String signif;

    private short etn;

    private long begintime;

    private long endtime;

    private String severity;

    private String immed_cause;

    private long risetime;

    private long cresttime;

    private long falltime;

    private String record;

    private String risets;

    private String crests;

    private String fallts;

    private double crest_value;

    private long expiretime;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public VTECeventRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public VTECeventRecord(VTECeventRecord origRecord)
    {
        setGeoid(origRecord.getGeoid());
        setProduct_id(origRecord.getProduct_id());
        setProducttime(origRecord.getProducttime());
        setProductmode(origRecord.getProductmode());
        setAction(origRecord.getAction());
        setOffice_id(origRecord.getOffice_id());
        setPhenom(origRecord.getPhenom());
        setSignif(origRecord.getSignif());
        setEtn(origRecord.getEtn());
        setBegintime(origRecord.getBegintime());
        setEndtime(origRecord.getEndtime());
        setSeverity(origRecord.getSeverity());
        setImmed_cause(origRecord.getImmed_cause());
        setRisetime(origRecord.getRisetime());
        setCresttime(origRecord.getCresttime());
        setFalltime(origRecord.getFalltime());
        setRecord(origRecord.getRecord());
        setRisets(origRecord.getRisets());
        setCrests(origRecord.getCrests());
        setFallts(origRecord.getFallts());
        setCrest_value(origRecord.getCrest_value());
        setExpiretime(origRecord.getExpiretime());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a VTECevent record

    //-----------------------------------------------------------------
    public String getGeoid()
    {
        return geoid;
    }

    public void setGeoid(String geoid)
    {
        this.geoid = geoid ;
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

    public String getProductmode()
    {
        return productmode;
    }

    public void setProductmode(String productmode)
    {
        this.productmode = productmode ;
    }

    public String getAction()
    {
        return action;
    }

    public void setAction(String action)
    {
        this.action = action ;
    }

    public String getOffice_id()
    {
        return office_id;
    }

    public void setOffice_id(String office_id)
    {
        this.office_id = office_id ;
    }

    public String getPhenom()
    {
        return phenom;
    }

    public void setPhenom(String phenom)
    {
        this.phenom = phenom ;
    }

    public String getSignif()
    {
        return signif;
    }

    public void setSignif(String signif)
    {
        this.signif = signif ;
    }

    public short getEtn()
    {
        return etn;
    }

    public void setEtn(short etn)
    {
        this.etn = etn ;
    }

    public long getBegintime()
    {
        return begintime;
    }

    public void setBegintime(long begintime)
    {
        this.begintime = begintime ;
    }

    public long getEndtime()
    {
        return endtime;
    }

    public void setEndtime(long endtime)
    {
        this.endtime = endtime ;
    }

    public String getSeverity()
    {
        return severity;
    }

    public void setSeverity(String severity)
    {
        this.severity = severity ;
    }

    public String getImmed_cause()
    {
        return immed_cause;
    }

    public void setImmed_cause(String immed_cause)
    {
        this.immed_cause = immed_cause ;
    }

    public long getRisetime()
    {
        return risetime;
    }

    public void setRisetime(long risetime)
    {
        this.risetime = risetime ;
    }

    public long getCresttime()
    {
        return cresttime;
    }

    public void setCresttime(long cresttime)
    {
        this.cresttime = cresttime ;
    }

    public long getFalltime()
    {
        return falltime;
    }

    public void setFalltime(long falltime)
    {
        this.falltime = falltime ;
    }

    public String getRecord()
    {
        return record;
    }

    public void setRecord(String record)
    {
        this.record = record ;
    }

    public String getRisets()
    {
        return risets;
    }

    public void setRisets(String risets)
    {
        this.risets = risets ;
    }

    public String getCrests()
    {
        return crests;
    }

    public void setCrests(String crests)
    {
        this.crests = crests ;
    }

    public String getFallts()
    {
        return fallts;
    }

    public void setFallts(String fallts)
    {
        this.fallts = fallts ;
    }

    public double getCrest_value()
    {
        return crest_value;
    }

    public void setCrest_value(double crest_value)
    {
        this.crest_value = crest_value ;
    }

    public long getExpiretime()
    {
        return expiretime;
    }

    public void setExpiretime(long expiretime)
    {
        this.expiretime = expiretime ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE geoid = '" + geoid + "'" 
                 + " AND product_id = '" + product_id + "'" 
                 + " AND producttime = '" +  getDateTimeStringFromLongTime(producttime) + "'" 
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
                getGeoid() + " " +
                getProduct_id() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getProductmode() + " " +
                getAction() + " " +
                getOffice_id() + " " +
                getPhenom() + " " +
                getSignif() + " " +
                getEtn() + " " +
                getDateTimeStringFromLongTime(getBegintime()) + " " +
                getDateTimeStringFromLongTime(getEndtime()) + " " +
                getSeverity() + " " +
                getImmed_cause() + " " +
                getDateTimeStringFromLongTime(getRisetime()) + " " +
                getDateTimeStringFromLongTime(getCresttime()) + " " +
                getDateTimeStringFromLongTime(getFalltime()) + " " +
                getRecord() + " " +
                getRisets() + " " +
                getCrests() + " " +
                getFallts() + " " +
                getCrest_value() + " " +
                getDateTimeStringFromLongTime(getExpiretime()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of VTECeventRecord class

