// filename: FpPrevProdPracticeRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FpPrevProdPractice table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FpPrevProdPracticeRecord extends DbRecord
{
    private String lid;

    private String product_id;

    private String prod_categ;

    private long producttime;

    private String office_id;

    private double obsvalue;

    private long obstime;

    private double max_fcstvalue;

    private long validtime;

    private long basistime;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FpPrevProdPracticeRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FpPrevProdPracticeRecord(FpPrevProdPracticeRecord origRecord)
    {
        setLid(origRecord.getLid());
        setProduct_id(origRecord.getProduct_id());
        setProd_categ(origRecord.getProd_categ());
        setProducttime(origRecord.getProducttime());
        setOffice_id(origRecord.getOffice_id());
        setObsvalue(origRecord.getObsvalue());
        setObstime(origRecord.getObstime());
        setMax_fcstvalue(origRecord.getMax_fcstvalue());
        setValidtime(origRecord.getValidtime());
        setBasistime(origRecord.getBasistime());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FpPrevProdPractice record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getProduct_id()
    {
        return product_id;
    }

    public void setProduct_id(String product_id)
    {
        this.product_id = product_id ;
    }

    public String getProd_categ()
    {
        return prod_categ;
    }

    public void setProd_categ(String prod_categ)
    {
        this.prod_categ = prod_categ ;
    }

    public long getProducttime()
    {
        return producttime;
    }

    public void setProducttime(long producttime)
    {
        this.producttime = producttime ;
    }

    public String getOffice_id()
    {
        return office_id;
    }

    public void setOffice_id(String office_id)
    {
        this.office_id = office_id ;
    }

    public double getObsvalue()
    {
        return obsvalue;
    }

    public void setObsvalue(double obsvalue)
    {
        this.obsvalue = obsvalue ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public double getMax_fcstvalue()
    {
        return max_fcstvalue;
    }

    public void setMax_fcstvalue(double max_fcstvalue)
    {
        this.max_fcstvalue = max_fcstvalue ;
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

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE lid = '" + lid + "'" 
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
                getLid() + " " +
                getProduct_id() + " " +
                getProd_categ() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getOffice_id() + " " +
                getObsvalue() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getMax_fcstvalue() + " " +
                getDateTimeStringFromLongTime(getValidtime()) + " " +
                getDateTimeStringFromLongTime(getBasistime()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FpPrevProdPracticeRecord class

