// filename: FcstPtWatSupRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FcstPtWatSup table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FcstPtWatSupRecord extends DbRecord
{
    private String lid;

    private String watsup_method;

    private String watsup_coord_agency;

    private String frequpd_normal;

    private String period_req;

    private String watsup_crit;

    private String watsup_resp_agency;

    private String customer_desc;

    private long impl_date;

    private long web_date;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FcstPtWatSupRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FcstPtWatSupRecord(FcstPtWatSupRecord origRecord)
    {
        setLid(origRecord.getLid());
        setWatsup_method(origRecord.getWatsup_method());
        setWatsup_coord_agency(origRecord.getWatsup_coord_agency());
        setFrequpd_normal(origRecord.getFrequpd_normal());
        setPeriod_req(origRecord.getPeriod_req());
        setWatsup_crit(origRecord.getWatsup_crit());
        setWatsup_resp_agency(origRecord.getWatsup_resp_agency());
        setCustomer_desc(origRecord.getCustomer_desc());
        setImpl_date(origRecord.getImpl_date());
        setWeb_date(origRecord.getWeb_date());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FcstPtWatSup record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getWatsup_method()
    {
        return watsup_method;
    }

    public void setWatsup_method(String watsup_method)
    {
        this.watsup_method = watsup_method ;
    }

    public String getWatsup_coord_agency()
    {
        return watsup_coord_agency;
    }

    public void setWatsup_coord_agency(String watsup_coord_agency)
    {
        this.watsup_coord_agency = watsup_coord_agency ;
    }

    public String getFrequpd_normal()
    {
        return frequpd_normal;
    }

    public void setFrequpd_normal(String frequpd_normal)
    {
        this.frequpd_normal = frequpd_normal ;
    }

    public String getPeriod_req()
    {
        return period_req;
    }

    public void setPeriod_req(String period_req)
    {
        this.period_req = period_req ;
    }

    public String getWatsup_crit()
    {
        return watsup_crit;
    }

    public void setWatsup_crit(String watsup_crit)
    {
        this.watsup_crit = watsup_crit ;
    }

    public String getWatsup_resp_agency()
    {
        return watsup_resp_agency;
    }

    public void setWatsup_resp_agency(String watsup_resp_agency)
    {
        this.watsup_resp_agency = watsup_resp_agency ;
    }

    public String getCustomer_desc()
    {
        return customer_desc;
    }

    public void setCustomer_desc(String customer_desc)
    {
        this.customer_desc = customer_desc ;
    }

    public long getImpl_date()
    {
        return impl_date;
    }

    public void setImpl_date(long impl_date)
    {
        this.impl_date = impl_date ;
    }

    public long getWeb_date()
    {
        return web_date;
    }

    public void setWeb_date(long web_date)
    {
        this.web_date = web_date ;
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
                 + " AND watsup_method = '" + watsup_method + "'" 
                 + " AND watsup_coord_agency = '" + watsup_coord_agency + "'" 
                 + " AND frequpd_normal = '" + frequpd_normal + "'" 
                 + " AND period_req = '" + period_req + "'" 
                 + " AND watsup_crit = '" + watsup_crit + "'" 
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
                getWatsup_method() + " " +
                getWatsup_coord_agency() + " " +
                getFrequpd_normal() + " " +
                getPeriod_req() + " " +
                getWatsup_crit() + " " +
                getWatsup_resp_agency() + " " +
                getCustomer_desc() + " " +
                getDateStringFromLongTime(getImpl_date()) + " " +
                getDateStringFromLongTime(getWeb_date()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FcstPtWatSupRecord class

