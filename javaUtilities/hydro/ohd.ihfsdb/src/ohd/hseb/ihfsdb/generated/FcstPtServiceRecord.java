// filename: FcstPtServiceRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FcstPtService table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FcstPtServiceRecord extends DbRecord
{
    private String lid;

    private double flood_thres;

    private short exceed_prob;

    private String service_type;

    private long anal_start_date;

    private long anal_end_date;

    private long impl_date;

    private long web_date;

    private String verif_resp_type;

    private double drainage_area;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FcstPtServiceRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FcstPtServiceRecord(FcstPtServiceRecord origRecord)
    {
        setLid(origRecord.getLid());
        setFlood_thres(origRecord.getFlood_thres());
        setExceed_prob(origRecord.getExceed_prob());
        setService_type(origRecord.getService_type());
        setAnal_start_date(origRecord.getAnal_start_date());
        setAnal_end_date(origRecord.getAnal_end_date());
        setImpl_date(origRecord.getImpl_date());
        setWeb_date(origRecord.getWeb_date());
        setVerif_resp_type(origRecord.getVerif_resp_type());
        setDrainage_area(origRecord.getDrainage_area());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FcstPtService record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public double getFlood_thres()
    {
        return flood_thres;
    }

    public void setFlood_thres(double flood_thres)
    {
        this.flood_thres = flood_thres ;
    }

    public short getExceed_prob()
    {
        return exceed_prob;
    }

    public void setExceed_prob(short exceed_prob)
    {
        this.exceed_prob = exceed_prob ;
    }

    public String getService_type()
    {
        return service_type;
    }

    public void setService_type(String service_type)
    {
        this.service_type = service_type ;
    }

    public long getAnal_start_date()
    {
        return anal_start_date;
    }

    public void setAnal_start_date(long anal_start_date)
    {
        this.anal_start_date = anal_start_date ;
    }

    public long getAnal_end_date()
    {
        return anal_end_date;
    }

    public void setAnal_end_date(long anal_end_date)
    {
        this.anal_end_date = anal_end_date ;
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

    public String getVerif_resp_type()
    {
        return verif_resp_type;
    }

    public void setVerif_resp_type(String verif_resp_type)
    {
        this.verif_resp_type = verif_resp_type ;
    }

    public double getDrainage_area()
    {
        return drainage_area;
    }

    public void setDrainage_area(double drainage_area)
    {
        this.drainage_area = drainage_area ;
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
                getFlood_thres() + " " +
                getExceed_prob() + " " +
                getService_type() + " " +
                getDateStringFromLongTime(getAnal_start_date()) + " " +
                getDateStringFromLongTime(getAnal_end_date()) + " " +
                getDateStringFromLongTime(getImpl_date()) + " " +
                getDateStringFromLongTime(getWeb_date()) + " " +
                getVerif_resp_type() + " " +
                getDrainage_area() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FcstPtServiceRecord class

