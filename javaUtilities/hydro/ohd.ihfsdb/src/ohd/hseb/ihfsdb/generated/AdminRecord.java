// filename: AdminRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Admin table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class AdminRecord extends DbRecord
{
    private String focalpoint;

    private String ofc;

    private String phone;

    private String region;

    private String regno;

    private String cd404;

    private long tenyr;

    private long oneyr;

    private String hsa;

    private short hsa_num;

    private String hb_password;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public AdminRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public AdminRecord(AdminRecord origRecord)
    {
        setFocalpoint(origRecord.getFocalpoint());
        setOfc(origRecord.getOfc());
        setPhone(origRecord.getPhone());
        setRegion(origRecord.getRegion());
        setRegno(origRecord.getRegno());
        setCd404(origRecord.getCd404());
        setTenyr(origRecord.getTenyr());
        setOneyr(origRecord.getOneyr());
        setHsa(origRecord.getHsa());
        setHsa_num(origRecord.getHsa_num());
        setHb_password(origRecord.getHb_password());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Admin record

    //-----------------------------------------------------------------
    public String getFocalpoint()
    {
        return focalpoint;
    }

    public void setFocalpoint(String focalpoint)
    {
        this.focalpoint = focalpoint ;
    }

    public String getOfc()
    {
        return ofc;
    }

    public void setOfc(String ofc)
    {
        this.ofc = ofc ;
    }

    public String getPhone()
    {
        return phone;
    }

    public void setPhone(String phone)
    {
        this.phone = phone ;
    }

    public String getRegion()
    {
        return region;
    }

    public void setRegion(String region)
    {
        this.region = region ;
    }

    public String getRegno()
    {
        return regno;
    }

    public void setRegno(String regno)
    {
        this.regno = regno ;
    }

    public String getCd404()
    {
        return cd404;
    }

    public void setCd404(String cd404)
    {
        this.cd404 = cd404 ;
    }

    public long getTenyr()
    {
        return tenyr;
    }

    public void setTenyr(long tenyr)
    {
        this.tenyr = tenyr ;
    }

    public long getOneyr()
    {
        return oneyr;
    }

    public void setOneyr(long oneyr)
    {
        this.oneyr = oneyr ;
    }

    public String getHsa()
    {
        return hsa;
    }

    public void setHsa(String hsa)
    {
        this.hsa = hsa ;
    }

    public short getHsa_num()
    {
        return hsa_num;
    }

    public void setHsa_num(short hsa_num)
    {
        this.hsa_num = hsa_num ;
    }

    public String getHb_password()
    {
        return hb_password;
    }

    public void setHb_password(String hb_password)
    {
        this.hb_password = hb_password ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE hsa = '" + hsa + "'" 
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
                getFocalpoint() + " " +
                getOfc() + " " +
                getPhone() + " " +
                getRegion() + " " +
                getRegno() + " " +
                getCd404() + " " +
                getDateStringFromLongTime(getTenyr()) + " " +
                getDateStringFromLongTime(getOneyr()) + " " +
                getHsa() + " " +
                getHsa_num() + " " +
                getHb_password() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of AdminRecord class

