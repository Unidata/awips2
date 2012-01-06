// This is a view record !
// filename: StationListRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              StationList table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class StationListRecord extends DbRecord
{
    private String lid;

    private String name;

    private String firstname;

    private String lastname;

    private String rb;

    private String county;

    private String wfo;

    private String hphone;

    private String ophone;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public StationListRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public StationListRecord(StationListRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setFirstname(origRecord.getFirstname());
        setLastname(origRecord.getLastname());
        setRb(origRecord.getRb());
        setCounty(origRecord.getCounty());
        setWfo(origRecord.getWfo());
        setHphone(origRecord.getHphone());
        setOphone(origRecord.getOphone());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a StationList record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

    public String getFirstname()
    {
        return firstname;
    }

    public void setFirstname(String firstname)
    {
        this.firstname = firstname ;
    }

    public String getLastname()
    {
        return lastname;
    }

    public void setLastname(String lastname)
    {
        this.lastname = lastname ;
    }

    public String getRb()
    {
        return rb;
    }

    public void setRb(String rb)
    {
        this.rb = rb ;
    }

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
    }

    public String getWfo()
    {
        return wfo;
    }

    public void setWfo(String wfo)
    {
        this.wfo = wfo ;
    }

    public String getHphone()
    {
        return hphone;
    }

    public void setHphone(String hphone)
    {
        this.hphone = hphone ;
    }

    public String getOphone()
    {
        return ophone;
    }

    public void setOphone(String ophone)
    {
        this.ophone = ophone ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getLid() + " " +
                getName() + " " +
                getFirstname() + " " +
                getLastname() + " " +
                getRb() + " " +
                getCounty() + " " +
                getWfo() + " " +
                getHphone() + " " +
                getOphone() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of StationListRecord class

