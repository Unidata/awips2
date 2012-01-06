// This is a view record !
// filename: ServiceTableViewRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ServiceTableView table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ServiceTableViewRecord extends DbRecord
{
    private String lid;

    private String name;

    private String stream;

    private String state;

    private String county;

    private String hsa;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ServiceTableViewRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ServiceTableViewRecord(ServiceTableViewRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setStream(origRecord.getStream());
        setState(origRecord.getState());
        setCounty(origRecord.getCounty());
        setHsa(origRecord.getHsa());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ServiceTableView record

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

    public String getStream()
    {
        return stream;
    }

    public void setStream(String stream)
    {
        this.stream = stream ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
    }

    public String getHsa()
    {
        return hsa;
    }

    public void setHsa(String hsa)
    {
        this.hsa = hsa ;
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
                getStream() + " " +
                getState() + " " +
                getCounty() + " " +
                getHsa() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ServiceTableViewRecord class

