// filename: StnClassRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              StnClass table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class StnClassRecord extends DbRecord
{
    private String lid;

    private String disp_class;

    private String dcp;

    private String observer;

    private String telem_type;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public StnClassRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public StnClassRecord(StnClassRecord origRecord)
    {
        setLid(origRecord.getLid());
        setDisp_class(origRecord.getDisp_class());
        setDcp(origRecord.getDcp());
        setObserver(origRecord.getObserver());
        setTelem_type(origRecord.getTelem_type());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a StnClass record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getDisp_class()
    {
        return disp_class;
    }

    public void setDisp_class(String disp_class)
    {
        this.disp_class = disp_class ;
    }

    public String getDcp()
    {
        return dcp;
    }

    public void setDcp(String dcp)
    {
        this.dcp = dcp ;
    }

    public String getObserver()
    {
        return observer;
    }

    public void setObserver(String observer)
    {
        this.observer = observer ;
    }

    public String getTelem_type()
    {
        return telem_type;
    }

    public void setTelem_type(String telem_type)
    {
        this.telem_type = telem_type ;
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
                getDisp_class() + " " +
                getDcp() + " " +
                getObserver() + " " +
                getTelem_type() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of StnClassRecord class

