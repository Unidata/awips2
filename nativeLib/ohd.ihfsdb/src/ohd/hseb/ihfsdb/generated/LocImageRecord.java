// filename: LocImageRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocImage table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocImageRecord extends DbRecord
{
    private String lid;

    private String imageid;

    private String title;

    private String descr;

    private String format;

    private String url_internal;

    private String url_external;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocImageRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocImageRecord(LocImageRecord origRecord)
    {
        setLid(origRecord.getLid());
        setImageid(origRecord.getImageid());
        setTitle(origRecord.getTitle());
        setDescr(origRecord.getDescr());
        setFormat(origRecord.getFormat());
        setUrl_internal(origRecord.getUrl_internal());
        setUrl_external(origRecord.getUrl_external());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocImage record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getImageid()
    {
        return imageid;
    }

    public void setImageid(String imageid)
    {
        this.imageid = imageid ;
    }

    public String getTitle()
    {
        return title;
    }

    public void setTitle(String title)
    {
        this.title = title ;
    }

    public String getDescr()
    {
        return descr;
    }

    public void setDescr(String descr)
    {
        this.descr = descr ;
    }

    public String getFormat()
    {
        return format;
    }

    public void setFormat(String format)
    {
        this.format = format ;
    }

    public String getUrl_internal()
    {
        return url_internal;
    }

    public void setUrl_internal(String url_internal)
    {
        this.url_internal = url_internal ;
    }

    public String getUrl_external()
    {
        return url_external;
    }

    public void setUrl_external(String url_external)
    {
        this.url_external = url_external ;
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
                 + " AND imageid = '" + imageid + "'" 
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
                getImageid() + " " +
                getTitle() + " " +
                getDescr() + " " +
                getFormat() + " " +
                getUrl_internal() + " " +
                getUrl_external() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocImageRecord class

