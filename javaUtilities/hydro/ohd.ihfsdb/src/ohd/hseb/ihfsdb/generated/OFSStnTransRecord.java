// filename: OFSStnTransRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              OFSStnTrans table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class OFSStnTransRecord extends DbRecord
{
    private String lid;

    private String ofs_data_type;

    private String shef_source_code;

    private String ofs_lid;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public OFSStnTransRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public OFSStnTransRecord(OFSStnTransRecord origRecord)
    {
        setLid(origRecord.getLid());
        setOfs_data_type(origRecord.getOfs_data_type());
        setShef_source_code(origRecord.getShef_source_code());
        setOfs_lid(origRecord.getOfs_lid());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a OFSStnTrans record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getOfs_data_type()
    {
        return ofs_data_type;
    }

    public void setOfs_data_type(String ofs_data_type)
    {
        this.ofs_data_type = ofs_data_type ;
    }

    public String getShef_source_code()
    {
        return shef_source_code;
    }

    public void setShef_source_code(String shef_source_code)
    {
        this.shef_source_code = shef_source_code ;
    }

    public String getOfs_lid()
    {
        return ofs_lid;
    }

    public void setOfs_lid(String ofs_lid)
    {
        this.ofs_lid = ofs_lid ;
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
                 + " AND ofs_data_type = '" + ofs_data_type + "'" 
                 + " AND shef_source_code = '" + shef_source_code + "'" 
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
                getOfs_data_type() + " " +
                getShef_source_code() + " " +
                getOfs_lid() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of OFSStnTransRecord class

