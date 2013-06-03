// filename: PointDataPresetsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PointDataPresets table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PointDataPresetsRecord extends DbRecord
{
    private String preset_id;

    private String descr;

    private short preset_rank;

    private String preset_string;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PointDataPresetsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PointDataPresetsRecord(PointDataPresetsRecord origRecord)
    {
        setPreset_id(origRecord.getPreset_id());
        setDescr(origRecord.getDescr());
        setPreset_rank(origRecord.getPreset_rank());
        setPreset_string(origRecord.getPreset_string());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PointDataPresets record

    //-----------------------------------------------------------------
    public String getPreset_id()
    {
        return preset_id;
    }

    public void setPreset_id(String preset_id)
    {
        this.preset_id = preset_id ;
    }

    public String getDescr()
    {
        return descr;
    }

    public void setDescr(String descr)
    {
        this.descr = descr ;
    }

    public short getPreset_rank()
    {
        return preset_rank;
    }

    public void setPreset_rank(short preset_rank)
    {
        this.preset_rank = preset_rank ;
    }

    public String getPreset_string()
    {
        return preset_string;
    }

    public void setPreset_string(String preset_string)
    {
        this.preset_string = preset_string ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE preset_id = '" + preset_id + "'" 
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
                getPreset_id() + " " +
                getDescr() + " " +
                getPreset_rank() + " " +
                getPreset_string() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PointDataPresetsRecord class

