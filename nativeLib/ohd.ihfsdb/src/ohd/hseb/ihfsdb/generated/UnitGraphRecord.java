// filename: UnitGraphRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              UnitGraph table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class UnitGraphRecord extends DbRecord
{
    private String lid;

    private String area_id;

    private String model;

    private int dur;

    private int ordinal;

    private double discharge;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public UnitGraphRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public UnitGraphRecord(UnitGraphRecord origRecord)
    {
        setLid(origRecord.getLid());
        setArea_id(origRecord.getArea_id());
        setModel(origRecord.getModel());
        setDur(origRecord.getDur());
        setOrdinal(origRecord.getOrdinal());
        setDischarge(origRecord.getDischarge());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a UnitGraph record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getArea_id()
    {
        return area_id;
    }

    public void setArea_id(String area_id)
    {
        this.area_id = area_id ;
    }

    public String getModel()
    {
        return model;
    }

    public void setModel(String model)
    {
        this.model = model ;
    }

    public int getDur()
    {
        return dur;
    }

    public void setDur(int dur)
    {
        this.dur = dur ;
    }

    public int getOrdinal()
    {
        return ordinal;
    }

    public void setOrdinal(int ordinal)
    {
        this.ordinal = ordinal ;
    }

    public double getDischarge()
    {
        return discharge;
    }

    public void setDischarge(double discharge)
    {
        this.discharge = discharge ;
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
                 + " AND area_id = '" + area_id + "'" 
                 + " AND model = '" + model + "'" 
                 + " AND dur = '" + dur + "'" 
                 + " AND ordinal = '" + ordinal + "'" 
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
                getArea_id() + " " +
                getModel() + " " +
                getDur() + " " +
                getOrdinal() + " " +
                getDischarge() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of UnitGraphRecord class

