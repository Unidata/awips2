// filename: ShefPeRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ShefPe table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ShefPeRecord extends DbRecord
{
    private String pe;

    private String name;

    private String eng_unit;

    private String met_unit;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ShefPeRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ShefPeRecord(ShefPeRecord origRecord)
    {
        setPe(origRecord.getPe());
        setName(origRecord.getName());
        setEng_unit(origRecord.getEng_unit());
        setMet_unit(origRecord.getMet_unit());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ShefPe record

    //-----------------------------------------------------------------
    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

    public String getEng_unit()
    {
        return eng_unit;
    }

    public void setEng_unit(String eng_unit)
    {
        this.eng_unit = eng_unit ;
    }

    public String getMet_unit()
    {
        return met_unit;
    }

    public void setMet_unit(String met_unit)
    {
        this.met_unit = met_unit ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE pe = '" + pe + "'" 
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
                getPe() + " " +
                getName() + " " +
                getEng_unit() + " " +
                getMet_unit() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ShefPeRecord class

