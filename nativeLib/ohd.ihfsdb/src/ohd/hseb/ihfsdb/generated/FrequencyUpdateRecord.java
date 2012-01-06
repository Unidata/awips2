// filename: FrequencyUpdateRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FrequencyUpdate table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FrequencyUpdateRecord extends DbRecord
{
    private String frequency_update;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FrequencyUpdateRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FrequencyUpdateRecord(FrequencyUpdateRecord origRecord)
    {
        setFrequency_update(origRecord.getFrequency_update());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FrequencyUpdate record

    //-----------------------------------------------------------------
    public String getFrequency_update()
    {
        return frequency_update;
    }

    public void setFrequency_update(String frequency_update)
    {
        this.frequency_update = frequency_update ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE frequency_update = '" + frequency_update + "'" 
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
                getFrequency_update() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FrequencyUpdateRecord class

