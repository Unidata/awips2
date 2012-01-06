// filename: ReservoirModelRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ReservoirModel table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ReservoirModelRecord extends DbRecord
{
    private String reservoir_model;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ReservoirModelRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ReservoirModelRecord(ReservoirModelRecord origRecord)
    {
        setReservoir_model(origRecord.getReservoir_model());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ReservoirModel record

    //-----------------------------------------------------------------
    public String getReservoir_model()
    {
        return reservoir_model;
    }

    public void setReservoir_model(String reservoir_model)
    {
        this.reservoir_model = reservoir_model ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE reservoir_model = '" + reservoir_model + "'" 
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
                getReservoir_model() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ReservoirModelRecord class

