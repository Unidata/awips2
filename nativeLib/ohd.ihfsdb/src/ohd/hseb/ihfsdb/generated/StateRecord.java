// filename: StateRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              State table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class StateRecord extends DbRecord
{
    private String state;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public StateRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public StateRecord(StateRecord origRecord)
    {
        setState(origRecord.getState());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a State record

    //-----------------------------------------------------------------
    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE state = '" + state + "'" 
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
                getState() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of StateRecord class

