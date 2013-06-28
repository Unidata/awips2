// filename: ShefDurRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ShefDur table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ShefDurRecord extends DbRecord
{
    private short dur;

    private String durcode;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ShefDurRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ShefDurRecord(ShefDurRecord origRecord)
    {
        setDur(origRecord.getDur());
        setDurcode(origRecord.getDurcode());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ShefDur record

    //-----------------------------------------------------------------
    public short getDur()
    {
        return dur;
    }

    public void setDur(short dur)
    {
        this.dur = dur ;
    }

    public String getDurcode()
    {
        return durcode;
    }

    public void setDurcode(String durcode)
    {
        this.durcode = durcode ;
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
                "WHERE dur = '" + dur + "'" 
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
                getDur() + " " +
                getDurcode() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ShefDurRecord class

