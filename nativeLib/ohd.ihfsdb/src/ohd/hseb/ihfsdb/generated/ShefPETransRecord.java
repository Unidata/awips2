// filename: ShefPETransRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ShefPETrans table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ShefPETransRecord extends DbRecord
{
    private String pe;

    private int coded_value;

    private String value_trans;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ShefPETransRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ShefPETransRecord(ShefPETransRecord origRecord)
    {
        setPe(origRecord.getPe());
        setCoded_value(origRecord.getCoded_value());
        setValue_trans(origRecord.getValue_trans());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ShefPETrans record

    //-----------------------------------------------------------------
    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public int getCoded_value()
    {
        return coded_value;
    }

    public void setCoded_value(int coded_value)
    {
        this.coded_value = coded_value ;
    }

    public String getValue_trans()
    {
        return value_trans;
    }

    public void setValue_trans(String value_trans)
    {
        this.value_trans = value_trans ;
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
                 + " AND coded_value = '" + coded_value + "'" 
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
                getCoded_value() + " " +
                getValue_trans() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ShefPETransRecord class

