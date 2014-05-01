// filename: ShefProbRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ShefProb table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ShefProbRecord extends DbRecord
{
    private String probcode;

    private float probability;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ShefProbRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ShefProbRecord(ShefProbRecord origRecord)
    {
        setProbcode(origRecord.getProbcode());
        setProbability(origRecord.getProbability());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ShefProb record

    //-----------------------------------------------------------------
    public String getProbcode()
    {
        return probcode;
    }

    public void setProbcode(String probcode)
    {
        this.probcode = probcode ;
    }

    public float getProbability()
    {
        return probability;
    }

    public void setProbability(float probability)
    {
        this.probability = probability ;
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
                "WHERE probcode = '" + probcode + "'" 
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
                getProbcode() + " " +
                getProbability() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ShefProbRecord class

