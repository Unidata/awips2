// filename: NetworkRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Network table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class NetworkRecord extends DbRecord
{
    private String network;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public NetworkRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public NetworkRecord(NetworkRecord origRecord)
    {
        setNetwork(origRecord.getNetwork());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Network record

    //-----------------------------------------------------------------
    public String getNetwork()
    {
        return network;
    }

    public void setNetwork(String network)
    {
        this.network = network ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE network = '" + network + "'" 
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
                getNetwork() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of NetworkRecord class

