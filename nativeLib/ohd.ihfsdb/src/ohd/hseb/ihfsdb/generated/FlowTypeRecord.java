// filename: FlowTypeRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FlowType table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FlowTypeRecord extends DbRecord
{
    private String flowtype;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FlowTypeRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FlowTypeRecord(FlowTypeRecord origRecord)
    {
        setFlowtype(origRecord.getFlowtype());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FlowType record

    //-----------------------------------------------------------------
    public String getFlowtype()
    {
        return flowtype;
    }

    public void setFlowtype(String flowtype)
    {
        this.flowtype = flowtype ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE flowtype = '" + flowtype + "'" 
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
                getFlowtype() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FlowTypeRecord class

