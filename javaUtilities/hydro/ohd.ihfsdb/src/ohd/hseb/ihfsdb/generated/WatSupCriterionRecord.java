// filename: WatSupCriterionRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              WatSupCriterion table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class WatSupCriterionRecord extends DbRecord
{
    private String watsup_criterion;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public WatSupCriterionRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public WatSupCriterionRecord(WatSupCriterionRecord origRecord)
    {
        setWatsup_criterion(origRecord.getWatsup_criterion());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a WatSupCriterion record

    //-----------------------------------------------------------------
    public String getWatsup_criterion()
    {
        return watsup_criterion;
    }

    public void setWatsup_criterion(String watsup_criterion)
    {
        this.watsup_criterion = watsup_criterion ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE watsup_criterion = '" + watsup_criterion + "'" 
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
                getWatsup_criterion() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of WatSupCriterionRecord class

