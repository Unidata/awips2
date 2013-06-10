// filename: DefiningIssueCriteriaRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              DefiningIssueCriteria table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class DefiningIssueCriteriaRecord extends DbRecord
{
    private String def_issue_crit;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public DefiningIssueCriteriaRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public DefiningIssueCriteriaRecord(DefiningIssueCriteriaRecord origRecord)
    {
        setDef_issue_crit(origRecord.getDef_issue_crit());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a DefiningIssueCriteria record

    //-----------------------------------------------------------------
    public String getDef_issue_crit()
    {
        return def_issue_crit;
    }

    public void setDef_issue_crit(String def_issue_crit)
    {
        this.def_issue_crit = def_issue_crit ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE def_issue_crit = '" + def_issue_crit + "'" 
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
                getDef_issue_crit() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of DefiningIssueCriteriaRecord class

