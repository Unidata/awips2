// filename: UserPrefsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              UserPrefs table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class UserPrefsRecord extends DbRecord
{
    private String userid;

    private int title;

    private int statlist;

    private int sortlist;

    private int fieldlist;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public UserPrefsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public UserPrefsRecord(UserPrefsRecord origRecord)
    {
        setUserid(origRecord.getUserid());
        setTitle(origRecord.getTitle());
        setStatlist(origRecord.getStatlist());
        setSortlist(origRecord.getSortlist());
        setFieldlist(origRecord.getFieldlist());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a UserPrefs record

    //-----------------------------------------------------------------
    public String getUserid()
    {
        return userid;
    }

    public void setUserid(String userid)
    {
        this.userid = userid ;
    }

    public int getTitle()
    {
        return title;
    }

    public void setTitle(int title)
    {
        this.title = title ;
    }

    public int getStatlist()
    {
        return statlist;
    }

    public void setStatlist(int statlist)
    {
        this.statlist = statlist ;
    }

    public int getSortlist()
    {
        return sortlist;
    }

    public void setSortlist(int sortlist)
    {
        this.sortlist = sortlist ;
    }

    public int getFieldlist()
    {
        return fieldlist;
    }

    public void setFieldlist(int fieldlist)
    {
        this.fieldlist = fieldlist ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE userid = '" + userid + "'" 
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
                getUserid() + " " +
                getTitle() + " " +
                getStatlist() + " " +
                getSortlist() + " " +
                getFieldlist() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of UserPrefsRecord class

