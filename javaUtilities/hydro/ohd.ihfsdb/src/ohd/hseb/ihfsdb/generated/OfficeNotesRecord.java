// filename: OfficeNotesRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              OfficeNotes table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class OfficeNotesRecord extends DbRecord
{
    private String topic;

    private String id;

    private long datatime;

    private long postingtime;

    private long updatetime;

    private long expiretime;

    private String note;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public OfficeNotesRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public OfficeNotesRecord(OfficeNotesRecord origRecord)
    {
        setTopic(origRecord.getTopic());
        setId(origRecord.getId());
        setDatatime(origRecord.getDatatime());
        setPostingtime(origRecord.getPostingtime());
        setUpdatetime(origRecord.getUpdatetime());
        setExpiretime(origRecord.getExpiretime());
        setNote(origRecord.getNote());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a OfficeNotes record

    //-----------------------------------------------------------------
    public String getTopic()
    {
        return topic;
    }

    public void setTopic(String topic)
    {
        this.topic = topic ;
    }

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id ;
    }

    public long getDatatime()
    {
        return datatime;
    }

    public void setDatatime(long datatime)
    {
        this.datatime = datatime ;
    }

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

    public long getUpdatetime()
    {
        return updatetime;
    }

    public void setUpdatetime(long updatetime)
    {
        this.updatetime = updatetime ;
    }

    public long getExpiretime()
    {
        return expiretime;
    }

    public void setExpiretime(long expiretime)
    {
        this.expiretime = expiretime ;
    }

    public String getNote()
    {
        return note;
    }

    public void setNote(String note)
    {
        this.note = note ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE topic = '" + topic + "'" 
                 + " AND id = '" + id + "'" 
                 + " AND postingtime = '" +  getDateTimeStringFromLongTime(postingtime) + "'" 
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
                getTopic() + " " +
                getId() + " " +
                getDateTimeStringFromLongTime(getDatatime()) + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getDateTimeStringFromLongTime(getUpdatetime()) + " " +
                getDateTimeStringFromLongTime(getExpiretime()) + " " +
                getNote() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of OfficeNotesRecord class

