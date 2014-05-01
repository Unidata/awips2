// filename: PostProcessorRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PostProcessor table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PostProcessorRecord extends DbRecord
{
    private String post_processor;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PostProcessorRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PostProcessorRecord(PostProcessorRecord origRecord)
    {
        setPost_processor(origRecord.getPost_processor());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PostProcessor record

    //-----------------------------------------------------------------
    public String getPost_processor()
    {
        return post_processor;
    }

    public void setPost_processor(String post_processor)
    {
        this.post_processor = post_processor ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE post_processor = '" + post_processor + "'" 
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
                getPost_processor() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PostProcessorRecord class

