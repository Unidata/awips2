// filename: PurgeProductRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PurgeProduct table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PurgeProductRecord extends DbRecord
{
    private String product_id;

    private int num_versions;

    private long producttime;

    private long postingtime;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PurgeProductRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PurgeProductRecord(PurgeProductRecord origRecord)
    {
        setProduct_id(origRecord.getProduct_id());
        setNum_versions(origRecord.getNum_versions());
        setProducttime(origRecord.getProducttime());
        setPostingtime(origRecord.getPostingtime());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PurgeProduct record

    //-----------------------------------------------------------------
    public String getProduct_id()
    {
        return product_id;
    }

    public void setProduct_id(String product_id)
    {
        this.product_id = product_id ;
    }

    public int getNum_versions()
    {
        return num_versions;
    }

    public void setNum_versions(int num_versions)
    {
        this.num_versions = num_versions ;
    }

    public long getProducttime()
    {
        return producttime;
    }

    public void setProducttime(long producttime)
    {
        this.producttime = producttime ;
    }

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE product_id = '" + product_id + "'" 
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
                getProduct_id() + " " +
                getNum_versions() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PurgeProductRecord class

