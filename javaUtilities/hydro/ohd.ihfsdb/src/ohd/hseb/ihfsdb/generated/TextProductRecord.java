// filename: TextProductRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              TextProduct table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class TextProductRecord extends DbRecord
{
    private String product_id;

    private long producttime;

    private long postingtime;

    private String prodtype;

    private int issnum;

    private String product;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public TextProductRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public TextProductRecord(TextProductRecord origRecord)
    {
        setProduct_id(origRecord.getProduct_id());
        setProducttime(origRecord.getProducttime());
        setPostingtime(origRecord.getPostingtime());
        setProdtype(origRecord.getProdtype());
        setIssnum(origRecord.getIssnum());
        setProduct(origRecord.getProduct());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a TextProduct record

    //-----------------------------------------------------------------
    public String getProduct_id()
    {
        return product_id;
    }

    public void setProduct_id(String product_id)
    {
        this.product_id = product_id ;
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

    public String getProdtype()
    {
        return prodtype;
    }

    public void setProdtype(String prodtype)
    {
        this.prodtype = prodtype ;
    }

    public int getIssnum()
    {
        return issnum;
    }

    public void setIssnum(int issnum)
    {
        this.issnum = issnum ;
    }

    public String getProduct()
    {
        return product;
    }

    public void setProduct(String product)
    {
        this.product = product ;
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
                 + " AND producttime = '" +  getDateTimeStringFromLongTime(producttime) + "'" 
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
                getProduct_id() + " " +
                getDateTimeStringFromLongTime(getProducttime()) + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getProdtype() + " " +
                getIssnum() + " " +
                getProduct() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of TextProductRecord class

