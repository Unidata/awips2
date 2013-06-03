// This is a view record !
// filename: PrevProdRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PrevProd table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PrevProdRecord extends DbRecord
{
    private String product_id;

    private long producttime;

    private long postingtime;

    private String prodtype;

    private int issnum;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PrevProdRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PrevProdRecord(PrevProdRecord origRecord)
    {
        setProduct_id(origRecord.getProduct_id());
        setProducttime(origRecord.getProducttime());
        setPostingtime(origRecord.getPostingtime());
        setProdtype(origRecord.getProdtype());
        setIssnum(origRecord.getIssnum());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PrevProd record

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
                "" ;
        return outString;
    } // end toString()
} // end of PrevProdRecord class

