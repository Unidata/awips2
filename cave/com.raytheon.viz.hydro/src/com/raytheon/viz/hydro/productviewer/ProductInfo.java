/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.hydro.productviewer;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.viz.hydro.productviewer.ProductViewerConstants.ProdListType;

/**
 * Data structure to hold the product information.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2009  2488       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class ProductInfo {
    /** Product ID */
    private String productId;

    /** Product Time */
    private Date productTime;

    /** Posting Time */
    private Date postingTime;

    /** Text Product */
    private String product;
    
    /** Date format */
    private static final SimpleDateFormat sdf = new SimpleDateFormat("EEE MM-dd HH:mm:ss");
    
    /** Constructor */
    public ProductInfo(Object[] oa, ProdListType type) {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        populate(oa, type);
    }
    
    /**
     * Populate the data fields.
     * 
     * @param oa
     *      Object[] of data
     * @param type
     *      Product List Type
     */
    private void populate(Object[] oa, ProdListType type) {
        if (type == ProdListType.ALL) {
//            product_id, producttime, postingtime, product
            productId = (String) oa[0];
            productTime = (Date) oa[1];
            postingTime = (Date) oa[2];
            product = (String) oa[3];
        } else if (type == ProdListType.LATEST) {
//            product_id, producttime, postingtime
            productId = (String) oa[0];
            productTime = (Date) oa[1];
            postingTime = (Date) oa[2];
        } else {
//            lid, product_id, producttime, postingtime
            productId = (String) oa[1];
            productTime = (Date) oa[2];
            postingTime = (Date) oa[3];
        }

    }

    /**
     * Get the data in a formatted string.
     * 
     * @return The formatted data in a string format.
     */
    @Override
    public String toString() {
        return String.format("%s %s %s", productId, sdf
                .format(productTime), sdf.format(postingTime));
    }

    /**
     * Get the product ID
     * 
     * @return productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * Get the product time
     * 
     * @return productTime
     */
    public Date getProductTime() {
        return productTime;
    }

    /**
     * Get the posting time
     * 
     * @return postingTime
     */
    public Date getPostingTime() {
        return postingTime;
    }

    /**
     * Get the text product
     * 
     * @return Text Product
     */
    public String getProduct() {
        return product;
    }
    
    /**
     * Get the posting time as a string.
     * 
     * @return
     *      Formatted String
     */
    public String getPostingTimeString() {
        return sdf.format(postingTime);
    }
    
    /**
     * Get the product time as a string.
     * 
     * @return
     *      Formatted String
     */
    public String getProductTimeString() {
        return sdf.format(productTime);
    }
}
