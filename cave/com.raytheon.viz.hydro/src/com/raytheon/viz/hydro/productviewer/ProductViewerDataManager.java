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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydro.productviewer.ProductViewerConstants.ProdListType;
import com.raytheon.viz.hydro.productviewer.ProductViewerConstants.SortType;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;

/**
 * Product Viewer Data Access Manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2009  2488       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ProductViewerDataManager extends HydroDataManager {
    /** Instance of this class */
    private static ProductViewerDataManager instance = null;

    /** Textproduct query */
    private static final String TEXT_PRODUCT_QUERY = "select product_id, producttime, postingtime, product from textproduct";

    /** purgeproduct query */
    private static final String PURGE_PRODUCT_QUERY = "select product_id, producttime, postingtime from purgeproduct";

    /** productlink query */
    private static final String PRODUCT_LINK_QUERY = "select lid, product_id, producttime, postingtime from productlink";

    /** Private constructor */
    private ProductViewerDataManager() {
    }

    /**
     * Get an instance of this class.
     * 
     * @return  
     *      instance
     */
    public static synchronized ProductViewerDataManager getInstance() {
        if (instance == null) {
            instance = new ProductViewerDataManager();
        }

        return instance;
    }

    /**
     * Get product list by location.
     * 
     * @param prodType
     *      The product type
     * @param sortType
     *      The sort type
     * @param prodFilter
     *      The product id to filter on
     * @param lid
     *      The location id
     * @return
     *      List of ProductInfo objects
     */
    public List<ProductInfo> getProductsByLocation(ProdListType prodType, SortType sortType,
            String prodFilter, String lid) {
        List<Object[]> rs = null;
        List<ProductInfo> prodList = new ArrayList<ProductInfo>();

        String where = createWhere(prodType, sortType, prodFilter, lid);
        
        rs = runQuery(PRODUCT_LINK_QUERY + where);

        if ((rs != null) && (rs.size() > 0)) {
            for (Object[] oa : rs) {
                ProductInfo prodInfo = new ProductInfo(oa, ProdListType.LOCATION);
                prodList.add(prodInfo);
            }
        }
        
        return prodList;
    }

    /**
     * Get product list of latest products.
     * 
     * @param prodType
     *      The product type
     * @param sortType
     *      The sort type
     * @param prodFilter
     *      The product id to filter on
     * @param lid
     *      The location id
     * @return
     *      List of ProductInfo objects
     */
    public List<ProductInfo> getLatestProducts(ProdListType prodType, SortType sortType,
            String prodFilter, String lid) {
        List<Object[]> rs = null;
        List<ProductInfo> prodList = new ArrayList<ProductInfo>();

        String where = createWhere(prodType, sortType, prodFilter, lid);

        rs = runQuery(PURGE_PRODUCT_QUERY + where);

        if ((rs != null) && (rs.size() > 0)) {
            for (Object[] oa : rs) {
                ProductInfo prodInfo = new ProductInfo(oa, ProdListType.LATEST);
                prodList.add(prodInfo);
            }
        }
        
        return prodList;
    }

    /**
     * Get product list of all products.
     * 
     * @param prodType
     *      The product type
     * @param sortType
     *      The sort type
     * @param prodFilter
     *      The product id to filter on
     * @param lid
     *      The location id
     * @return
     *      List of ProductInfo objects
     */
    public List<ProductInfo> getAllProducts(ProdListType prodType, SortType sortType,
            String prodFilter, String lid) {
        List<Object[]> rs = null;
        List<ProductInfo> prodList = new ArrayList<ProductInfo>();

        String where = createWhere(prodType, sortType, prodFilter, lid);

        rs = runQuery(TEXT_PRODUCT_QUERY + where);

        if ((rs != null) && (rs.size() > 0)) {
            for (Object[] oa : rs) {
                ProductInfo prodInfo = new ProductInfo(oa, ProdListType.ALL);
                prodList.add(prodInfo);
            }
        }

        return prodList;
    }

    /**
     * Get the text product from the database.
     * 
     * @param prodInfo  
     *      The ProductInfo object
     * @return
     *      The text String of this product
     */
    public String getTextProduct(ProductInfo prodInfo) {
        /* Build the where clause */
        String where = String.format(
                "WHERE product_id = '%s' AND producttime = '%s' "
                        + "AND postingtime='%s'", prodInfo.getProductId(),
                HydroConstants.DATE_FORMAT.format(prodInfo.getProductTime()),
                HydroConstants.DATE_FORMAT.format(prodInfo.getPostingTime()));

        String query = "select product from textproduct " + where;

        ArrayList<Object[]> rs = runQuery(query);

        if ((rs != null) && (rs.size() > 0)) {
            return (String) rs.get(0)[0];
        } else {
            return String.format(
                    "\n\nSelected %s product not available in database.\n",
                    prodInfo.getProductId());
        }
    }

    /**
     * Create the where clause for the query.
     * 
     * @param prodType
     *      The product type
     * @param sortType
     *      The sort type
     * @param prodFilter
     *      The product id to filter on
     * @param lid
     *      The location id
     * @return
     *      The where clause
     */
    private String createWhere(ProdListType prodType, SortType sortType,
            String prodFilter, String lid) {
        String where = "";
        String sortClause = null;
        Calendar now = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date date = SimulatedTime.getSystemTime().getTime();
        now.setTime(date);

        /*
         * set the filter on product id string for the where clause; wildcards
         * are allowed using the asterisk. simply substitute the sql wildcard %
         * for the asterisk.
         */
        if ((prodFilter != null) && (prodFilter.length() > 0)) {
            prodFilter = prodFilter.replaceAll("\\*", "%");
        }

        /*
         * limit the retrieval to products with a postingtime within the
         * specified number of days
         */
        String dateClause = " where (extract(day from timestamp '"
                + HydroConstants.DATE_FORMAT.format(now.getTime())
                + "' - postingtime) < " + ProductViewerConstants.LOOKBACK_DAYS
                + ") ";

        /*
         * set the sort order string for the where clause. when getting for all
         * products, a "unique" query approach is used; because of its nature,
         * the sort is accomplished differently
         */
        if (prodType == ProdListType.ALL) {
            if (sortType.equals(SortType.PROD_TIME.getStringValue())) {
                sortClause = " ORDER BY producttime DESC, postingtime DESC ";
            } else if (sortType.equals(SortType.POST_TIME.getStringValue())) {
                sortClause = " ORDER BY postingtime DESC, producttime DESC ";
            } else { //if (sortType.equals(SortType.PROD_ID.getStringValue())) {
                sortClause = " ORDER BY product_id ASC, producttime DESC ";
            }
        } else {
            if (sortType == SortType.PROD_TIME) {
                sortClause = " ORDER BY producttime DESC ";
            } else if (sortType == SortType.POST_TIME) {
                sortClause = " ORDER BY postingtime DESC ";
            } else {
                sortClause = " ORDER BY product_id ASC ";
            }
        }

        /*
         * if loading products for a given location, load from the ProductLink
         * table
         */
        if (prodType == ProdListType.LOCATION) {
            where = String.format("%s AND lid = '%s' ", dateClause, lid);

            if ((prodFilter != null) && (prodFilter.length() > 0)) {
                where = where.concat(" AND product_id LIKE '" + prodFilter
                        + "' ");
            }

            where = where.concat(sortClause);
        } else if (prodType == ProdListType.LATEST) {
            /*
             * if loading the latest of the products, load from the PurgeProduct
             * table.
             */
            if ((prodFilter != null) && (prodFilter.length() > 0)) {
                where = String.format("%s AND product_id LIKE '%s' ",
                        dateClause, prodFilter);
            } else {
                where = where.concat(dateClause);
            }

            where = where.concat(sortClause);
        } else {
            /*
             * if loading all products, then load from the TextProduct table.
             * because the product text in blobs can potentially be large, only
             * load in the pertinent info using the unique utility function. for
             * this "unique" type query, use the field number for the sort.
             * because of this, the "unique" field specified must correspond to
             * the sort order. therefore use a special sort string for the
             * retrieved concatenated field, and parse and rearrange the string
             * later
             */

            if ((prodFilter != null) && (prodFilter.length() > 0)) {
                where = String.format("%s AND product_id LIKE '%s' ",
                        dateClause, prodFilter);
            } else {
                where = where.concat(dateClause);
            }

            where = where.concat(sortClause);
        }

        return where;
    }
}
