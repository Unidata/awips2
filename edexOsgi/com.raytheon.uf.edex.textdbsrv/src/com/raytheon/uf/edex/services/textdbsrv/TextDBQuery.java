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
package com.raytheon.uf.edex.services.textdbsrv;

import static com.raytheon.edex.textdb.dbapi.impl.TextDB.asciiToHex;
import static com.raytheon.edex.textdb.dbapi.impl.TextDB.hexToAscii;

import java.util.ArrayList;

import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;

/**
 * Query for text products based on desired desired criteria.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2008            jkorman     Initial creation
 * 28May2010               cjeanbap    Added operational functionality.
 * 02Aug2010    2187       cjeanbap    Update variable/method signature to be consistent.
 * 22Jan2013    1496      rferrel     Added method clearProductIds
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TextDBQuery {

    private final IQueryTransport queryTransport;

    private String queryViewName = null;

    private String queryOpName = null;

    private String querySubObName = null;

    private String queryTimeFormatName = null;

    private String queryTimeFormat = null;

    private String queryAfosCmd = null;

    private String queryWmoId = null;

    private String querySite = null;

    private String queryHour = null;

    private String queryHdrTime = null;

    private String queryBBB = null;

    private String queryNnnXxx = null;

    private String queryFullDataRead = null;

    private String queryOperationalMode = null;

    // Text product to save.
    private String queryProduct = null;

    private ArrayList<String> productIds = null;

    /**
     * 
     */
    public TextDBQuery(IQueryTransport transport) {
        queryTransport = transport;
    }

    /**
     * @return the queryViewName
     */
    public String getQueryViewName() {
        return queryViewName;
    }

    /**
     * @param queryViewName
     *            the queryViewName to set
     */
    public void setQueryViewName(String viewName) {
        queryViewName = viewName;
    }

    /**
     * @return the queryOpName
     */
    public String getQueryOpName() {
        return queryOpName;
    }

    /**
     * @param queryOpName
     *            the queryOpName to set
     */
    public void setQueryOpName(String opName) {
        queryOpName = opName;
    }

    /**
     * @return the querySubObName
     */
    public String getQuerySubObName() {
        return querySubObName;
    }

    /**
     * @param querySubObName
     *            the querySubObName to set
     */
    public void setQuerySubObName(String subObName) {
        querySubObName = subObName;
    }

    /**
     * @return the queryTimeFormatName
     */
    public String getQueryTimeFormatName() {
        return queryTimeFormatName;
    }

    /**
     * This value should be "DEFAULT", "UNIX", "RAW", or "CLIENT". When
     * specifying "CLIENT", the queryTimeFormat must be set to a valid format
     * string.
     * 
     * @param queryTimeFormatName
     *            the queryTimeFormatName to set
     */
    public void setQueryTimeFormatName(String timeFormatName) {
        queryTimeFormatName = timeFormatName;
    }

    /**
     * @return the queryTimeFormat
     */
    public String getQueryTimeFormat() {
        return queryTimeFormat;
    }

    /**
     * This format string represents the requested format when the time format
     * string must be similar to the following " %1$tb %1$td %1$ty %1$tT GMT".<BR>
     * Note that all time format specifiers must reference parameter 1 using the
     * 1$ notation.
     * 
     * @param queryTimeFormat
     *            The queryTimeFormat to set.
     */
    public void setQueryTimeFormat(String timeFormat) {
        queryTimeFormat = timeFormat;
    }

    /**
     * Add an AFOS product identifier to add to the list of productids to get.
     * 
     * @param productId
     *            An AFOS product identifier to add to the list.
     */
    public void addProductId(String productId) {
        if (productIds == null) {
            productIds = new ArrayList<String>();
        }
        productIds.add(productId);
    }

    /**
     * Remove current productIds. This enables the reuse of query when only the
     * productIds change. This allows a Job to break up a large product id list
     * into chunks so it can check back with the UI thread to continue or cancel
     * doing more queries.
     */
    public void clearProductIds() {
        productIds = null;
    }

    /**
     * 
     * @return the queryAfosCmd
     */
    public String getQueryAfosCmd() {
        return queryAfosCmd;
    }

    /**
     * @param queryAfosCmd
     *            the queryAfosCmd to set
     */
    public void setQueryAfosCmd(String queryAfosCmd) {
        this.queryAfosCmd = queryAfosCmd;
    }

    /**
     * @return the queryWmoId
     */
    public String getQueryWmoId() {
        return queryWmoId;
    }

    /**
     * @param queryWmoId
     *            the queryWmoId to set
     */
    public void setQueryWmoId(String queryWmoId) {
        this.queryWmoId = queryWmoId;
    }

    /**
     * @return the querySite
     */
    public String getQuerySite() {
        return querySite;
    }

    /**
     * @param querySite
     *            the querySite to set
     */
    public void setQuerySite(String querySite) {
        this.querySite = querySite;
    }

    /**
     * @return the queryHour
     */
    public String getQueryHour() {
        return queryHour;
    }

    /**
     * @param queryHour
     *            the queryHour to set
     */
    public void setQueryHour(String queryHour) {
        this.queryHour = queryHour;
    }

    /**
     * @return the queryHdrTime
     */
    public String getQueryHdrTime() {
        return queryHdrTime;
    }

    /**
     * @param queryHdrTime
     *            the queryHdrTime to set
     */
    public void setQueryHdrTime(String queryHdrTime) {
        this.queryHdrTime = queryHdrTime;
    }

    /**
     * @return the queryBBB
     */
    public String getQueryBBB() {
        return queryBBB;
    }

    /**
     * @param queryBBB
     *            the queryBBB to set
     */
    public void setQueryBBB(String queryBBB) {
        this.queryBBB = queryBBB;
    }

    /**
     * @return the queryNnnXxx
     */
    public String getQueryNnnXxx() {
        return queryNnnXxx;
    }

    /**
     * @param queryNnnXxx
     *            the queryNnnXxx to set
     */
    public void setQueryNnnXxx(String queryNnnXxx) {
        this.queryNnnXxx = queryNnnXxx;
    }

    /**
     * Set the text product to be stored to the textDB.
     * 
     * @param product
     *            A text product to be stored.
     */
    public void setQueryProduct(String product) {
        queryProduct = product;
    }

    public String getQueryFullDataRead() {
        return queryFullDataRead;
    }

    public void setQueryFullDataRead(String queryFullDataRead) {
        this.queryFullDataRead = queryFullDataRead;
    }

    public String getQueryOperationalModeFlag() {
        return queryOperationalMode;
    }

    /**
     * Set the current mode of CAVE, operational/test or practice.
     * 
     * @param operationalMode
     *            true or false as a string representation.
     */
    public void setQueryOperationalMode(String operationalMode) {
        this.queryOperationalMode = operationalMode;
    }

    /**
     * 
     * @return
     */
    private Message assembleQueryMessage() {

        ArrayList<Property> properties = new ArrayList<Property>();
        // Mandatory
        properties.add(new Property("VIEW", queryViewName));
        // Op
        properties.add(new Property("OP", queryOpName));
        if (querySubObName != null) {
            properties.add(new Property("SUBOP", querySubObName));
        }
        if (queryTimeFormatName != null) {
            properties.add(new Property("FORMAT", queryTimeFormatName));

            // Only pass a CLIENT format string if the format name == CLIENTFMT
            if ("CLIENTFMT".equals(queryTimeFormatName)
                    && queryTimeFormat != null) {
                properties.add(new Property("CLIENT", queryTimeFormat));
            } else {
                // need to complain that it's not correct.
            }
        }
        if (productIds != null) {
            for (String productId : productIds) {
                properties.add(new Property("PRODID", productId));
            }
        }
        if (queryProduct != null) {
            properties.add(new Property("PRODUCT", queryProduct));
        }
        if (queryAfosCmd != null) {
            properties.add(new Property("AFOSCMD", queryAfosCmd));
        }
        if (queryWmoId != null) {
            properties.add(new Property("WMOID", queryWmoId));
        }
        if (querySite != null) {
            properties.add(new Property("SITE", querySite));
        }
        if (queryNnnXxx != null) {
            properties.add(new Property("NNNXXX", queryNnnXxx));
        }
        if (queryBBB != null) {
            properties.add(new Property("BBB", queryBBB));
        }
        if (queryHdrTime != null) {
            properties.add(new Property("HDRTIME", queryHdrTime));
        }
        if (queryHour != null) {
            properties.add(new Property("HOUR", queryHour));
        }
        if (queryFullDataRead != null) {
            properties.add(new Property("FULLREAD", queryFullDataRead));
        }
        if (queryOperationalMode != null) {
            properties.add(new Property("OPERATIONAL", queryOperationalMode));
        }

        Message message = new Message();
        Header hdr = new Header();
        message.setHeader(hdr);

        for (Property property : properties) {
            String s = property.getValue();
            if (s != null) {
                property.setValue(asciiToHex(s));
            }
        }
        Property[] p = new Property[properties.size()];
        hdr.setProperties(properties.toArray(p));
        return message;
    }

    /**
     * 
     * @return
     */
    public Message executeQuery() {
        Message message = queryTransport.executeQuery(assembleQueryMessage());
        if (message != null) {
            Header hdr = message.getHeader();
            if (hdr != null) {
                Property[] properties = hdr.getProperties();
                if (properties != null) {
                    for (Property p : properties) {
                        String s = p.getValue();
                        if (s != null) {
                            p.setValue(hexToAscii(s));
                        }
                    }
                }
            }
        }
        return message;
    }

}
