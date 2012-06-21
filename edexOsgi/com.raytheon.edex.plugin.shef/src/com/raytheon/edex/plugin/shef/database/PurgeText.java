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
package com.raytheon.edex.plugin.shef.database;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.shef.ShefSeparator;
import com.raytheon.uf.common.dataplugin.shef.tables.Purgeproduct;
import com.raytheon.uf.common.dataplugin.shef.tables.PurgeproductId;
import com.raytheon.uf.common.dataplugin.shef.tables.Textproduct;
import com.raytheon.uf.common.dataplugin.shef.tables.TextproductId;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class PurgeText {
    /** The logger */
    private final Log log = LogFactory.getLog(getClass());

    private boolean storeText = false; 

    private CoreDao dao = null;

    private boolean daoValid = false;
    
    private Date postDate;
    
    private SimpleDateFormat dFmt;
    
    public PurgeText(Date date) {
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            daoValid = true;
        } catch(Exception e) {
            log.error("Could not create dao ", e);
        }
        postDate = date;
        dFmt = new SimpleDateFormat(ShefConstants.POSTGRES_DATE_STRING);
        dFmt.setTimeZone(SHEFTimezone.GMT_TIMEZONE);
        storeText = AppsDefaults.getInstance().getBoolean(ShefConstants.SHEF_STORETEXT, false);
    }
    
    /**
     * 
     * @param separator
     */
    public void storeTextProduct(ShefSeparator separator) {
        if(daoValid) {
            String traceId = "storeTextProduct";
            String message = null;
            String prodId = "MSGPRODID";
            Date prodDate = null;
            try {
                WMOHeader hdr = separator.getWmoHeader();
                if((hdr != null)&&(hdr.isValid())) {
                    int start = hdr.getMessageDataStart();
                    message = separator.getRawMessage().substring(start);

                    String awipsHdr = separator.getAwipsHeader();
                    if(awipsHdr != null) {
                        if(awipsHdr.length() > 6) {
                            log.error("AWIPS Header [" + awipsHdr + "] too long, truncating to 6 characters");
                            awipsHdr = awipsHdr.substring(0,6);
                        }
                    } else {
                        awipsHdr = "";
                    }
                    prodId = hdr.getCccc() + awipsHdr;
                    traceId = separator.getTraceId();
                    
                    if(hdr.getHeaderDate() != null) {
                        prodDate = hdr.getHeaderDate().getTime();
                    }
                    
                } else {
                    message = separator.getRawMessage();
                }
                if(prodDate == null) {
                    prodDate = new Date();
                }
                if(message != null) {
                    storeTextProduct(message,prodId,prodDate,traceId);
                }
                
            } catch(Exception e) {
                log.error(traceId + " - Error processing text data.  ", e);
            }
        }
    }
    
    /**
     * Populate the TextProduct database object product_id | character
     * varying(10) | not null producttime | timestamp without time zone | not
     * null postingtime | timestamp without time zone | not null prodtype |
     * character varying(1) | not null issnum | integer | product | text |
     */
    private void storeTextProduct(String message, String productId, Date prodDate, String traceId) {
        
        int num_to_keep = 0;
        List<Purgeproduct> products = getPurgeProduct(productId);
        Purgeproduct p = null;
        PurgeproductId id = null;
        if((products != null) && (products.size() == 1)) {
            // Update the product and posting time for this product.
            p = products.get(0);
            id = p.getId();
            num_to_keep = p.getId().getNumVersions();
            updatePurgeProduct(productId, prodDate, postDate);
        } else {
            // Didn't find an entry so add one
            id = new PurgeproductId();
            p = new Purgeproduct(id);
            id.setProductId(productId);
            id.setNumVersions(num_to_keep);
            id.setProducttime(prodDate);
            id.setPostingtime(postDate);
            dao.saveOrUpdate(p);
        }
        
        if (log.isDebugEnabled()) {
            log.debug("PurgeText.storeTextProduct() called...");
        }

        try {
            // Should we attempt to store the text?
            if (storeText && (num_to_keep > 0)) {
                
                if(storeTextProduct(message,prodDate,productId)) {
                    
                    // Now that we've stored the text, we need to find out how many versions
                    // of this productId are stored and purge any over the num_to_keep.
                    purgeTextProduct(productId,num_to_keep);
                }
            }
        } catch (Exception e) {
            log.error(traceId + " - Error processing text data.  ", e);
        }
    }

    
    //*****************************************************
    //* Support methods.
    //***********************
    
    /**
     * 
     */
    private List<Purgeproduct> getPurgeProduct(String productId) {

        List<Purgeproduct> prods = null;
        String query = String.format("select * from purgeproduct where product_id = '%s'",productId);
        
        Object[] oa = dao.executeSQLQuery(query);
        if((oa != null) && (oa.length > 0)) {
            prods = new ArrayList<Purgeproduct>();
            for(Object o : oa) {
                Object[] oa2 = (Object[]) o;
                PurgeproductId id = new PurgeproductId();
                id.setProductId((String) oa2[0]);
                id.setNumVersions((Integer) oa2[1]);
                id.setProducttime(toDate((Timestamp) oa2[2]));
                id.setPostingtime(toDate((Timestamp) oa2[3]));
                prods.add(new Purgeproduct(id));
            }
        }
        return prods;
    }
    
    /**
     * 
     */
    private boolean updatePurgeProduct(String productId, Date prodDate, Date postDate) {
        String qFmt = "update purgeproduct set producttime = '%s', postingtime = '%s' where product_id = '%s'";
        
        String prDate = dFmt.format(prodDate);
        String poDate = dFmt.format(postDate);
        String query = String.format(qFmt,prDate, poDate, productId);
        
        int n = dao.executeSQLUpdate(query);
        if(log.isDebugEnabled()) {
            log.debug("Lines updated = " + n + " from Update query  = " + query);
        }
        return (n > 0);
    }
    
    /**
     * 
     * @param text
     * @param prodDate
     * @param productId
     * @return
     */
    private boolean storeTextProduct(String text, Date prodDate, String productId) {
        
        TextproductId tId = new TextproductId();
        
        tId.setProductId(productId);
        tId.setProducttime(prodDate);
        tId.setPostingtime(postDate);
        
        Textproduct t = new Textproduct(tId, productId);
        t.setProdtype(findProductType(productId));
        t.setIssnum(0);
        t.setProduct(convertMessage(text));
        
        boolean success = false;
        try {
            dao.saveOrUpdate(t);
            success = true;
        } catch(Exception e) {
            log.error("Error saving text ", e);
        }
        return success;
    }
    
    /**
     * Convert carriage control to '\n'.
     * @param message
     * @return
     */
    private String convertMessage(String message) {
        StringBuilder sbOut = new StringBuilder(message.length());
        char last = 0;
        for(int i = 0;i < message.length();i++) {
            char c = message.charAt(i);
            if(c == '\'') {
                sbOut.append("\\");
                sbOut.append(c);
            } else if (c == '\r') {
                if(last != '\n') {
                    sbOut.append("\n");
                }
                c = '\n';
            } else if (c == '\n') {
                if(last != '\n') {
                    sbOut.append("\n");
                }
            } else {
                sbOut.append(c);
            }
            last = c;
        }
        return sbOut.toString();
    }
    
    private void purgeTextProduct(String productId, int numToKeep) {
        String selQuery = "select postingTime from textproduct where product_id = '%s' order by postingtime desc";
        String purQuery = "delete from textproduct where product_id = '%s' and postingtime < '%s'";
        
        String query = String.format(selQuery,productId);

        Timestamp [] t = null; 
        Object[] oa = dao.executeSQLQuery(query);
        if((oa != null) && (oa.length > 0)) {
            t = new Timestamp[oa.length];
            for(int i = 0;i < oa.length;i++) {
                t[i] = (Timestamp) oa[i];
            }
        }
        if(t != null) {
            // Do we need to purge?
            if(t.length > numToKeep) {
                String poDate = dFmt.format(toDate(t[numToKeep-1]));
                query = String.format(purQuery,productId, poDate);

                int n = dao.executeSQLUpdate(query);
            }
        }
    }
    
    /**
     * 
     * @param t
     * @return
     */
    private static Date toDate(Timestamp t) {
        Date d = null;
        if(t != null) {
            d = new Date(t.getTime());
        }
        return d;
    }
    
    /**
     * 
     * @param productId
     * @return
     */
    private static String findProductType(String productId) {
        String productType = "O";
        if ((productId.indexOf("RVF") > -1) || (productId.indexOf("QPF") > -1)) {
            productType = "F";
        } else if ((productId.indexOf("FFG") > -1) || (productId.indexOf("FFH") > -1)) {
            productType = "C";
        }
        return productType;
    }
    
}
