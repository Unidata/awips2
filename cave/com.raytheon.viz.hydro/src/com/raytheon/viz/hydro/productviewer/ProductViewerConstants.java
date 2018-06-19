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

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Constants for the Product Viewer Dialog.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2009  2488       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0 
 */
public class ProductViewerConstants extends HydroConstants {
    /**
     * Number of days in the past to look back.
     */
    public static final int LOOKBACK_DAYS = 180;
    
    /**
     * Product list type enum.
     */
    public static enum ProdListType {
        LOCATION("Products for Selected Location"),
        LATEST("Latest Product by Product Identifier"),
        ALL("Text Products in Database");
        
        private String listType;
        
        ProdListType(String value) {
            listType = value;
        }
        
        public String getStringValue() {
            return listType;
        }
    }
    
    /**
     * Sort type enum.
     */
    public static enum SortType {
        PROD_ID("Product ID"),
        POST_TIME("Posting Time"),
        PROD_TIME("Product Time");
        
        private String sortType;
        
        SortType(String value) {
            sortType = value;
        }
        
        public String getStringValue() {
            return sortType;
        }
    }
}
