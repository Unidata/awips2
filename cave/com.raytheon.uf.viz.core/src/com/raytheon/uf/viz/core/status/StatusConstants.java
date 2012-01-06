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
package com.raytheon.uf.viz.core.status;

/**
 * Constants for VizStatus categories and subcategories
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2008  1433       chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class StatusConstants {

    /** Category for general workstation */
    public static final String CATEGORY_WORKSTATION = "WORKSTATION";

    /** Indicates a data availability issue/notification */
    public static final String SUBCATEGORY_DATAAVAIL = "DATA_AVAIL";

    /** Indicates a connectivity issue/notification */
    public static final String SUBCATEGORY_CONNECTIVITY = "CONNECTIVITY";

    /** Indicates a meteoLib functionality issue */
    public static final String SUBCATEGORY_METEOLIB = "METEOROLOGICAL LIBRARY";
}
