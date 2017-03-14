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
package com.raytheon.viz.gfe.textproduct;

import java.io.File;

/**
 * Utilities for text products
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sept 23, 2008            askripsky   Initial creation
 * Sept 09, 2013  #2033     dgilling    Remove dead code.
 * Feb  05, 2016  #5242     dgilling    Remove dead code.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class TextProductUtils {

    // Extension of text products
    public static final String EXTENSION = ".py";

    // Key for products
    public final static String PRODUCT = "PRODUCT";

    // Key for utilities
    public final static String UTILITIES = "UTILITIES";

    // Smart sub type text product
    public final static String SMART = "SMART";

    // Table sub type text product
    public final static String TABLE = "TABLE";

    // Root path for text products
    private final static String TEXT_PRODUCTS_ROOT_PATH = "gfe"
            + File.separator + "userPython";

    // Root path for Products
    public final static String PRODUCTS_PATH = TEXT_PRODUCTS_ROOT_PATH
            + File.separator + "textProducts";

    // Root path for Utilities
    public final static String UTILITIES_PATH = TEXT_PRODUCTS_ROOT_PATH
            + File.separator + "utilities";
}
