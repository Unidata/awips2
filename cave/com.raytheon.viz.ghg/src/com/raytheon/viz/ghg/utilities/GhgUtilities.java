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
package com.raytheon.viz.ghg.utilities;

/**
 * Class containing general utility methods for use by the various GHG dialogs.
 * <P>
 * The intent here is to have all methods be static so that no class instance
 * required.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20Jun2008    1157       MW Fegan    Initial creation - method 
 *                                     extracted from data classes.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public final class GhgUtilities {

    /**
     * Constructor. Constructor is private since no instantiations
     * are allowed.
     */
    private GhgUtilities() {
        // Intentionally empty.
    }
    /**
     * Convenience method that makes a copy (clone) of a string array.
     *  
     * @param rhs the array to copy
     * 
     * @return the copy
     */
    public static final String[] arrayClone(String[] rhs) {
        /* short circuit 1 - null input*/
        if (rhs == null) {
            return null;
        }
        /* short circuit 2 - empty array */
        if (rhs.length == 0) {
            return new String[0];
        }
        /* copy the input */
        String[] retVal = new String[rhs.length];
        for (int i = 0; i < rhs.length; i++) {
            retVal[i] = new String(rhs[i]);
        }
        return retVal;
    }

}
