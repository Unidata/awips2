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
package com.raytheon.viz.gfe;

import org.eclipse.core.expressions.PropertyTester;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class QuickSetPropertyTester extends PropertyTester {
    private static final int MAX_BUTTONS = 7;

    private static final int DEFAULT_BUTTONS = 4;

    private static int quickSetButtons = -1;

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.expressions.IPropertyTester#test(java.lang.Object,
     * java.lang.String, java.lang.Object[], java.lang.Object)
     */
    @Override
    public boolean test(Object receiver, String property, Object[] args,
            Object expectedValue) {
        if ("QuickSetButtons".equalsIgnoreCase(property)) {
            return ((Integer) expectedValue).intValue() <= getQuickSetButtons();
        }
        return false;
    }

    private int getQuickSetButtons() {
        if (quickSetButtons < 0) {
            PythonPreferenceStore prefs = Activator.getDefault()
                    .getPreferenceStore();
            if (prefs.contains("QuickSetButtons")) {
                quickSetButtons = prefs.getInt("QuickSetButtons");
                if (quickSetButtons < 1) {
                    quickSetButtons = 0;
                } else if (quickSetButtons > MAX_BUTTONS) {
                    quickSetButtons = MAX_BUTTONS;
                }
            } else {
                quickSetButtons = DEFAULT_BUTTONS;
            }
        }
        return quickSetButtons;
    }

}
