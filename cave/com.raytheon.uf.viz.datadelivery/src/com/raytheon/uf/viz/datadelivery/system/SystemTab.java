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
package com.raytheon.uf.viz.datadelivery.system;

import org.eclipse.swt.widgets.Composite;

/**
 * Base class for system management tabs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class SystemTab {

    /** Parent Composite */
    protected final Composite parentComp;

    /**
     * @return the parentComp
     */
    public Composite getParentComp() {
        return parentComp;
    }

    /**
     * Constructor.
     * 
     * @param parentComp
     */
    protected SystemTab(Composite parentComp) {
        this.parentComp = parentComp;
    }

    /**
     * Initialize the tab.
     */
    public abstract void init();

    /**
     * Get the tab's title text.
     * 
     * @return the title text
     */
    public abstract String getTabText();
}
