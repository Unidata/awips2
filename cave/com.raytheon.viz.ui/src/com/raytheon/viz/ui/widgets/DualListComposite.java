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
package com.raytheon.viz.ui.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;

/**
 * Dual list widget composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2013     1040   mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DualListComposite extends Composite {

    private DualList dl;

    private final DualListConfig config;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite
     * @param config
     *            Dual list config
     */
    public DualListComposite(Composite parent, DualListConfig config) {
        super(parent, SWT.NONE);
        this.config = config;
        init();
    }

    /**
     * Initialize
     */
    private void init() {
        GridLayout gl = new GridLayout(3, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        dl = new DualList(this, SWT.NONE, config);
        dl.setLayout(gl);
        dl.setLayoutData(gd);
    }

    /**
     * Get the selected items.
     * 
     * @return String[] of items in the selected list
     */
    public String[] getSelectedItems() {
        return dl.getSelectedListItems();
    }
}
