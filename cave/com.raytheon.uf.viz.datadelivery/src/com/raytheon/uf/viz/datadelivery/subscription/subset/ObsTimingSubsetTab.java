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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Subset tab for observation data temporal selection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2012   223      mpduff      Initial creation
 * Sep 24, 2012  1209      djohnson    Remove isValid().
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ObsTimingSubsetTab extends SubsetTab {
    /** Status handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ObsTimingSubsetTab.class);

    /** Parent composite */
    private final Composite parentComp;

    public ObsTimingSubsetTab(Composite parentComp) {
        this.parentComp = parentComp;
        
        init();
    }

    /**
     * Initialize components
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group dateRangeGrp = new Group(parentComp, SWT.NONE);
        dateRangeGrp.setText(" Date Range ");
        dateRangeGrp.setLayout(gl);
        dateRangeGrp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        gl.horizontalSpacing = 2;
        gl.verticalSpacing = 2;
        gl.marginWidth = 2;
        gl.marginHeight = 2;

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
    
        Group incGrp = new Group(parentComp, SWT.NONE);
        incGrp.setText(" Request Increment ");
        incGrp.setLayout(gl);
        incGrp.setLayoutData(gd);

    }
}
