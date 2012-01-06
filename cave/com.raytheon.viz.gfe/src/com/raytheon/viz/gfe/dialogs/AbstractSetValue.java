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
package com.raytheon.viz.gfe.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Base class for type specific SetValue classes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009 #1318      randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractSetValue extends Composite {

    protected DataManager dataManager;

    protected Parm parm;

    protected String siteId;

    /**
     * @param parent
     *            a widget which will be the parent of the new instance (cannot
     *            be null)
     * @param style
     *            the style of widget to construct
     */
    protected AbstractSetValue(Composite parent, Parm parm) {
        super(parent, SWT.NONE);

        this.dataManager = parm.getDataManager();
        this.parm = parm;
        this.siteId = parm.getParmID().getDbId().getSiteId();

        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayoutData(layoutData);
        this.setLayout(new GridLayout(1, false));

    }

    /**
     * Get the associated parm
     * 
     * @return the associated parm
     */
    public Parm getParm() {
        return this.parm;
    }
}
