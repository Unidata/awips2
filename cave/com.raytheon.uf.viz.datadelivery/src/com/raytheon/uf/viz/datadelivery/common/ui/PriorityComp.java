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
package com.raytheon.uf.viz.datadelivery.common.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
/**
 * This is the priority group information composite. This class is intended 
 * to be extended so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012     702    jpiatt      Initial creation.
 * Aug 21, 2012     712    mpduff      Default to Default, and allow for setting the combo box.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class PriorityComp extends Composite {
    /** Group Name combo box. */
    private Combo priorityCombo;
    
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public PriorityComp(Composite parent) {
        super(parent, SWT.NONE);
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createSubscriptionPriorityGroup();

    }
    
    /**
     * Create the Subscriptions Priority Group
     */
    private void createSubscriptionPriorityGroup() {
  	
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        
        Group subPriorityGroup = new Group(this, SWT.NONE);
        subPriorityGroup.setLayout(gl);
        subPriorityGroup.setLayoutData(gd);
        subPriorityGroup.setText("  Subscription Priority  ");

        Composite priorityComp = new Composite(subPriorityGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        priorityComp.setLayoutData(gd);
        priorityComp.setLayout(gl);
        
        Label priority = new Label(priorityComp, SWT.NONE);
        priority.setText(" Priority: "); 
        
        gd = new GridData(285, SWT.DEFAULT);
        priorityCombo = new Combo(priorityComp, SWT.READ_ONLY);
        priorityCombo.setItems(new String[] { "High", "Default", "Low" });
        priorityCombo.select(1); // Default to the Default setting
        priorityCombo.setLayoutData(gd);
        priorityCombo.setToolTipText("Select a priority");

    }

    /**
     * Get the priority combo box value.
     * 
     * @return priority
     */
    public int getPriorityIndex() {
        return priorityCombo.getSelectionIndex();
    }

    /**
     * Set the priority selection.
     * 
     * @param index
     */
    public void setPriorityIndex(int index) {
        if (index <= priorityCombo.getItemCount()) {
            priorityCombo.select(index);
        }
    }
}
