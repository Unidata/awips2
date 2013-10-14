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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * Overlap rules composite explaining the overlap rules.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2013            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionRuleDefinitionComposite extends Composite {

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent
     */
    public SubscriptionRuleDefinitionComposite(Composite parent) {
        super(parent, SWT.NONE);

        init();
    }

    /**
     * Initialize the components
     */
    private void init() {
        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(gl);
        this.setLayoutData(gd);

        Label directionsLabel = new Label(this, SWT.NONE);
        directionsLabel.setLayoutData(gd);

        StringBuilder buffer = new StringBuilder();
        buffer.append("Please select a percentage of items between two subscriptions\n");
        buffer.append("that would cause the subscriptions to be considered overlapping.\n");
        buffer.append("\nThe common items are items applicable to every data type.  These can\n");
        buffer.append("changed once for all files or individually depending on the settings.\n");
        buffer.append("\nSelect the rule type to edit by expanding the Subscription Rules item");

        directionsLabel.setText(buffer.toString());
    }
}
