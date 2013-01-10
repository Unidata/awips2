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
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.SubscriptionPriority;

/**
 * This is the priority group information composite. This class is intended to
 * be extended so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012     702    jpiatt      Initial creation.
 * Aug 21, 2012     712    mpduff      Default to Default, and allow for setting the combo box.
 * Jan 04, 2013    1420    mpduff      Add latency.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class PriorityComp extends Composite {
    /** Group Name combo box. */
    private Combo priorityCombo;

    /** Latency Text field */
    private Text latencyText;

    /** The latency value */
    private final int latency;

    /** The priority value */
    private final int priority;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param latency
     * @param priority
     */
    public PriorityComp(Composite parent, int latency, int priority) {
        super(parent, SWT.NONE);
        this.latency = latency;
        this.priority = priority - 1;
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
        subPriorityGroup.setText("   Priority/Latency  ");

        Composite priorityComp = new Composite(subPriorityGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        priorityComp.setLayoutData(gd);
        priorityComp.setLayout(gl);

        Label priorityLbl = new Label(priorityComp, SWT.NONE);
        priorityLbl.setText(" Priority: ");

        SubscriptionPriority[] prioritiesArr = SubscriptionPriority.values();
        String[] priorities = new String[prioritiesArr.length];
        for (int i = 0; i < prioritiesArr.length; i++) {
            priorities[i] = prioritiesArr[i].getPriorityName();
        }
        gd = new GridData(285, SWT.DEFAULT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        priorityCombo = new Combo(priorityComp, SWT.READ_ONLY);
        priorityCombo.setItems(priorities);
        priorityCombo.select(this.priority);
        priorityCombo.setLayoutData(gd);
        priorityCombo.setToolTipText("Select a priority");

        Composite latencyComp = new Composite(subPriorityGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        latencyComp.setLayout(gl);
        latencyComp.setLayoutData(gd);

        Label latencyLbl = new Label(latencyComp, SWT.NONE);
        latencyLbl.setText("Latency (Minutes):");

        latencyText = new Text(latencyComp, SWT.BORDER);
        latencyText.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        latencyText.setToolTipText("Subscription Latency in Minutes");
        latencyText.setText(String.valueOf(this.latency));
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

    /**
     * Return the latency value.
     * 
     * @return The latency value entered, -1 if nothing entered or an invalid
     *         entry entered
     */
    public int getLatencyValue() {
        String latency = latencyText.getText().trim();
        int intLatency;
        try {
            intLatency = Integer.parseInt(latency);
        } catch (NumberFormatException e) {
            intLatency = -1;
        }

        return intLatency;
    }
}
