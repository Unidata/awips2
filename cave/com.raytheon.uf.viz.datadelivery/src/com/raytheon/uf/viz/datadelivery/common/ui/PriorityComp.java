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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;

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
 * Jan 25, 2013 1528       djohnson    Use priority enum instead of raw integers.
 * Jun 04, 2013     223    mpduff      Changes for Point Data.
 * Aug 30, 2013    2288    bgonzale    Added display of priority and latency rules.
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

    /** Should the rules for priority and latency be displayed. **/
    private final boolean hasRules;

    /** The latency Rule */
    private final int latencyRule;

    /** The priority Rule */
    private final SubscriptionPriority priorityRule;

    /** The latency value */
    private final int latency;

    /** The priority value */
    private SubscriptionPriority priority;

    private final boolean readOnlyLatency;

    private Label latencyLabel;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param latency
     * @param priority
     * @param readOnlyLatency
     *            is latency editable.
     */
    public PriorityComp(Composite parent, int latency,
            SubscriptionPriority priority, boolean readOnlyLatency) {
        super(parent, SWT.NONE);
        this.hasRules = false;
        this.latencyRule = 0;
        this.latency = latency;
        this.priorityRule = null;
        this.priority = priority;
        this.readOnlyLatency = readOnlyLatency;
        init();
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param latencyRule
     *            configured rule setting to display for latency
     * @param latency
     * @param priorityRule
     *            configured rule setting to display for latency
     * @param priority
     * @param readOnlyLatency
     *            is latency editable.
     */
    public PriorityComp(Composite parent, int latencyRule, int latency,
            SubscriptionPriority priorityRule, SubscriptionPriority priority,
            boolean readOnlyLatency) {
        super(parent, SWT.NONE);
        this.hasRules = true;
        this.latencyRule = latencyRule;
        this.latency = latency;
        this.priorityRule = priorityRule;
        this.priority = priority;
        this.readOnlyLatency = readOnlyLatency;
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
        StringBuilder sb = new StringBuilder(" Priority");
        if (hasRules) {
            sb.append(" (Rule: ");
            sb.append(priorityRule.getPriorityName());
            sb.append(")");
        }
        sb.append(":");
        priorityLbl.setText(sb.toString());

        SubscriptionPriority[] prioritiesArr = SubscriptionPriority.values();
        String[] priorities = new String[prioritiesArr.length];
        for (int i = 0; i < prioritiesArr.length; i++) {
            priorities[i] = prioritiesArr[i].getPriorityName();
        }
        gd = new GridData(285, SWT.DEFAULT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        priorityCombo = new Combo(priorityComp, SWT.READ_ONLY);
        priorityCombo.setItems(priorities);
        priorityCombo.setLayoutData(gd);
        priorityCombo.setToolTipText("Select a priority");
        priorityCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                priority = SubscriptionPriority.fromPriorityName(priorityCombo
                        .getItem(priorityCombo.getSelectionIndex()));
            }
        });
        setPriority(priority);

        Composite latencyComp = new Composite(subPriorityGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        latencyComp.setLayout(gl);
        latencyComp.setLayoutData(gd);

        Label latencyLbl = new Label(latencyComp, SWT.NONE);
        sb = new StringBuilder("Latency in Minutes");
        if (hasRules) {
            sb.append(" (Rule: ");
            sb.append(latencyRule);
            sb.append(")");
        }
        sb.append(":");
        latencyLbl.setText(sb.toString());

        if (readOnlyLatency) {
            latencyLabel = new Label(latencyComp, SWT.BORDER);
            latencyLabel.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                    true, false));
            latencyLabel
                    .setToolTipText("Point Data's latency is the retrieval interval.");
            latencyLabel.setText(String.valueOf(this.latency));
        } else {
            latencyText = new Text(latencyComp, SWT.BORDER);
            latencyText.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false));
            latencyText
                    .setToolTipText("Time in minutes allotted for a subscription to download");
            latencyText.setText(String.valueOf(this.latency));
        }

    }

    /**
     * Get the priority combo box value.
     * 
     * @return priority
     */
    public SubscriptionPriority getPriority() {
        return priority;
    }

    /**
     * Set the priority selection.
     * 
     * @param index
     */
    public void setPriority(SubscriptionPriority priority) {
        priorityCombo.select(priorityCombo.indexOf(priority.getPriorityName()));
        this.priority = priority;
    }

    /**
     * Return the latency value.
     * 
     * @return The latency value entered, -1 if nothing entered or an invalid
     *         entry entered
     */
    public int getLatencyValue() {
        String latency;
        if (latencyText != null) {
            latency = latencyText.getText().trim();
        } else {
            latency = latencyLabel.getText();
        }

        int intLatency;
        try {
            intLatency = Integer.parseInt(latency);
        } catch (NumberFormatException e) {
            intLatency = -1;
        }

        return intLatency;
    }
}
