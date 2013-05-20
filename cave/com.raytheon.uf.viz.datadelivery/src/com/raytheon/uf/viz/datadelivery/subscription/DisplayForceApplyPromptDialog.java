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
package com.raytheon.uf.viz.datadelivery.subscription;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog allowing the user to choose how to continue with their subscription
 * creation/modification request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            djohnson     Initial creation
 * May 22, 2013 1650       djohnson     Add more bandwidth information.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DisplayForceApplyPromptDialog extends CaveSWTDialog {

    private final ForceApplyPromptConfiguration configuration;

    /**
     * Constructor.
     * 
     * @param title
     * @param message
     * @param requiredLatency
     * @param subscription
     * @param wouldBeUnscheduledSubscriptions
     */
    public DisplayForceApplyPromptDialog(
            ForceApplyPromptConfiguration configuration) {
        super(configuration.displayTextStrategy.getShell());

        this.configuration = configuration;
    }

    /**
     * {@inheritDoc}
     * 
     * @param subscription
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        setText(configuration.title);

        // Initialize layout
        GridData gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        gd.widthHint = 400;

        Label textLabel = new Label(shell, SWT.WRAP);
        textLabel.setLayoutData(gd);
        textLabel.setText(configuration.message);

        if (configuration.hasUnscheduledSubscriptions()) {
            Composite unscheduledSubscriptionsComp = new Composite(shell,
                    SWT.NONE);
            unscheduledSubscriptionsComp.setLayout(new GridLayout(1, false));
            unscheduledSubscriptionsComp.setLayoutData(new GridData(SWT.FILL,
                    SWT.FILL, true, true));

            final List list = new List(unscheduledSubscriptionsComp, SWT.MULTI
                    | SWT.BORDER);
            list.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
            final SelectionListener listener = new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    list.deselectAll();
                }
            };
            list.setItems(configuration.wouldBeUnscheduledSubscriptions
                    .toArray(new String[configuration.wouldBeUnscheduledSubscriptions
                            .size()]));
            list.addSelectionListener(listener);
        }

        if (configuration.hasBandwidthDetails()) {
            Group group = new Group(shell, SWT.BORDER);
            group.setText("Bandwidth Details");
            group.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
            group.setLayout(new GridLayout(1, false));

            Label rulesLatency = new Label(group, SWT.WRAP);
            rulesLatency.setText("Maximum latency recommended by rules: "
                    + configuration.maximumLatency + " minutes");
            rulesLatency.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                    true));

            Label sizeLabel = new Label(group, SWT.WRAP);
            sizeLabel
                    .setText("Maximum allowed size with current latency: "
                            + SizeUtil
                                    .prettyByteSize(configuration.maximumAllowedSize));
            sizeLabel
                    .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        }

        Composite leftComp = new Composite(shell, SWT.NONE);
        leftComp.setLayout(new GridLayout(1, false));
        leftComp.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true, false));

        Label choiceLabel = new Label(leftComp, SWT.WRAP);
        choiceLabel.setLayoutData(gd);
        choiceLabel.setText("\nWhat would you like to do?\n");

        // Add radio buttons
        Button[] radios = getRadioButtons(leftComp,
                configuration.requiredLatency,
                configuration.displayTextStrategy, configuration.subscription);
        radios[0].setSelection(true);
        setReturnValue(ForceApplyPromptResponse.CANCEL);
        radios[0].setFocus();

        // Add an OK button
        Composite centeredComp = new Composite(shell, SWT.NONE);
        centeredComp.setLayout(new GridLayout(1, false));
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        Button okBtn = new Button(centeredComp, SWT.NONE);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        shell.pack();
    }

    /**
     * Get the radio buttons to display based on available responses.
     * 
     * @param composite
     * @param requiredLatency
     * @param subscription
     * @param displayTextStrategy
     * @param Subscription
     *            subscription
     * @return the radio buttons
     */
    private Button[] getRadioButtons(Composite composite, int requiredLatency,
            IForceApplyPromptDisplayText displayTextStrategy,
            Subscription subscription) {

        final ForceApplyPromptResponse[] values = ForceApplyPromptResponse
                .values();
        final int length = values.length;

        java.util.List<Button> buttons = Lists.newArrayList();
        for (int i = 0; i < length; i++) {
            final ForceApplyPromptResponse promptResponse = values[i];
            final String optionDisplayText = displayTextStrategy
                    .getOptionDisplayText(promptResponse, requiredLatency,
                            subscription,
                            configuration.wouldBeUnscheduledSubscriptions);

            // Skip any options that return a null display text
            if (optionDisplayText != null) {
                Button button = new Button(composite, SWT.RADIO);
                button.setText(optionDisplayText);
                button.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        setReturnValue(promptResponse);
                    }
                });
                buttons.add(button);
            }
        }

        return buttons.toArray(new Button[buttons.size()]);
    }

}
