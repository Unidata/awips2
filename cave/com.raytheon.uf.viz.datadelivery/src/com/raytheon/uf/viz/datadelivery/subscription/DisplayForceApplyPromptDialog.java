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

import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            djohnson     Initial creation
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0	
 */

public class DisplayForceApplyPromptDialog extends CaveSWTDialog {
    private final String dialogTitle;

    private final String message;

    private final int requiredLatency;

    private final IForceApplyPromptDisplayText displayTextStrategy;

    private final Subscription subscription;

    private final Set<String> wouldBeUnscheduledSubscriptions;

    /**
     * Constructor.
     * 
     * @param title
     * @param message
     * @param requiredLatency
     * @param subscription
     * @param wouldBeUnscheduledSubscriptions
     */
    public DisplayForceApplyPromptDialog(String title, String message,
            int requiredLatency,
            IForceApplyPromptDisplayText displayTextStrategy,
            Subscription subscription,
            Set<String> wouldBeUnscheduledSubscriptions) {
        super(displayTextStrategy.getShell());

        this.dialogTitle = title;
        this.message = message;
        this.requiredLatency = requiredLatency;
        this.displayTextStrategy = displayTextStrategy;
        this.subscription = subscription;
        this.wouldBeUnscheduledSubscriptions = wouldBeUnscheduledSubscriptions;
    }

    /**
     * {@inheritDoc}
     * @param subscription 
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        setText(dialogTitle);

        // Initialize layout
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 100;

        Label textLabel = new Label(shell, SWT.WRAP);
        textLabel.setLayoutData(gd);
        textLabel.setText(message);

        // Add radio buttons
        Composite leftComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        leftComp.setLayout(gl);
        GridData gd2 = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        leftComp.setLayoutData(gd2);

        Button[] radios = getRadioButtons(leftComp, requiredLatency,
                displayTextStrategy, subscription);
        radios[0].setSelection(true);
        setReturnValue(ForceApplyPromptResponse.CANCEL);

        // Add a close button
        Composite centeredComp = new Composite(shell, SWT.NONE);
        gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        gd2 = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd2);

        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("OK");
        closeBtn.setLayoutData(gd2);
        closeBtn.addSelectionListener(new SelectionAdapter() {
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

        Button[] buttons = new Button[length];
        for (int i = 0; i < length; i++) {
            final ForceApplyPromptResponse promptResponse = values[i];

            buttons[i] = new Button(composite, SWT.RADIO);
            final String optionDisplayText = displayTextStrategy
                    .getOptionDisplayText(promptResponse, requiredLatency,
                            subscription, wouldBeUnscheduledSubscriptions);

            // Skip any options that return a null display text
            if (optionDisplayText == null) {
                buttons[i].setVisible(false);
                continue;
            }

            buttons[i].setText(optionDisplayText);
            buttons[i].addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    setReturnValue(promptResponse);
                }
            });
        }

        return buttons;
    }
}
