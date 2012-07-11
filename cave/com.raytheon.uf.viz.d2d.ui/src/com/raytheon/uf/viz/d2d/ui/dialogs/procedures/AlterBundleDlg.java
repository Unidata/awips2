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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.procedures.AlterBundleFactory;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.IAlterBundleContributor;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2010            mschenke     Initial creation
 * Jul 11, 2012 #875       rferrel     Return Value now only set
 *                                      to bundle on Load. Prevents
 *                                      the window's 'x' close from
 *                                      trying to perform a load.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class AlterBundleDlg extends CaveSWTDialog {

    private static class AlterBundleEntry {

        IAlterBundleContributor contributor;

        boolean enabled = false;

        String alterKey;

        String alterValue;

    }

    private List<AlterBundleEntry> entries = new ArrayList<AlterBundleEntry>();

    private Button cancelBtn;

    private Button loadBtn;

    private Bundle bundle;

    protected AlterBundleDlg(Bundle bundle, Shell parentShell) {
        super(parentShell);
        setText("Alter Bundle on Loading");

        this.bundle = bundle;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComposite = new Composite(shell, SWT.NONE);
        GridLayout mainLayout = new GridLayout(3, false);
        mainComposite.setLayout(mainLayout);

        initializeCombos(mainComposite);
        initializeBottomButtons(mainComposite);
    }

    private void initializeCombos(Composite comp) {
        for (IAlterBundleContributor contrib : AlterBundleFactory
                .getContributors()) {
            Map<String, String[]> alterables = contrib.getAlterables();
            final List<AlterBundleEntry> contribEntries = new ArrayList<AlterBundleEntry>(
                    alterables.size());
            // Create Enable button
            final Button enabledBtn = new Button(comp, SWT.CHECK);
            GridData gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
            gd.widthHint = 17;
            enabledBtn.setLayoutData(gd);
            enabledBtn.addSelectionListener(new SelectionAdapter() {
                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org
                 * .eclipse.swt.events.SelectionEvent)
                 */
                @Override
                public void widgetSelected(SelectionEvent e) {
                    for (AlterBundleEntry entry : contribEntries) {
                        entry.enabled = enabledBtn.getSelection();
                    }
                }
            });

            int i = 0;
            for (Entry<String, String[]> entry : alterables.entrySet()) {
                if (i != 0) {
                    new Label(comp, SWT.NONE);
                }
                Label label = new Label(comp, SWT.CENTER);
                label.setText(entry.getKey() + " = ");

                final Combo valueCbo = new Combo(comp, SWT.DROP_DOWN);
                gd = new GridData(SWT.CENTER, SWT.CENTER, false, true);
                valueCbo.setLayoutData(gd);

                String[] values = entry.getValue();
                valueCbo.setItems(values);
                if (values.length > 0) {
                    valueCbo.select(0);
                }

                final AlterBundleEntry abe = new AlterBundleEntry();
                abe.contributor = contrib;
                abe.alterKey = entry.getKey();
                if (values.length > 0) {
                    abe.alterValue = valueCbo.getItem(valueCbo
                            .getSelectionIndex());
                }
                entries.add(abe);
                contribEntries.add(abe);

                valueCbo.addSelectionListener(new SelectionAdapter() {
                    /*
                     * (non-Javadoc)
                     * 
                     * @see
                     * org.eclipse.swt.events.SelectionAdapter#widgetSelected
                     * (org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        abe.alterValue = valueCbo.getItem(valueCbo
                                .getSelectionIndex());
                    }
                });

                ++i;
            }

        }
    }

    private void initializeBottomButtons(Composite comp) {
        Composite buttonComp = new Composite(comp, SWT.NONE);
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        gd.horizontalSpan = 3;
        buttonComp.setLayoutData(gd);

        GridLayout layout = new GridLayout(2, true);
        buttonComp.setLayout(layout);

        loadBtn = new Button(buttonComp, SWT.PUSH);
        loadBtn.setText("Load");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 75;
        loadBtn.setLayoutData(gd);
        loadBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                load();
            }

        });

        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 75;
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                cancel();
            }

        });
    }

    private void load() {
        for (AlterBundleEntry entry : entries) {
            if (entry.enabled) {
                entry.contributor.alterBundle(bundle, entry.alterKey,
                        entry.alterValue);
            }
        }
        setReturnValue(bundle);
        shell.close();
    }

    private void cancel() {
        bundle = null;
        shell.close();
    }
}
