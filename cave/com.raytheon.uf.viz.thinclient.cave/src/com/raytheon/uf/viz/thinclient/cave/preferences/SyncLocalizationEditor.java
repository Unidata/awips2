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
package com.raytheon.uf.viz.thinclient.cave.preferences;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class SyncLocalizationEditor extends BooleanFieldEditor {

    private Button button;

    public SyncLocalizationEditor(String name, String labelText,
            Composite parent) {
        init(name, labelText);
        createControl(parent);
    }

    protected void adjustForNumColumns(int numColumns) {
        super.adjustForNumColumns(numColumns - 1);
    }

    private void synchronize() {
        new Job("Synchronizing Localization Files") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                IPathManager pathManager = PathManagerFactory
                        .getPathManager(new CAVELocalizationAdapter());
                LocalizationType[] types = { LocalizationType.CAVE_CONFIG,
                        LocalizationType.CAVE_STATIC,
                        LocalizationType.COMMON_STATIC };
                for (LocalizationType type : types) {
                    long startTime = System.currentTimeMillis();
                    LocalizationContext[] contexts = pathManager
                            .getLocalSearchHierarchy(type);
                    LocalizationFile[] files = pathManager.listFiles(contexts,
                            "/", null, true, false);
                    for (LocalizationFile file : files) {
                        if (monitor.isCanceled()) {
                            return Status.OK_STATUS;
                        }
                        if (!file.isDirectory()) {
                            file.getFile();
                        }
                    }
                    long endTime = System.currentTimeMillis();
                    System.out.println("Time to download " + type + ": "
                            + (endTime - startTime) + "ms");
                }
                return Status.OK_STATUS;
            }

        }.schedule();
    }

    protected void doFillIntoGrid(Composite parent, int numColumns) {
        super.doFillIntoGrid(parent, numColumns - 1);
        if (button != null) {
            button.dispose();
        }
        button = new Button(parent, SWT.PUSH);
        button.setText("Sync Files");
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent evt) {
                MessageBox msgBox = new MessageBox(button.getShell(), SWT.YES
                        | SWT.NO | SWT.ICON_QUESTION);
                msgBox.setText("Are you you want to synchronize?");
                msgBox.setMessage("Synchronizing will take lots of time and bandwidth.\n\n Would you like to synchronize now?");
                int result = msgBox.open();
                if (result == SWT.YES) {
                    synchronize();
                }
            }
        });
        button.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
    }

    public int getNumberOfControls() {
        return super.getNumberOfControls() + 1;
    }

    public void setEnabled(boolean enabled) {
        setEnabled(enabled, button.getParent());
    }

    public void setEnabled(boolean enabled, Composite parent) {
        super.setEnabled(enabled, parent);
        if (!enabled) {
            super.getChangeControl(parent).setSelection(false);
        }
        if (button != null) {
            button.setEnabled(getBooleanValue());
        }
    }

    @Override
    protected void doLoad() {
        super.doLoad();
        if (button != null) {
            button.setEnabled(getBooleanValue());
        }
    }

    @Override
    protected void doLoadDefault() {
        super.doLoadDefault();
        if (button != null) {
            button.setEnabled(getBooleanValue());
        }
    }

    @Override
    protected void valueChanged(boolean oldValue, boolean newValue) {
        super.valueChanged(oldValue, newValue);
        if (newValue == true) {
            button.setEnabled(true);
            MessageBox msgBox = new MessageBox(button.getShell(), SWT.YES
                    | SWT.NO | SWT.CANCEL | SWT.ICON_WARNING);
            msgBox.setText("Do you want to synchronize?");
            msgBox.setMessage("If there are files missing locally, your CAVE might not work correctly. It is recommended that you synchronize all localization files with edex before enabling this option. Synchronizing will take lots of time and bandwidth.\n\n Would you like to syncronize now?");
            int result = msgBox.open();
            if (result == SWT.CANCEL) {
                getChangeControl(button.getParent()).setSelection(false);
                button.setEnabled(false);
            } else if (result == SWT.YES) {
                synchronize();
            }

        } else {
            button.setEnabled(false);
        }
    }
}
