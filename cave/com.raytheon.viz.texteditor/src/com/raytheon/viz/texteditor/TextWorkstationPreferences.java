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
package com.raytheon.viz.texteditor;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.Duration;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Text Workstation preferences page in CAVE
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009            mschenke     Initial creation
 * Apr 20, 2020 8137       tgurney      Add Validate button instead of using
 *                                      automatic validation
 *
 * </pre>
 *
 * @author mschenke
 */

public class TextWorkstationPreferences extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private HostStringFieldEditor hostEditor;

    public TextWorkstationPreferences() {
        super(GRID);
        setPreferenceStore(TextWorkstationConstants.getPreferenceStore());
        setDescription("Specify the host for Text Workstation communication."
                + " Should be the network accessible host of workstation as seen"
                + " in the Text Workstation dialog.");
    }

    private class HostStringFieldEditor extends StringFieldEditor {

        public HostStringFieldEditor(String name, String labelText, int width,
                Composite parent) {
            super(name, labelText, width, parent);
        }

        private String ip;

        private boolean resolve(String host) throws InterruptedException {
            try {
                ip = InetAddress.getByName(host).getHostAddress();
                return true;
            } catch (UnknownHostException e) {
                /*
                 * The UI will indicate that the validation failed, so this does
                 * not have to be any higher than debug.
                 */
                statusHandler.debug(e.getLocalizedMessage(), e);
            }
            return false;
        }

        private boolean resolveWithTimeout(String host, Duration timeout)
                throws InterruptedException, ExecutionException {
            ExecutorService executor = Executors.newSingleThreadExecutor();
            try {
                Future<Boolean> hostResolved = executor
                        .submit(() -> resolve(host));
                return hostResolved.get(timeout.toMillis(),
                        TimeUnit.MILLISECONDS);
            } catch (TimeoutException e) {
                statusHandler.warn("Attempt to resolve " + host + " timed out",
                        e);
            } catch (ExecutionException e) {
                statusHandler.warn("Failed to resolve " + host, e);
            } catch (InterruptedException e) {
                // ignore
            } finally {
                executor.shutdownNow();
            }
            return false;
        }

        @Override
        protected void doStore() {
            if (!validate()) {
                return;
            }
            boolean rval = true;
            if (ip != null && ip.startsWith("127.0.")) {
                rval = MessageDialog.openQuestion(getShell(), "Confirm",
                        "The host string you entered maps to a localhost"
                                + " ip address, are you sure this is"
                                + " the host name as seen in the Text Workstation dialog?");
            }
            if (rval) {
                super.doStore();
            }
        }

        public boolean validate() {
            boolean hostResolved = false;
            try {
                String host = getTextControl().getText();
                hostResolved = resolveWithTimeout(host, Duration.ofSeconds(5));
            } catch (Exception e) {
                statusHandler.warn(e.getLocalizedMessage(), e);
            } finally {
                int color = hostResolved ? SWT.COLOR_WHITE : SWT.COLOR_RED;
                getTextControl().setBackground(
                        getShell().getDisplay().getSystemColor(color));
                if (hostResolved) {
                    clearErrorMessage();
                } else {
                    showErrorMessage();
                }
            }
            setValid(hostResolved);
            return hostResolved;
        }
    }

    @Override
    protected void createFieldEditors() {
        // Make sure we grab the env variable if needed
        TextWorkstationConstants.getId();
        hostEditor = new HostStringFieldEditor(
                TextWorkstationConstants.P_TEXTWORKSTATION_ID,
                "Text Workstation host: ", StringFieldEditor.UNLIMITED,
                getFieldEditorParent());
        hostEditor.setErrorMessage(
                "Unable to validate hostname for text workstation");
        addField(hostEditor);
        addValidateBtn();
    }

    private void addValidateBtn() {
        GridData gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        Button validateBtn = new Button(getFieldEditorParent(), SWT.PUSH);
        validateBtn.setText("Validate");
        validateBtn.setLayoutData(gd);
        validateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                hostEditor.validate();
            }
        });
    }

    @Override
    public void init(IWorkbench workbench) {
        // TODO Auto-generated method stub
    }

}
