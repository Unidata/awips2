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

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TextWorkstationPreferences extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    private StringFieldEditor hostEditor;

    public TextWorkstationPreferences() {
        super(GRID);
        setPreferenceStore(TextWorkstationConstants.getPreferenceStore());
        setDescription("Specify the host for Text Workstation communication."
                + " Should be the network accessible host of workstation as seen"
                + " in the Text Workstation dialog.");
    }

    @Override
    protected void createFieldEditors() {
        // Make sure we grab the env variable if needed
        TextWorkstationConstants.getId();
        hostEditor = new StringFieldEditor(
                TextWorkstationConstants.P_TEXTWORKSTATION_ID,
                "Text Workstation host: ", getFieldEditorParent()) {

            private String ip;

            @Override
            protected boolean checkState() {
                String host = getTextControl().getText();
                boolean rval = true;
                try {
                    ip = InetAddress.getByName(host).getHostAddress();
                } catch (UnknownHostException e) {
                    rval = false;
                }

                if (rval == false) {
                    getTextControl().setBackground(
                            getShell().getDisplay().getSystemColor(
                                    SWT.COLOR_RED));
                } else {
                    getTextControl().setBackground(
                            getShell().getDisplay().getSystemColor(
                                    SWT.COLOR_WHITE));
                }

                return rval;
            }

            @Override
            protected void doStore() {
                boolean rval = true;
                if (ip != null && ip.startsWith("127.0.")) {
                    rval = MessageDialog
                            .openQuestion(
                                    getShell(),
                                    "Confirm",
                                    "The host string you entered maps to a localhost"
                                            + " ip address, are you sure this is"
                                            + " the host name as seen in the Text Workstation dialog?");
                }
                if (rval) {
                    super.doStore();
                }
            }

        };

        hostEditor
                .setErrorMessage("Unable to validate hostname for text workstation");
        addField(hostEditor);
    }

    @Override
    public void init(IWorkbench workbench) {
        // TODO Auto-generated method stub
    }

}
