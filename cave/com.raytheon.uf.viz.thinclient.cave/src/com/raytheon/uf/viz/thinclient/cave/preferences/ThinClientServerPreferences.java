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

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;

import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.comm.ConnectivityManager.ConnectivityResult;
import com.raytheon.uf.viz.core.comm.IConnectivityCallback;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Thin client server preferences
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ThinClientServerPreferences extends FieldEditorPreferencePage {

    private StringFieldEditor pypiesServer;

    private StringFieldEditor servicesServer;

    private Button connectivityButton;

    /**
     * Constructor
     */
    public ThinClientServerPreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Thin Client Servers (changes require a restart)");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors
     * ()
     */
    @Override
    protected void createFieldEditors() {
        createServerEditors();
    }

    private void setProxiesEnabled(boolean enabled) {
        servicesServer.setEnabled(enabled, connectivityButton.getParent());
        pypiesServer.setEnabled(enabled, connectivityButton.getParent());
        connectivityButton.setEnabled(enabled);
    }

    /**
     * Create the server field editors
     */
    private void createServerEditors() {
        // TODO: Hook in complete disabling of JMS
        addField(new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_DISABLE_JMS, "Disable JMS",
                getFieldEditorParent()));

        // TODO: Hook in proxy setting via dialogs and disabling setting of
        // proxy servers when use proxies is false
        BooleanFieldEditor useProxyServers = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_USE_PROXIES,
                "&Use Proxy Servers", getFieldEditorParent()) {

            @Override
            protected void valueChanged(boolean oldValue, boolean newValue) {
                super.valueChanged(oldValue, newValue);
                setProxiesEnabled(newValue);
            }

            @Override
            protected void doLoad() {
                super.doLoad();
                setProxiesEnabled(getBooleanValue());
            }

            @Override
            protected void doLoadDefault() {
                super.doLoadDefault();
                setProxiesEnabled(getBooleanValue());
            }

        };
        addField(useProxyServers);

        servicesServer = new StringFieldEditor(
                ThinClientPreferenceConstants.P_SERVICES_PROXY,
                "&Services Address: ", getFieldEditorParent());
        servicesServer.setErrorMessage("Cannot connect to Services server");
        addField(servicesServer);

        pypiesServer = new StringFieldEditor(
                ThinClientPreferenceConstants.P_PYPIES_PROXY,
                "&Pypies Address: ", getFieldEditorParent());
        pypiesServer.setErrorMessage("Cannot connect to Pypies server");
        addField(pypiesServer);

        addField(new StringFieldEditor(
                ThinClientPreferenceConstants.P_SERVER_DATA_DIR,
                "&Server Data Dir: ", getFieldEditorParent()));

        addConnectivityButton();
    }

    /**
     * Add button to check server connectivity
     */
    private void addConnectivityButton() {
        connectivityButton = new Button(getFieldEditorParent(), SWT.PUSH);
        GridData gd = new GridData(SWT.RIGHT, SWT.TOP, false, true);
        gd.horizontalSpan = 2;
        connectivityButton.setLayoutData(gd);
        connectivityButton.setText("Check Servers");
        connectivityButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                checkConnectivity();
            }

        });
    }

    /**
     * Check the connectivity of the server field editors
     */
    private void checkConnectivity() {
        final ConnectivityResult result = new ConnectivityResult(false, "");
        String errorMessage = "Cannot connect to proxy server: ";
        boolean serverError = false;

        // check HTTP Server
        Text text = servicesServer.getTextControl(getFieldEditorParent());
        ConnectivityManager.checkServer(text.getText().trim(),
                new IConnectivityCallback() {
                    @Override
                    public void connectionChecked(ConnectivityResult results) {
                        result.hasConnectivity = results.hasConnectivity;
                    }
                });
        if (result.hasConnectivity) {
            text.setBackground(Display.getDefault().getSystemColor(
                    SWT.COLOR_WHITE));
        } else {
            text.setBackground(Display.getDefault().getSystemColor(
                    SWT.COLOR_RED));
            serverError = true;
        }

        // check Pypies Server
        Text textPypies = pypiesServer.getTextControl(getFieldEditorParent());

        ConnectivityManager.checkServer(textPypies.getText().trim(),
                new IConnectivityCallback() {
                    @Override
                    public void connectionChecked(ConnectivityResult results) {
                        result.hasConnectivity = results.hasConnectivity;
                    }
                });
        if (result.hasConnectivity) {
            textPypies.setBackground(Display.getDefault().getSystemColor(
                    SWT.COLOR_WHITE));
        } else {
            textPypies.setBackground(Display.getDefault().getSystemColor(
                    SWT.COLOR_RED));
            serverError = true;
        }

        if (serverError) {
            this.setErrorMessage(errorMessage);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {

    }

}
