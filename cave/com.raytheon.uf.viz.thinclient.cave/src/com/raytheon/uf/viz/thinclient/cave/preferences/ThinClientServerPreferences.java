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
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;

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
 * Jan 14, 2013 1469      bkowal       The hdf5 data directory is no longer a preference.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ThinClientServerPreferences extends FieldEditorPreferencePage {

    private BooleanFieldEditor useProxies;

    private StringFieldEditor pypiesServer;

    private StringFieldEditor servicesServer;

    private Button connectivityButton;

    /**
     * Constructor
     */
    public ThinClientServerPreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setTitle("Thin Client Servers");
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

        useProxies = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_USE_PROXIES,
                "&Use Proxy Servers", getFieldEditorParent());

        addField(useProxies);

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
        connectivityButton.setText("&Check Servers");
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

    @Override
    public void propertyChange(PropertyChangeEvent event) {
        super.propertyChange(event);
        this.updateEnabledFields();
    }

    @Override
    protected void checkState() {
        super.checkState();
        this.updateEnabledFields();
    }

    private void updateEnabledFields() {
        boolean useProxies = this.useProxies.getBooleanValue();
        servicesServer.setEnabled(useProxies, connectivityButton.getParent());
        pypiesServer.setEnabled(useProxies, connectivityButton.getParent());
        connectivityButton.setEnabled(useProxies);
    }

}
