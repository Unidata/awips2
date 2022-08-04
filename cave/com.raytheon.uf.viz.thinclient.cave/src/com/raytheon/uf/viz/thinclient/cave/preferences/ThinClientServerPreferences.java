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

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.comm.ConnectivityManager.ConnectivityResult;
import com.raytheon.uf.viz.core.comm.IConnectivityCallback;
import com.raytheon.uf.viz.core.localization.TextOrCombo;
import com.raytheon.uf.viz.core.localization.TextOrComboEditor;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.ThinClientUriUtil;
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
 * Nov 08, 2011            mschenke    Initial creation
 * Jan 14, 2013 1469       bkowal      The hdf5 data directory is no longer a
 *                                     preference.
 * Aug 02, 2013 2202       bsteffen    Add edex specific connectivity checking.
 * Feb 04, 2014 2704       njensen     Only one field for proxy server
 * Jun 26, 2014 3236       njensen     Proxy server can now be text or combo field
 * Oct 08, 2015 4891       njensen     Added tooltip to useProxies
 * Feb 09, 2016 5281       tjensen     Remove option for Use Proxy Server
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ThinClientServerPreferences extends FieldEditorPreferencePage {

    private TextOrComboEditor proxyServer;

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

    @Override
    protected void createFieldEditors() {

        proxyServer = new TextOrComboEditor(getFieldEditorParent(),
                this.getPreferenceStore(),
                ThinClientPreferenceConstants.P_PROXY_ADDRESS,
                ThinClientPreferenceConstants.P_PROXY_SERVER_OPTIONS,
                "&Proxy Address: ");
        proxyServer.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                checkConnectivity();
            }
        });

        proxyServer.setErrorMessage("Cannot connect to proxy server");
        addField(proxyServer);

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
        final ConnectivityResult servicesResult = new ConnectivityResult(false,
                "");
        final ConnectivityResult pypiesResult = new ConnectivityResult(false,
                "");
        String errorMessage = "Cannot connect to proxy server: ";
        boolean serverError = false;

        // check HTTP Server
        TextOrCombo text = proxyServer
                .getTextOrComboControl(getFieldEditorParent());
        String proxyAddr = text.getText().trim();
        ConnectivityManager.checkLocalizationServer(
                ThinClientUriUtil.getServicesAddress(proxyAddr),
                new IConnectivityCallback() {
                    @Override
                    public void connectionChecked(ConnectivityResult results) {
                        servicesResult.hasConnectivity = results.hasConnectivity;
                    }
                });

        // check Pypies Server
        ConnectivityManager.checkHttpServer(
                ThinClientUriUtil.getPypiesAddress(proxyAddr),
                new IConnectivityCallback() {
                    @Override
                    public void connectionChecked(ConnectivityResult results) {
                        pypiesResult.hasConnectivity = results.hasConnectivity;
                    }
                });

        if (servicesResult.hasConnectivity && pypiesResult.hasConnectivity) {
            text.widget.setBackground(null);
            proxyServer.clearErrorMessage();
        } else {
            text.widget.setBackground(Display.getDefault().getSystemColor(
                    SWT.COLOR_RED));
            serverError = true;
        }

        if (serverError) {
            this.setErrorMessage(errorMessage);
        }
    }

}
