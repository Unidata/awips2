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
package com.raytheon.uf.viz.datadelivery.system;

import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Encryption;
import com.raytheon.uf.common.datadelivery.registry.Encryption.Algorithim;
import com.raytheon.uf.common.datadelivery.registry.Encryption.Padding;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest.Status;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IProviderHandler;
import com.raytheon.uf.common.registry.RegistryConstants;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.widgets.ApplyCancelComposite;
import com.raytheon.viz.ui.widgets.IApplyCancelAction;

/**
 * Data Provider Password settings composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2013   2180     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataProviderPasswordComposite extends Composite implements
        IApplyCancelAction {
    /** Status handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataProviderPasswordComposite.class);

    /** Key text field */
    private Text keyTxt;

    /** Password text field */
    private Text passTxt;

    /** Username text field */
    private Text userTxt;

    /** Button composite */
    private ApplyCancelComposite buttonComp;

    /** Provider name combo box */
    private Combo providerCombo;

    /** List of Provider objects */
    private List<Provider> providerList = Collections.emptyList();

    /** Provider object */
    private Provider provider;

    /** Remove credentials check box */
    private Button removeChk;

    /**
     * Constructor
     * 
     * @param parent
     *            Parent Composite
     * @param style
     *            Style bits
     */
    public DataProviderPasswordComposite(Composite parent, int style) {
        super(parent, style);
        init();
    }

    /**
     * Initialize class
     */
    private void init() {
        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(gl);
        this.setLayoutData(gd);

        gl = new GridLayout(3, false);
        gd = new GridData(SWT.VERTICAL, SWT.DEFAULT, true, false);
        gl.marginTop = 15;
        gl.marginLeft = 15;
        Composite comp = new Composite(this, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        Label providerLabel = new Label(comp, SWT.NONE);
        providerLabel.setText("Provider:");

        providerCombo = new Combo(comp, SWT.READ_ONLY);
        providerCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleProviderSelection();
                checkUserInput();
            }
        });

        Label spacer = new Label(comp, SWT.NONE);
        spacer.setText("");

        Label userLabel = new Label(comp, SWT.NONE);
        userLabel.setText("User Name:");

        userTxt = new Text(comp, SWT.BORDER);
        userTxt.setLayoutData(new GridData(150, SWT.DEFAULT));
        userTxt.addKeyListener(new KeyListener() {
            @Override
            public void keyReleased(KeyEvent e) {
                checkUserInput();
            }

            @Override
            public void keyPressed(KeyEvent e) {
                checkUserInput();
            }
        });

        Label spacer2 = new Label(comp, SWT.NONE);
        spacer2.setText("");

        Label passLabel = new Label(comp, SWT.NONE);
        passLabel.setText("Password:");

        passTxt = new Text(comp, SWT.BORDER);
        passTxt.setLayoutData(new GridData(150, SWT.DEFAULT));
        passTxt.setEchoChar('*');
        passTxt.addKeyListener(new KeyListener() {
            @Override
            public void keyReleased(KeyEvent e) {
                checkUserInput();
            }

            @Override
            public void keyPressed(KeyEvent e) {
                checkUserInput();
            }
        });

        final Button showPassChk = new Button(comp, SWT.CHECK);
        showPassChk.setText("Show password text");
        showPassChk.setSelection(false);
        showPassChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (showPassChk.getSelection()) {
                    passTxt.setEchoChar('\0');
                } else {
                    passTxt.setEchoChar('*');
                }
            }
        });

        Label keyLabel = new Label(comp, SWT.NONE);
        keyLabel.setText("Encryption Key:");

        keyTxt = new Text(comp, SWT.BORDER);
        keyTxt.setLayoutData(new GridData(150, SWT.DEFAULT));
        keyTxt.setEchoChar('*');
        keyTxt.addKeyListener(new KeyListener() {
            @Override
            public void keyReleased(KeyEvent e) {
                checkUserInput();
            }

            @Override
            public void keyPressed(KeyEvent e) {
                checkUserInput();
            }
        });

        final Button showKeyChk = new Button(comp, SWT.CHECK);
        showKeyChk.setText("Show encryption key text");
        showKeyChk.setSelection(false);
        showKeyChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (showKeyChk.getSelection()) {
                    keyTxt.setEchoChar('\0');
                } else {
                    keyTxt.setEchoChar('*');
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 3;
        Label sep = new Label(comp, SWT.SEPARATOR | SWT.SHADOW_IN
                | SWT.HORIZONTAL);
        sep.setLayoutData(gd);

        removeChk = new Button(comp, SWT.CHECK);
        removeChk.setText("Remove Credentials");
        removeChk.setSelection(false);
        removeChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enableWidgets(!removeChk.getSelection());
            }
        });

        // Buttons
        buttonComp = new ApplyCancelComposite(this, SWT.NONE, this);

        load();
    }

    /**
     * Load the settings info.
     */
    public void load() {
        IProviderHandler handler = DataDeliveryHandlers.getProviderHandler();

        try {
            providerList = handler.getAll();
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        for (Provider p : providerList) {
            providerCombo.add(p.getName());
        }

        if (providerCombo.getItemCount() > 0) {
            providerCombo.select(0);
            handleProviderSelection();
            checkUserInput();
        }
    }

    /**
     * Check the text fields to determine of buttons should be enabled or not
     */
    private boolean validation() {
        if ((!userTxt.getText().isEmpty() && !keyTxt.getText().isEmpty() && !passTxt
                .getText().isEmpty()) && providerCombo.getSelectionIndex() > -1) {
            return true;
        }

        return false;
    }

    private void checkUserInput() {
        buttonComp.enableButtons(validation());
    }

    /**
     * Provider selection handler
     */
    private void handleProviderSelection() {
        userTxt.setText("");
        passTxt.setText("");
        keyTxt.setText("");
        String provider = providerCombo.getText();

        for (Provider p : providerList) {
            if (p.getName().equals(provider)) {
                this.provider = p;
                ProviderKeyRequest req = new ProviderKeyRequest();
                req.setRequestType(RequestType.RETRIEVE);
                req.setProvider(p);
                ProviderKeyRequest resp;
                try {
                    resp = (ProviderKeyRequest) RequestRouter.route(req,
                            RegistryConstants.EBXML_REGISTRY_SERVICE);
                    Connection conn = resp.getProvider().getConnection();
                    if (conn != null) {
                        String userName = conn.getUnencryptedUsername();
                        String passwd = conn.getUnencryptedPassword();
                        String key = conn.getProviderKey();
                        if (userName != null) {
                            userTxt.setText(userName);
                        }
                        if (passwd != null) {
                            passTxt.setText(passwd);
                        }
                        if (key != null) {
                            keyTxt.setText(key);
                        }
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error requesting provider information.", e);
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean apply() {
        if (validation()) {

            ProviderKeyRequest req = new ProviderKeyRequest();
            Connection conn = provider.getConnection();
            conn.setPassword(passTxt.getText());
            conn.setUserName(userTxt.getText());
            conn.setProviderKey(keyTxt.getText());
            conn.setEncryption(getEncryption());
            provider.setConnection(conn);
            req.setProvider(provider);
            if (removeChk.getSelection()) {
                req.setRequestType(RequestType.DELETE);
            } else {
                req.setRequestType(RequestType.SAVE);
            }
            req.setProviderKey(keyTxt.getText());

            ProviderKeyRequest resp;
            Status status = Status.FAILURE;
            try {
                resp = (ProviderKeyRequest) RequestRouter.route(req,
                        RegistryConstants.EBXML_REGISTRY_SERVICE);
                status = resp.getStatus();
                if (status == Status.FAILURE) {
                    statusHandler.error(resp.getMessage());
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return false;
            }

            if (status == Status.SUCCESS) {
                DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                        "Change Successful",
                        "The username/password has been updated.");
                if (removeChk.getSelection()) {
                    userTxt.setText("");
                    passTxt.setText("");
                    keyTxt.setText("");
                }
            }

            return status == Status.SUCCESS;
        }

        return false;

    }

    private void enableWidgets(boolean enable) {
        userTxt.setEnabled(enable);
        passTxt.setEnabled(enable);
        keyTxt.setEnabled(enable);

        if (!enable) {
            buttonComp.enableButtons(true);
        } else {
            checkUserInput();
        }
    }

    /**
     * Get the encryption object
     * 
     * @param text
     *            The encryption algorithm
     * @return The Encryption object
     */
    private Encryption getEncryption() {
        Encryption enc = new Encryption();
        enc.setAlgorithim(Algorithim.AES);
        enc.setPadding(Padding.AES);

        return enc;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cancel() {
        // This resets the fields
        handleProviderSelection();
    }
}