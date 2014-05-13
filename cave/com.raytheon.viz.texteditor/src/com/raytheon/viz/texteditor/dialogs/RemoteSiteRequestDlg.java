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

package com.raytheon.viz.texteditor.dialogs;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.GetWmoIdRequest;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.msgs.IWmoIdSelectionCallback;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * The Remote Site Request dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 9/13/07      368         lvenable    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 09/20/2012   1196        rferrel     Changing dialogs being called to not block.
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class RemoteSiteRequestDlg extends CaveSWTDialog implements
        IWmoIdSelectionCallback {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteSiteRequestDlg.class);

    private StyledText ttaaiiTF;

    private StyledText ccccTF;

    /**
     * AFOS site ID text field.
     */
    private StyledText wsfoIdTF;

    /**
     * Product category text field.
     */
    private StyledText productCatTF;

    /**
     * Product designator text field.
     */
    private StyledText prodDesignatorTF;

    /**
     * Address text field. Defines the site where a text product or message is
     * sent.
     */
    private StyledText addresseeTF;

    private Button enterBtn;

    private String initialAfosID;

    private WmoIdSelectionDialog wmoIdSelectionDialog;

    boolean lookupAllowed = true;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RemoteSiteRequestDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Send Request");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createInputFields();
        createBottomButtons();
        checkEnableEnter();
    }

    /**
     * Create the input fields.
     */
    private void createInputFields() {
        Composite topComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        gridLayout.horizontalSpacing = 20;
        topComp.setLayout(gridLayout);
        GridData gd = null;
        Label sepLbl = null;

        gd = new GridData(70, SWT.DEFAULT);
        ttaaiiTF = new StyledText(topComp, SWT.BORDER | SWT.READ_ONLY);
        ttaaiiTF.setTextLimit(6);
        ttaaiiTF.setLayoutData(gd);
        ttaaiiTF.setEnabled(false);
        ttaaiiTF.setEditable(false);
        ttaaiiTF.setBackground(shell.getBackground());

        Label ttaaiiLbl = new Label(topComp, SWT.NONE);
        ttaaiiLbl.setText("TTAAii");

        gd = new GridData(45, SWT.DEFAULT);
        ccccTF = new StyledText(topComp, SWT.BORDER | SWT.READ_ONLY);
        ccccTF.setEnabled(false);
        ccccTF.setEditable(false);
        ccccTF.setLayoutData(gd);
        ccccTF.setBackground(shell.getBackground());
        Label ccccLbl = new Label(topComp, SWT.NONE);
        ccccLbl.setText("CCCC");

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // Create the WSFO ID text field.
        gd = new GridData(45, SWT.DEFAULT);
        wsfoIdTF = new StyledText(topComp, SWT.BORDER);
        wsfoIdTF.setTextLimit(3);
        wsfoIdTF.setLayoutData(gd);

        Label wsfoIdLbl = new Label(topComp, SWT.NONE);
        wsfoIdLbl.setText("WSFO Identification");

        // Create the product category text field.
        gd = new GridData(45, SWT.DEFAULT);
        productCatTF = new StyledText(topComp, SWT.BORDER);
        productCatTF.setTextLimit(3);
        productCatTF.setLayoutData(gd);

        Label productCatLbl = new Label(topComp, SWT.NONE);
        productCatLbl.setText("Product Category");

        // Create the product designator text field.
        gd = new GridData(45, SWT.DEFAULT);
        prodDesignatorTF = new StyledText(topComp, SWT.BORDER);
        prodDesignatorTF.setTextLimit(3);
        prodDesignatorTF.setLayoutData(gd);

        Label prodDesignatorLbl = new Label(topComp, SWT.NONE);
        prodDesignatorLbl.setText("Product Designator");

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // Create the addressee text field.
        gd = new GridData(45, SWT.DEFAULT);
        addresseeTF = new StyledText(topComp, SWT.BORDER);
        addresseeTF.setTextLimit(4);
        addresseeTF.setText("DEF");
        addresseeTF.setLayoutData(gd);

        Label addresseeLbl = new Label(topComp, SWT.NONE);
        addresseeLbl.setText("Addressee");

        setupTextFieldListeners();

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // TODO set fields should be moved to preOpen()
        String ccc = "";
        String nnn = "";
        String xxx = "";
        try {
            ccc = initialAfosID.substring(0, 3);
            nnn = initialAfosID.substring(3, 6);
            xxx = initialAfosID.substring(6);
        } catch (Exception e) {
            // ignore
        }
        wsfoIdTF.setText(ccc);
        productCatTF.setText(nnn);
        prodDesignatorTF.setText(xxx);
    }

    private void setupTextFieldListeners() {
        // forces all fields to uppercase and only allows numbers/digits
        textFieldVerifyListener(wsfoIdTF);
        textFieldVerifyListener(productCatTF);
        textFieldVerifyListener(prodDesignatorTF);
        textFieldVerifyListener(addresseeTF);

        // forces overwrite and arrow key traversal
        textFieldKeyListener(wsfoIdTF, addresseeTF, productCatTF);
        textFieldKeyListener(productCatTF, wsfoIdTF, prodDesignatorTF);
        textFieldKeyListener(prodDesignatorTF, productCatTF, addresseeTF);
        textFieldKeyListener(addresseeTF, prodDesignatorTF, wsfoIdTF);

        textFieldModifyListener(wsfoIdTF, productCatTF, true);
        textFieldModifyListener(productCatTF, prodDesignatorTF, true);
        textFieldModifyListener(prodDesignatorTF, addresseeTF, false);
    }

    /**
     * Create the bottom Enter and Cancel buttons.
     */
    private void createBottomButtons() {
        // Create a composite that will center added controls/composites.
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));
        buttonArea.setLayout(new GridLayout(1, false));

        // Create a composite to hold the enter and cancel buttons.
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayout(new GridLayout(2, true));

        // Create the Enter button.
        enterBtn = new Button(buttons, SWT.PUSH);
        enterBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        enterBtn.setText("Enter");
        enterBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (createReturnValue(false))
                    shell.dispose();
            }
        });

        // Create the Cancel button.
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                shell.dispose();
            }
        });
    }

    private Calendar createCalRelativeTo(Calendar relative,
            WMOHeader wmoHeader, int monthAdjustment) {
        Calendar c = new GregorianCalendar(relative.getTimeZone());
        c.setTimeInMillis(relative.getTimeInMillis());
        c.add(GregorianCalendar.MONTH, monthAdjustment);
        c.set(GregorianCalendar.DAY_OF_MONTH, wmoHeader.getDay());
        c.set(GregorianCalendar.HOUR_OF_DAY, wmoHeader.getHour());
        c.set(GregorianCalendar.MINUTE, wmoHeader.getMinute());
        c.set(GregorianCalendar.SECOND, 0);
        c.set(GregorianCalendar.MILLISECOND, 0);
        return c;
    }

    private Calendar getCloserCalendar(Calendar reference, Calendar a,
            Calendar b) {
        return Math.abs(a.getTimeInMillis() - reference.getTimeInMillis()) < Math
                .abs(b.getTimeInMillis() - reference.getTimeInMillis()) ? a : b;
    }

    private boolean createReturnValue(boolean validateOnly) {
        String ccc = wsfoIdTF.getText();
        String nnn = productCatTF.getText();
        String xxx = prodDesignatorTF.getText();
        String addr = addresseeTF.getText();
        if (ccc.length() != 3 || nnn.length() != 3 || xxx.length() < 1
                || xxx.length() > 3 || addr.length() < 1)
            return false;

        if (validateOnly)
            return true;

        String afosID = ccc + nnn + xxx;
        GetWmoIdRequest request = new GetWmoIdRequest();
        request.setAfosId(afosID);

        RemoteRetrievalRequest req = new RemoteRetrievalRequest();
        // TODO: Translate addr via awipsSites.txt or siteDistLists.txt
        req.setAddressee(addr);
        req.setAfosID(afosID);

        List<StdTextProduct> latest = null;
        try {
            latest = CommandFactory.getAfosCommand(req.getAfosID())
                    .executeCommand(TextEditorUtil.getTextDbsrvTransport());
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving metatdata", e);
            // but keep going...
        }

        if (latest != null && latest.size() > 0) {
            Calendar c = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
            c.setTimeInMillis(latest.get(0).getRefTime());
            req.setMostRecentTime(c.getTimeInMillis()); // default
            try {
                WMOHeader wmo = new WMOHeader(latest.get(0).getProduct()
                        .getBytes());
                Calendar t = getCloserCalendar(c,
                        createCalRelativeTo(c, wmo, 0),
                        createCalRelativeTo(c, wmo, 1));
                t = getCloserCalendar(c, t, createCalRelativeTo(c, wmo, -1));
                req.setMostRecentTime(t.getTimeInMillis());
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error determining product time", e);
            }
        } else
            req.setMostRecentTime(0);

        String ttaaii = ttaaiiTF.getText();
        String cccc = ccccTF.getText();
        if (ttaaii.length() > 0 && cccc.length() > 0) {
            req.setWmoHeader(ttaaii + " " + cccc);
        } else {
            req.setWmoHeader("");
        }

        req.setValidTime(System.currentTimeMillis() + 600 * 1000); // Current
                                                                   // time plus
                                                                   // 10 minutes

        setReturnValue(req);
        return true;
    }

    private void textFieldKeyListener(final StyledText tf,
            final StyledText previousTF, final StyledText nextTF) {
        tf.addKeyListener(new KeyAdapter() {

            @Override
            public void keyPressed(KeyEvent e) {
                char c = e.character;

                if (Character.isLetterOrDigit(c) || Character.isSpaceChar(c)) {
                    int pos = tf.getCaretOffset();
                    String text = tf.getText();

                    if (text.length() > pos) {
                        StringBuilder b = new StringBuilder(text);
                        b.deleteCharAt(pos);
                        tf.setText(b.toString());
                        tf.setSelection(pos);
                    }
                } else if (e.keyCode == SWT.ARROW_UP) {
                    previousTF.setFocus();
                    previousTF.selectAll();
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    nextTF.setFocus();
                    nextTF.selectAll();
                }
            }
        });
    }

    private void textFieldVerifyListener(final StyledText tf) {
        tf.addVerifyListener(new VerifyListener() {
            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
                StringBuilder b = null;
                int posMod = 0;
                char[] chars = e.text.toCharArray();
                for (int i = 0; i < chars.length; i++) {
                    char c = chars[i];
                    if (!Character.isLetterOrDigit(c)
                            && !Character.isSpaceChar(c)) {
                        if (b == null) {
                            b = new StringBuilder(e.text);
                        }
                        b.deleteCharAt(i - posMod++);
                    }
                }

                if (b != null) {
                    e.text = b.toString();
                }
            }
        });
    }

    private void textFieldModifyListener(final StyledText tf,
            final StyledText nextTF, final boolean limitCheck) {
        tf.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (!limitCheck || tf.getCharCount() == tf.getTextLimit()) {
                    if (wmoIdSelectionDialog == null) {
                        lookupWmoIDs();
                    } else {
                        wmoIdSelectionDialog.close();
                        wmoIdSelectionDialog = null;
                        lookupWmoIDs();
                    }
                } else {
                    if (wmoIdSelectionDialog != null) {
                        wmoIdSelectionDialog.close();
                        wmoIdSelectionDialog = null;
                    }
                    setWmoId("", "");
                    checkEnableEnter();
                }

                if (!isDisposed()) {
                    if (tf.getCaretOffset() == tf.getTextLimit()) {
                        nextTF.setFocus();
                        nextTF.selectAll();
                    }

                    checkEnableEnter();
                }
            }
        });
    }

    private void lookupWmoIDs() {
        if (lookupAllowed && wsfoIdTF.getCharCount() == wsfoIdTF.getTextLimit()
                && productCatTF.getCharCount() == productCatTF.getTextLimit()
                && prodDesignatorTF.getText().length() > 0) {
            GetWmoIdRequest request = new GetWmoIdRequest();
            request.setAfosId(wsfoIdTF.getText() + productCatTF.getText()
                    + prodDesignatorTF.getText());
            lookupAllowed = false;

            try {
                Object response = ThriftClient.sendRequest(request);
                if (response != null) {
                    if (response instanceof AfosWmoIdDataContainer) {
                        AfosWmoIdDataContainer container = (AfosWmoIdDataContainer) response;
                        if (container.getErrorMessage() != null) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Error occurred looking up WMO IDs\nMessage from server["
                                                    + container
                                                            .getErrorMessage()
                                                    + "]");
                        }

                        java.util.List<AfosToAwips> list = container
                                .getIdList();

                        if (list.size() > 1) {
                            ArrayList<String> ttaaiiIds = new ArrayList<String>(
                                    list.size());
                            ArrayList<String> ccccIds = new ArrayList<String>(
                                    list.size());
                            for (AfosToAwips id : list) {
                                ttaaiiIds.add(id.getWmottaaii());
                                ccccIds.add(id.getWmocccc());
                            }

                            wmoIdSelectionDialog = new WmoIdSelectionDialog(
                                    shell, this, ttaaiiIds, ccccIds);
                            wmoIdSelectionDialog
                                    .setCloseCallback(new ICloseCallback() {

                                        @Override
                                        public void dialogClosed(
                                                Object returnValue) {
                                            lookupAllowed = true;
                                            wmoIdSelectionDialog = null;
                                        }
                                    });
                            wmoIdSelectionDialog.setBlockOnOpen(false);
                            wmoIdSelectionDialog.open();
                            return;
                        } else if (list.size() == 1) {
                            AfosToAwips id = list.get(0);
                            setWmoId(id.getWmottaaii(), id.getWmocccc());
                        } else {
                            setWmoId("", "");
                        }
                        checkEnableEnter();
                    } else {
                        statusHandler.handle(Priority.PROBLEM,
                                "Received unhandled WMO Id lookup response from server. Received obj of type ["
                                        + response.getClass() + "], contents["
                                        + response + "]");
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred looking up WMO IDs", e);
            }

            lookupAllowed = true;
        }
    }

    public void setRequest(RemoteRetrievalRequest lastRemoteRetrievalRequest) {
        initialAfosID = null;
        if (lastRemoteRetrievalRequest != null)
            initialAfosID = lastRemoteRetrievalRequest.getAfosID();
    }

    private void checkEnableEnter() {
        boolean enabled = false;
        if (enterBtn != null && !isDisposed()) {
            if (ttaaiiTF.getCharCount() > 0 && ccccTF.getCharCount() > 0
                    && addresseeTF.getCharCount() > 0) {
                enabled = true;
            }
            enterBtn.setEnabled(enabled);
        }
    }

    @Override
    public void setWmoId(String ttaaii, String cccc) {
        ttaaiiTF.setText(ttaaii);
        ccccTF.setText(cccc);
        checkEnableEnter();
    }
}
