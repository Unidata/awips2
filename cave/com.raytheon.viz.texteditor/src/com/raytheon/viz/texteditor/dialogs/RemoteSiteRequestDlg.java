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

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.GetWmoIdRequest;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.msgs.IWmoIdSelectionCallback;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The Remote Site Request dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 13, 2007  368      lvenable  Initial creation.
 * Oct 11, 2007  482      grichard  Reformatted file.
 * Sep 20, 2012  1196     rferrel   Changing dialogs being called to not block.
 * Sep 09, 2014  3580     mapeters  Removed IQueryTransport usage (no longer
 *                                  exists).
 * Aug 31, 2015  4749     njensen   Changed setCloseCallback to addCloseCallback
 * Aug 28, 2018  7134     randerso  Reworked dialog for new remote request
 *                                  implementation
 *
 * </pre>
 *
 * @author lvenable
 */
public class RemoteSiteRequestDlg extends CaveSWTDialog
        implements IWmoIdSelectionCallback {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteSiteRequestDlg.class);

    // pattern used to parse the site id from a standard dx server hostname
    private static final Pattern ADDRESS_PATTERN = Pattern
            .compile("d[xv]\\d-(\\w{3,4})\\.?.*");

    private Text ttaaiiTF;

    private Text ccccTF;

    /**
     * AFOS site ID text field.
     */
    private Text wsfoIdTF;

    /**
     * Product category text field.
     */
    private Text productCatTF;

    /**
     * Product designator text field.
     */
    private Text prodDesignatorTF;

    /**
     * Address text field. Defines the site where a text product or message is
     * sent.
     */
    private Text addresseeTF;

    private Button overrideBtn;

    private Text overrideTF;

    private Label addresseeValid;

    private List<Text> inputFields;

    private int currentField;

    private Button enterBtn;

    private RemoteRetrievalRequest lastRemoteRetrievalRequest;

    private WmoIdSelectionDialog wmoIdSelectionDialog;

    private boolean lookupAllowed = true;

    private String addressee;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     * @param lastRemoteRetrievalRequest
     *            last RemoteRetrievalRequest used to initialize dialog
     */
    public RemoteSiteRequestDlg(Shell parent,
            RemoteRetrievalRequest lastRemoteRetrievalRequest) {
        super(parent, SWT.DIALOG_TRIM,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("Send Request");
        this.lastRemoteRetrievalRequest = lastRemoteRetrievalRequest;
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
        inputFields = new ArrayList<>();

        Composite topComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.verticalSpacing = 0;
        shell.setLayout(gridLayout);

        gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        gridLayout.horizontalSpacing = 20;
        topComp.setLayout(gridLayout);
        GridData gd = null;
        Label sepLbl = null;

        ttaaiiTF = new Text(topComp, SWT.BORDER | SWT.READ_ONLY);

        GC gc = new GC(ttaaiiTF);
        int charWidth = gc.textExtent("W").x;
        gc.dispose();

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = charWidth * 6;
        ttaaiiTF.setLayoutData(gd);
        ttaaiiTF.setEnabled(false);
        ttaaiiTF.setEditable(false);
        ttaaiiTF.setBackground(shell.getBackground());

        Label ttaaiiLbl = new Label(topComp, SWT.NONE);
        ttaaiiLbl.setText("TTAAii");

        ccccTF = new Text(topComp, SWT.BORDER | SWT.READ_ONLY);
        ccccTF.setEnabled(false);
        ccccTF.setEditable(false);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = charWidth * 4;
        ccccTF.setLayoutData(gd);
        ccccTF.setBackground(shell.getBackground());
        Label ccccLbl = new Label(topComp, SWT.NONE);
        ccccLbl.setText("CCCC");

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // Create the WSFO ID text field.
        wsfoIdTF = new Text(topComp, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = charWidth * 3;
        wsfoIdTF.setLayoutData(gd);
        wsfoIdTF.setTextLimit(3);

        // set value for limitCheck used in modifyListener
        wsfoIdTF.setData(true);

        Label wsfoIdLbl = new Label(topComp, SWT.NONE);
        wsfoIdLbl.setText("WSFO Identification");

        // Create the product category text field.
        productCatTF = new Text(topComp, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = charWidth * 3;
        productCatTF.setLayoutData(gd);
        productCatTF.setTextLimit(3);

        // set value for limitCheck used in modifyListener
        productCatTF.setData(true);

        Label productCatLbl = new Label(topComp, SWT.NONE);
        productCatLbl.setText("Product Category");

        // Create the product designator text field.
        prodDesignatorTF = new Text(topComp, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = charWidth * 3;
        prodDesignatorTF.setLayoutData(gd);
        prodDesignatorTF.setTextLimit(3);

        // set value for limitCheck used in modifyListener
        prodDesignatorTF.setData(false);

        Label prodDesignatorLbl = new Label(topComp, SWT.NONE);
        prodDesignatorLbl.setText("Product Designator");

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // Create the addressee text field.
        addresseeTF = new Text(topComp, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = charWidth * 4;
        addresseeTF.setLayoutData(gd);
        addresseeTF.setTextLimit(4);

        Label addresseeLbl = new Label(topComp, SWT.NONE);
        addresseeLbl.setText("Addressee");

        Composite overrideComp = new Composite(shell, SWT.NONE);
        gridLayout = new GridLayout(2, false);
        gridLayout.horizontalSpacing = 0;
        gridLayout.marginHeight = 0;
        gridLayout.marginBottom = 5;
        overrideComp.setLayout(gridLayout);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        overrideComp.setLayoutData(gd);

        overrideBtn = new Button(overrideComp, SWT.CHECK);
        overrideBtn.setText("Override Addressee");

        overrideTF = new Text(overrideComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        overrideTF.setLayoutData(gd);
        overrideTF.setEnabled(false);

        addresseeValid = new Label(overrideComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        addresseeValid.setLayoutData(gd);

        setupTextFieldListeners();

        gd = new GridData(GridData.FILL_HORIZONTAL);
        sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        String ccc = "";
        String nnn = "";
        String xxx = "";
        String addr = "WNCF";
        if (lastRemoteRetrievalRequest != null) {
            String initialAfosID = lastRemoteRetrievalRequest.getAfosID();
            ccc = initialAfosID.substring(0, 3);
            nnn = initialAfosID.substring(3, 6);
            xxx = initialAfosID.substring(6);

            Matcher m = ADDRESS_PATTERN
                    .matcher(lastRemoteRetrievalRequest.getAddressee());
            if (m.matches()) {
                addr = m.group(1);
            } else {
                overrideBtn.setSelection(true);
                overrideTF.setText(lastRemoteRetrievalRequest.getAddressee());
                overrideTF.setEnabled(true);
                addresseeTF.setEnabled(false);
            }
        }

        wsfoIdTF.setText(ccc);
        productCatTF.setText(nnn);
        prodDesignatorTF.setText(xxx);
        addresseeTF.setText(addr);
    }

    private void setupTextFieldListeners() {
        /*
         * force all fields to upper case and only allows numbers/digits
         */
        textFieldVerifyListener(wsfoIdTF, productCatTF, prodDesignatorTF,
                addresseeTF);

        /*
         * force overwrite and arrow key traversal
         */
        textFieldKeyListener(wsfoIdTF, productCatTF, prodDesignatorTF,
                addresseeTF, overrideTF);

        /*
         * lookup WMO IDs when wsfo id, cat, or designator are updated
         */
        textFieldModifyListener(wsfoIdTF, productCatTF, prodDesignatorTF);

        /*
         * update override text when addressee modified
         */
        addresseeTF.addModifyListener(e -> updateOverrideTF());

        /*
         * handle override check box selection
         */
        overrideBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean override = overrideBtn.getSelection();
                addresseeTF.setEnabled(!override);
                overrideTF.setEnabled(override);
                if (override) {
                    overrideTF.setFocus();
                    overrideTF.selectAll();
                } else {
                    addresseeTF.setFocus();
                    addresseeTF.selectAll();
                    updateOverrideTF();
                }
            }
        });

        overrideTF.addModifyListener(e -> {
            addresseeIsValid();
            checkEnableEnter();
        });
    }

    private void updateOverrideTF() {
        if (addresseeTF.isEnabled()) {
            overrideTF.setText("edexcluster-" + addresseeTF.getText().toLowerCase());
            addresseeIsValid();
            checkEnableEnter();
        }
    }

    private boolean addresseeIsValid() {
        boolean valid;
        String host = overrideTF.getText();
        try {
            InetAddress address = InetAddress.getByName(host);
            valid = true;
            this.addressee = SystemUtil.getHostName(address);
        } catch (UnknownHostException e) {
            // not logging this as it is expected
            valid = false;
        }
        addresseeValid.setText("Addressee " + (valid ? "valid" : "invalid"));

        return valid;
    }

    /**
     * Create the bottom Enter and Cancel buttons.
     */
    private void createBottomButtons() {
        // Create a composite that will center added controls/composites.
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        buttonArea.setLayout(new GridLayout(1, false));

        // Create a composite to hold the enter and cancel buttons.
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayout(new GridLayout(2, true));

        // Create the Enter button.
        enterBtn = new Button(buttons, SWT.PUSH);
        enterBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        enterBtn.setText("Enter");

        // Create the Cancel button.
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        cancelBtn.setText("Cancel");

        SelectionAdapter selectionAdapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                buttonPressed((Button) event.widget);
            }
        };
        enterBtn.addSelectionListener(selectionAdapter);
        cancelBtn.addSelectionListener(selectionAdapter);
    }

    private void buttonPressed(Button button) {
        if (button == enterBtn) {
            if (!createReturnValue()) {
                return;
            }
        } else {
            setReturnValue(null);
        }
        shell.close();
    }

    private Calendar createCalRelativeTo(long relative, WMOHeader wmoHeader,
            int monthAdjustment) {
        Calendar c = TimeUtil.newGmtCalendar(relative);
        c.add(GregorianCalendar.MONTH, monthAdjustment);
        c.set(GregorianCalendar.DAY_OF_MONTH, wmoHeader.getDay());
        c.set(GregorianCalendar.HOUR_OF_DAY, wmoHeader.getHour());
        c.set(GregorianCalendar.MINUTE, wmoHeader.getMinute());
        c.set(GregorianCalendar.SECOND, 0);
        c.set(GregorianCalendar.MILLISECOND, 0);
        return c;
    }

    private Calendar getCloserCalendar(long reference, Calendar a, Calendar b) {
        return Math.abs(a.getTimeInMillis() - reference) < Math
                .abs(b.getTimeInMillis() - reference) ? a : b;
    }

    private boolean createReturnValue() {
        String ccc = wsfoIdTF.getText();
        String nnn = productCatTF.getText();
        String xxx = prodDesignatorTF.getText();
        if (ccc.length() != 3 || nnn.length() != 3 || xxx.isEmpty()) {
            return false;
        }

        String afosID = ccc + nnn + xxx;
        GetWmoIdRequest request = new GetWmoIdRequest();
        request.setAfosId(afosID);

        RemoteRetrievalRequest req = new RemoteRetrievalRequest();
        req.setAddressee(addressee);
        req.setAfosID(afosID);

        List<StdTextProduct> latest = null;
        try {
            latest = CommandFactory.getAfosCommand(req.getAfosID())
                    .executeCommand();
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM, "Error retrieving metadata",
                    e);
            // but keep going...
        }

        if (latest != null && !latest.isEmpty()) {
            long time = latest.get(0).getRefTime();
            req.setMostRecentTime(time);
            try {
                WMOHeader wmo = new WMOHeader(
                        latest.get(0).getProduct().getBytes());
                Calendar t = getCloserCalendar(time,
                        createCalRelativeTo(time, wmo, 0),
                        createCalRelativeTo(time, wmo, 1));
                t = getCloserCalendar(time, t,
                        createCalRelativeTo(time, wmo, -1));
                req.setMostRecentTime(t.getTimeInMillis());
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error determining product time", e);
            }
        } else {
            req.setMostRecentTime(0);
        }

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

    private void textFieldKeyListener(Text... fields) {
        KeyAdapter keyAdapter = new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                Text tf = (Text) e.widget;
                char c = e.character;

                if (Character.isLetterOrDigit(c) || Character.isSpaceChar(c)) {
                    int pos = tf.getCaretPosition();
                    String text = tf.getText();

                    if (text.length() > pos) {
                        StringBuilder b = new StringBuilder(text);
                        b.deleteCharAt(pos);
                        tf.setText(b.toString());
                        tf.setSelection(pos);
                    }
                } else if (e.keyCode == SWT.ARROW_UP) {
                    traverseFields(-1);
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    traverseFields(1);
                } else if (e.character == SWT.CR) {
                    if (enterBtn.isEnabled()) {
                        buttonPressed(enterBtn);
                    }
                }
            }
        };

        FocusListener focusListener = new FocusListener() {
            @Override
            public void focusLost(FocusEvent e) {
                ((Text) e.widget).clearSelection();
            }

            @Override
            public void focusGained(FocusEvent e) {
                currentField = inputFields.indexOf(e.widget);
            }
        };

        for (Text tf : fields) {
            inputFields.add(tf);
            tf.addKeyListener(keyAdapter);
            tf.addFocusListener(focusListener);
        }
    }

    private void textFieldVerifyListener(Text... fields) {
        /*
         * force all fields to upper case and only allows numbers/digits
         */
        VerifyListener verifyListener = e -> {
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
                    b.deleteCharAt(i - posMod);
                    posMod++;
                }
            }

            if (b != null) {
                e.text = b.toString();
            }
        };

        for (Text tf : fields) {
            tf.addVerifyListener(verifyListener);
        }
    }

    private void textFieldModifyListener(Text... fields) {
        ModifyListener modifyListener = event -> {
            Text tf = (Text) event.widget;
            Boolean limitCheck = (Boolean) tf.getData();
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
                if (tf.getCaretPosition() == tf.getTextLimit()) {
                    traverseFields(1);
                }

                checkEnableEnter();
            }
        };

        for (Text tf : fields) {
            tf.addModifyListener(modifyListener);
        }
    }

    private void traverseFields(int increment) {
        currentField += increment;
        if (currentField < 0) {
            currentField = inputFields.size() - 1;
        } else if (currentField >= inputFields.size()) {
            currentField = 0;
        }

        Text field = inputFields.get(currentField);
        if (!field.getEnabled()) {
            traverseFields(increment);
        } else {
            field.setFocus();
            field.selectAll();
        }
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
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error occurred looking up WMO IDs\nMessage from server["
                                            + container.getErrorMessage()
                                            + "]");
                        }

                        List<AfosToAwips> list = container.getIdList();

                        if (list.size() > 1) {
                            List<String> ttaaiiIds = new ArrayList<>(
                                    list.size());
                            List<String> ccccIds = new ArrayList<>(list.size());
                            for (AfosToAwips id : list) {
                                ttaaiiIds.add(id.getWmottaaii());
                                ccccIds.add(id.getWmocccc());
                            }

                            wmoIdSelectionDialog = new WmoIdSelectionDialog(
                                    shell, this, ttaaiiIds, ccccIds);
                            wmoIdSelectionDialog
                                    .addCloseCallback(returnValue -> {
                                        lookupAllowed = true;
                                        wmoIdSelectionDialog = null;
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

    private void checkEnableEnter() {
        boolean enabled = false;
        if (enterBtn != null && !isDisposed()) {
            if (ttaaiiTF.getCharCount() > 0 && ccccTF.getCharCount() > 0
                    && addresseeIsValid()) {
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
