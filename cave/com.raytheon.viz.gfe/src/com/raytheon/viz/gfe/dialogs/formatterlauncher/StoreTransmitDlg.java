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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.text.SimpleDateFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.activetable.PracticeProductOfftimeRequest;
import com.raytheon.uf.common.activetable.SendPracticeProductRequest;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.product.TextDBUtil;
import com.raytheon.viz.gfe.vtec.GFEVtecUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display the Store/Transmit dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2008  ###       lvenable    Initial creation
 * Feb 19, 2010  4132      ryu         Product correction.
 * May 28, 2010  2187      cjeanbap    Added StdTextProductFactory 
 *                                     functionality.
 * Nov 09, 2012  1298      rferrel     Changes for non-blocking dialog.
 * Apr 02, 2013  15564 mgamazaychikov  Ensured awipsWanPil to be 10 characters
 *                                     space-padded long
 * May 08, 2013  1842      dgilling    Use VtecUtil to set product ETNs, fix
 *                                     warnings.
 * Jun 07, 2013  1981      mduff       Set user's id in OUPRequest as it is
 *                                     now a protected operation.
 * Jan 06, 2014  2649      dgilling    Make ETN assignment process optional.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class StoreTransmitDlg extends CaveSWTDialog implements
        IStoreTransmitProduct {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StoreTransmitDlg.class);

    private static int SEQ_NUMBER = 0;

    /**
     * PRoduct ID text control.
     */
    private Text productIdTF;

    /**
     * Count down progress label.
     */
    private Label progressLbl;

    /**
     * Count down text string.
     */
    private String countdownText;

    /**
     * Count down progress bar.
     */
    private ProgressBar progBar;

    /**
     * Thread used to count down the store/transmit. A separate thread is needed
     * so updates can be made to the display with user interruption.
     */
    private StoreTransmitCountdownThread countdownThread;

    /**
     * Label image that will display the Store/Transmit image.
     */
    private Image labelImg;

    /**
     * Flag used to indicate if the dialog should be a store or transmit dialog.
     * True is store, false is transmit.
     */
    private boolean isStoreDialog = true;

    private String productText;

    private final ProductEditorComp parentEditor;

    /**
     * Product transmission callback to report the state of transmitting a
     * product.
     */
    private final ITransmissionState transmissionCB;

    private final String pid;

    private final boolean updateVtec;

    /**
     * @param parent
     *            Parent shell.
     * @param storeDialog
     *            Store flag. True is store, false is transmit.
     * @param editor
     *            Parent editor. Product will be updated in this editor after
     *            transmission.
     * @param transmissionCB
     * @param pid
     * @param updateVtec
     *            Whether or not to update the ETNs of any VTEC lines in the
     *            product to be transmitted. Recommend setting this to false
     *            when correcting a previously transmitted product.
     */
    public StoreTransmitDlg(Shell parent, boolean storeDialog,
            ProductEditorComp editor, ITransmissionState transmissionCB,
            String pid, boolean updateVtec) {
        super(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);

        this.transmissionCB = transmissionCB;
        isStoreDialog = storeDialog;
        parentEditor = editor;
        this.productText = editor.getProductText();
        this.pid = pid;
        this.updateVtec = updateVtec;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        String title = null;
        CAVEMode opMode = CAVEMode.getMode();
        if (opMode.equals(CAVEMode.OPERATIONAL)) {
            if (isStoreDialog == true) {
                title = "Store in AWIPS TextDB";
                countdownText = "Store Countdown";
            } else {
                title = "Transmit to AWIPS *WAN*";
                countdownText = "Transmit Countdown";
            }
        } else {

            if (isStoreDialog == true) {
                title = "Store in AWIPS TextDB";
                countdownText = "Simulated Store Countdown";
            } else {
                title = "Store Transmit to AWIPS *WAN*";
                countdownText = "Simulated Transmit Countdown";
            }
            title += " (" + opMode.name() + " MODE)";

        }
        shell.setText(title);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        productIdTF.insert(pid);
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        if (isStoreDialog == true) {
            labelImg = parentEditor.getImageRegistry().get("yieldsign");
        } else {
            labelImg = parentEditor.getImageRegistry().get("stopsign");
        }

        createMainControls();
        createBottomButtons();

        Display display = shell.getParent().getDisplay();

        countdownThread = new StoreTransmitCountdownThread(display, progBar,
                progressLbl, countdownText, this, isStoreDialog);
    }

    /**
     * Create the main Store/Transmit controls.
     */
    private void createMainControls() {
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));

        // -------------------------------------
        // Create the left side controls
        // -------------------------------------
        Composite leftComp = new Composite(mainComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(1, false));

        Label productIdLbl = new Label(leftComp, SWT.NONE);
        productIdLbl.setText("AWIPS Product ID:");

        GridData gd = new GridData(200, SWT.DEFAULT);
        productIdTF = new Text(leftComp, SWT.BORDER);
        productIdTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        progressLbl = new Label(leftComp, SWT.CENTER);
        progressLbl.setText(countdownText);
        progressLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        progBar = new ProgressBar(leftComp, SWT.SMOOTH);
        progBar.setMinimum(0);
        progBar.setMaximum(5);
        progBar.setLayoutData(gd);

        // -------------------------------------
        // Create the right side image control
        // -------------------------------------
        Composite rightComp = new Composite(mainComp, SWT.NONE);
        rightComp.setLayout(new GridLayout(1, false));
        rightComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        Label yieldLbl = new Label(rightComp, SWT.NONE);
        yieldLbl.setImage(labelImg);
        yieldLbl.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(150, SWT.DEFAULT);
        final Button actionBtn = new Button(buttons, SWT.PUSH);

        CAVEMode opMode = CAVEMode.getMode();
        if (opMode.equals(CAVEMode.OPERATIONAL)) {
            if (isStoreDialog == true) {
                actionBtn.setText("Store");
            } else {
                actionBtn.setText("Transmit");
            }
        } else if (isStoreDialog == true) {
            actionBtn.setText("Simulated Store");
        } else {
            actionBtn.setText("Simulated Transmit");
        }

        actionBtn.setLayoutData(gd);
        actionBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Disable the store button.
                actionBtn.setEnabled(false);

                // Start the countdown thread.
                countdownThread.start();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (countdownThread != null) {
                    if (countdownThread.isDone() == false) {
                        countdownThread.cancelThread();
                        progressLbl.setText(countdownText);
                        Display display = shell.getParent().getDisplay();
                        progressLbl.setBackground(display
                                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
                        progressLbl.setForeground(display
                                .getSystemColor(SWT.COLOR_BLACK));
                    }
                }

                setReturnValue(null);
                close();
            }
        });
    }

    /**
     * Method to store or transmit the product.
     */
    @Override
    public void storeTransmitProduct() {
        // Store/Transmit the product...

        if (!countdownThread.threadCancelled()) {
            if (updateVtec) {
                try {
                    productText = GFEVtecUtil.finalizeETNs(productText);
                } catch (VizException e) {
                    statusHandler.handle(Priority.CRITICAL,
                            "Error setting ETNs for product", e);
                    sendTransmissionStatus(ConfigData.productStateEnum.Failed);
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            StoreTransmitDlg.this.parentEditor.revive();
                        }
                    });
                    return;
                }
            }

            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    String pid = productIdTF.getText();
                    if (parentEditor.isTestVTEC()) {
                        if (isStoreDialog) {
                            parentEditor.devStore(pid.substring(3));
                        } else {
                            parentEditor.devStore(pid.substring(4));
                            transmitProduct(true);
                        }
                    } else {
                        if (isStoreDialog) {
                            TextDBUtil.storeProduct(pid, productText,
                                    parentEditor.isTestVTEC());
                        } else {
                            transmitProduct(false);
                        }
                    }
                }

            });
        }

        // The asyncExec call is used to dispose of the shell since it is
        // called outside the GUI thread (count down thread).
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                close();
            }
        });
    }

    /**
     * Method to transmit the product.
     */
    private void transmitProduct(boolean decode) {
        IServerRequest req = null;
        if (decode) {
            if (SimulatedTime.getSystemTime().isRealTime()) {
                req = new SendPracticeProductRequest();
                ((SendPracticeProductRequest) req).setProductText(productText);
            } else {
                req = new PracticeProductOfftimeRequest();
                ((PracticeProductOfftimeRequest) req)
                        .setProductText(productText);
                ((PracticeProductOfftimeRequest) req).setNotifyGFE(true);
                SimpleDateFormat dateFormatter = new SimpleDateFormat(
                        "yyyyMMdd_HHmm");
                ((PracticeProductOfftimeRequest) req)
                        .setDrtString(dateFormatter.format(SimulatedTime
                                .getSystemTime().getTime()));
            }
        } else {
            req = new OUPRequest();
            OfficialUserProduct oup = new OfficialUserProduct();
            // make sure the awipsWanPil is exactly 10 characters space-padded
            // long
            String awipsWanPil = String.format("%-10s", productIdTF.getText()
                    .trim());
            oup.setAwipsWanPil(awipsWanPil);
            oup.setProductText(productText);

            String tempName = awipsWanPil + "-" + SEQ_NUMBER + "-"
                    + (System.currentTimeMillis() / 1000);
            oup.setFilename(tempName);

            String type = parentEditor.getProductType();
            if (!type.equals("rou") && !type.equals("res")) {
                oup.setWmoType(type);
            }
            // oup.setAddress(parentEditor.getAutoSendAddress());
            oup.setNeedsWmoHeader(false);
            oup.setSource("GFE");
            ((OUPRequest) req).setProduct(oup);
            ((OUPRequest) req).setUser(UserController.getUserObject());
        }

        try {
            Object response = ThriftClient.sendRequest(req);
            // TODO need a response on the other one? it's going
            // async....
            if (response instanceof OUPResponse) {
                OUPResponse resp = (OUPResponse) response;
                Priority p = null;
                if (!resp.hasFailure()) {
                    p = Priority.EVENTA;
                    sendTransmissionStatus(ConfigData.productStateEnum.Transmitted);
                } else {
                    // determine the failure type and priority
                    ConfigData.productStateEnum state = null;
                    if (resp.isSendLocalSuccess()) {
                        state = ConfigData.productStateEnum.Transmitted;
                    } else {
                        state = ConfigData.productStateEnum.Failed;
                    }
                    p = Priority.EVENTA;
                    if (!resp.isAttempted()) {
                        // if was never attempted to send or store even locally
                        p = Priority.CRITICAL;
                    } else if (!resp.isSendLocalSuccess()) {
                        // if send/store locally failed
                        p = Priority.CRITICAL;
                    } else if (!resp.isSendWANSuccess()) {
                        // if send to WAN failed
                        if (resp.getNeedAcknowledgment()) {
                            // if ack was needed, if it never sent then no ack
                            // was recieved
                            p = Priority.CRITICAL;
                        } else {
                            // if no ack was needed
                            p = Priority.EVENTA;
                        }
                    } else if (resp.getNeedAcknowledgment()
                            && !resp.isAcknowledged()) {
                        // if sent but not acknowledged when acknowledgment is
                        // needed
                        p = Priority.CRITICAL;
                    }
                    sendTransmissionStatus(state);
                }
                statusHandler.handle(p, resp.getMessage());
            }

            this.parentEditor.setProductText(productText, false);
            this.parentEditor.brain();
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL, "Error sending product", e);
            sendTransmissionStatus(ConfigData.productStateEnum.Failed);
            this.parentEditor.revive();
        }

        SEQ_NUMBER++;
    }

    private void sendTransmissionStatus(ConfigData.productStateEnum status) {
        if (isStoreDialog == false) {
            transmissionCB.setTransmissionState(status);
        }
    }
}
