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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.activetable.response.GetNextEtnResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.product.TextDBUtil;
import com.raytheon.viz.gfe.textformatter.TextProductTransmitter;
import com.raytheon.viz.gfe.vtec.GFEVtecUtil;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeProhibitedOpException;

/**
 * Display the Store/Transmit dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
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
 * Oct 23, 2013  1843      dgilling    Ensure that dialog is always closed,
 *                                     even on failure, changes for error handling
 *                                     of intersite ETN assignment.
 * Dec 18, 2013  2641      dgilling    Support changes to GFEVtecUtil.getVtecLinesThatNeedEtn().
 * Jan 06, 2014  2649      dgilling    Make ETN assignment process optional.
 * Feb 17, 2014  2774      dgilling    Merge changes from 14.1 baseline to 14.2.
 * Nov 14, 2014  4953      randerso    Cleaned up practice product requests
 * Feb 26, 2015  4126      randerso    Ensure transmit/store is properly cancelled if dialog is closed
 *                                     Code cleanup
 * Apr 20, 2015  4027      randerso    Renamed ProductStateEnum with an initial capital
 * Aug 28, 2015  4806      dgilling    Extract code for product transmission into
 *                                     its own class.
 * Sep 15, 2015  4858      dgilling    Disable store/transmit in DRT mode.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StoreTransmitDlg extends CaveSWTDialog {
    private static final int COUNT_DOWN_SECONDS = 5;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StoreTransmitDlg.class);

    /**
     * Product ID text control.
     */
    private Text productIdText;

    /**
     * Count down progress label.
     */
    private Label progressLabel;

    /**
     * Count down text string.
     */
    private String countdownText;

    /**
     * Count down progress bar.
     */
    private ProgressBar progressBar;

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

    private String countdownFormat;

    private boolean isCancelled;

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
        CAVEMode opMode = CAVEMode.getMode();
        String title = null;
        if (isStoreDialog) {
            countdownFormat = "Store in %s seconds...";
            countdownText = "Store Countdown";
            title = "Store in AWIPS TextDB";
        } else {
            countdownFormat = "Transmit in %s seconds...";
            countdownText = "Transmit Countdown";
            title = "Transmit to AWIPS *WAN*";
        }

        if (!opMode.equals(CAVEMode.OPERATIONAL)) {
            countdownFormat = "Simulated " + countdownFormat;
            countdownText = "Simulated " + countdownText;
            title += " (" + opMode.name() + " MODE)";
        }
        setText(title);
    }

    @Override
    protected void initializeComponents(Shell shell) {

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                doCancel();
            }
        });
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        productIdText.insert(pid);
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        if (isStoreDialog) {
            labelImg = parentEditor.getImageRegistry().get("yieldsign");
        } else {
            labelImg = parentEditor.getImageRegistry().get("stopsign");
        }

        createMainControls();
        createBottomButtons();
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
        productIdText = new Text(leftComp, SWT.BORDER);
        productIdText.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        progressLabel = new Label(leftComp, SWT.CENTER);
        progressLabel.setText(countdownText);
        progressLabel.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        progressBar = new ProgressBar(leftComp, SWT.SMOOTH);
        progressBar.setMinimum(0);
        progressBar.setMaximum(COUNT_DOWN_SECONDS);
        progressBar.setLayoutData(gd);

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
        final Button actionButton = new Button(buttons, SWT.PUSH);

        CAVEMode opMode = CAVEMode.getMode();
        if (opMode.equals(CAVEMode.OPERATIONAL)) {
            if (isStoreDialog) {
                actionButton.setText("Store");
            } else {
                actionButton.setText("Transmit");
            }
        } else if (isStoreDialog) {
            actionButton.setText("Simulated Store");
        } else {
            actionButton.setText("Simulated Transmit");
        }

        actionButton.setLayoutData(gd);
        actionButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Disable the store button.
                actionButton.setEnabled(false);
                progressLabel.setText(String.format(countdownFormat,
                        COUNT_DOWN_SECONDS));
                progressLabel.setBackground(progressLabel.getDisplay()
                        .getSystemColor(SWT.COLOR_RED));
                progressLabel.setForeground(progressLabel.getDisplay()
                        .getSystemColor(SWT.COLOR_WHITE));

                countDown();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Method to store or transmit the product.
     */
    public void storeTransmitProduct() {
        // Store/Transmit the product...
        if (!this.isCancelled) {
            try {
                if (updateVtec) {
                    // With GFE VTEC products, it's possible to have multiple
                    // segments with NEW vtec action codes and the same phensig.
                    // For
                    // this reason, HazardsTable.py implemented a "cache" that
                    // would
                    // ensure all NEWs for the same phensig would be assigned
                    // the
                    // same ETN. This Map replicates that legacy behavior.
                    //
                    // This "cache" has two levels:
                    // 1. The first level is keyed by the hazard's phensig.
                    // 2. The second level is keyed by the valid period of the
                    // hazard.
                    // Effectively, making this a Map<Phensig, Map<ValidPeriod,
                    // ETN>>.
                    Map<String, List<VtecObject>> vtecsToAssignEtn = GFEVtecUtil
                            .initETNCache(productText);
                    Map<String, Map<TimeRange, Integer>> etnCache = new HashMap<String, Map<TimeRange, Integer>>();

                    for (String phensig : vtecsToAssignEtn.keySet()) {
                        Map<TimeRange, Integer> l2EtnCache = new HashMap<TimeRange, Integer>();
                        List<VtecObject> vtecs = vtecsToAssignEtn.get(phensig);

                        for (int i = 0; i < vtecs.size(); i++) {
                            VtecObject vtec = vtecs.get(i);
                            TimeRange validPeriod = new TimeRange(
                                    vtec.getStartTime(), vtec.getEndTime());
                            Integer currentEtn = vtec.getSequence();

                            // the first time we select a new, unique ETN, any
                            // other
                            // VTEC lines in the product that have the same
                            // phensig
                            // and an adjacent TimeRange can also re-use this
                            // ETN
                            if (currentEtn == 0) {
                                currentEtn = getNextEtn(vtec);
                                l2EtnCache.put(validPeriod, currentEtn);
                            } else {
                                // BUT...once we've made our one pass through
                                // the
                                // product and re-used the ETN where
                                // appropriate, we
                                // should not check again
                                continue;
                            }

                            for (int j = i + 1; j < vtecs.size(); j++) {
                                VtecObject vtec2 = vtecs.get(j);
                                TimeRange validPeriod2 = new TimeRange(
                                        vtec2.getStartTime(),
                                        vtec2.getEndTime());
                                Integer currentEtn2 = vtec2.getSequence();

                                if ((currentEtn2 == 0)
                                        && (validPeriod2
                                                .isAdjacentTo(validPeriod) || validPeriod2
                                                .overlaps(validPeriod))) {
                                    l2EtnCache.put(validPeriod2, currentEtn);
                                    vtec2.setSequence(currentEtn);
                                }
                            }
                        }

                        etnCache.put(phensig, l2EtnCache);
                    }

                    productText = GFEVtecUtil.finalizeETNs(productText,
                            etnCache);
                }

                String pid = productIdText.getText();
                if (parentEditor.isTestVTEC()) {
                    if (isStoreDialog) {
                        parentEditor.devStore(pid.substring(3));
                    } else {
                        parentEditor.devStore(pid.substring(4));
                        transmitProduct(true);
                    }
                } else {
                    if (!SimulatedTimeOperations.isTransmitAllowed()) {
                        throw SimulatedTimeOperations
                                .constructProhibitedOpException("Store/Transmit GFE text products");
                    }

                    if (isStoreDialog) {
                        TextDBUtil.storeProduct(pid, productText,
                                parentEditor.isTestVTEC());
                    } else {
                        transmitProduct(false);
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.CRITICAL,
                        "Error preparing product for transmission.", e);
                sendTransmissionStatus(ConfigData.ProductStateEnum.Failed);
                parentEditor.revive();
            } catch (SimulatedTimeProhibitedOpException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
                sendTransmissionStatus(ConfigData.ProductStateEnum.Failed);
                parentEditor.brain();
            }
        }

        close();
    }

    private Integer getNextEtn(VtecObject vtec) throws VizException {
        GetNextEtnResponse serverResponse = GFEVtecUtil.getNextEtn(
                vtec.getOffice(), vtec.getPhensig(), true, true);
        if (!serverResponse.isOkay()) {
            boolean exitLoop = false;
            Exception exception = null;

            do {
                ETNConfirmationDialog dlg = new ETNConfirmationDialog(
                        getShell(), serverResponse);
                if (dlg.open() == ETNConfirmationDialog.OK) {
                    int etn = dlg.getProposedEtn();
                    statusHandler.info(String.format(
                            "User confirmed ETN for %s: %04d",
                            serverResponse.getPhensig(), etn));
                    try {
                        GetNextEtnResponse followupResp = GFEVtecUtil
                                .getNextEtn(vtec.getOffice(),
                                        vtec.getPhensig(), true, true, true,
                                        etn);
                        serverResponse = followupResp;
                    } catch (VizException e) {
                        exception = e;
                        exitLoop = true;
                    }
                } else {
                    statusHandler.info("User declined to fix ETN for %s",
                            serverResponse.getPhensig());
                    exitLoop = true;
                }
            } while (!serverResponse.isOkay() && !exitLoop);

            if (!serverResponse.isOkay()) {
                String msg = "Unable to set ETN for phensig "
                        + serverResponse.getPhensig() + "\nStatus: "
                        + serverResponse.toString();
                Exception e = exception;
                if (e == null) {
                    throw new VizException(msg);
                } else {
                    throw new VizException(msg, e);
                }
            }
        }

        return serverResponse.getNextEtn();
    }

    private void countDown() {
        getShell().getDisplay().timerExec(1000, new Runnable() {
            @Override
            public void run() {
                bumpCounter();
            }
        });
    }

    private void bumpCounter() {
        if (!progressBar.isDisposed()) {
            // Increment the progress bar
            int count = progressBar.getSelection() + 1;
            if (count < COUNT_DOWN_SECONDS) {
                progressBar.setSelection(count);
                progressLabel.setText(String.format(countdownFormat,
                        (COUNT_DOWN_SECONDS - count)));
                countDown();
            } else {
                storeTransmitProduct();
            }
        }
    }

    /**
     * Method to transmit the product.
     * 
     * @param practice
     *            true if we are transmitting a practice product
     */
    private void transmitProduct(boolean practice) {
        try {
            TextProductTransmitter transmitter = new TextProductTransmitter(
                    productText, productIdText.getText(),
                    parentEditor.getProductType());
            ConfigData.ProductStateEnum state = transmitter
                    .transmitProduct(practice);

            sendTransmissionStatus(state);
            parentEditor.setProductText(productText, false);
            parentEditor.brain();
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL, "Error sending product", e);
            sendTransmissionStatus(ConfigData.ProductStateEnum.Failed);
            parentEditor.revive();
        } catch (SimulatedTimeProhibitedOpException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            sendTransmissionStatus(ConfigData.ProductStateEnum.Failed);
            parentEditor.brain();
        }
    }

    private void sendTransmissionStatus(ConfigData.ProductStateEnum status) {
        if (!isStoreDialog) {
            transmissionCB.setTransmissionState(status);
        }
    }

    private void doCancel() {
        this.isCancelled = true;
        storeTransmitProduct();
    }
}