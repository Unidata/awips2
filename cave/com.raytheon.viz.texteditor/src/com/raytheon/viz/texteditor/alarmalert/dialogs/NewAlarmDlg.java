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
package com.raytheon.viz.texteditor.alarmalert.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct.ProductType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            mnash     Initial creation
 * Jun 08, 2010 5851       cjeanbap    Properly stop alarm/alert observer listener; shellComp is null.
 * Oct 31, 2011 8510       rferrel   made PRIMARY_MODEL and add check for nonblank productID.
 * Sep 20, 2012 1196       rferrel     No longer Blocks
 * 12/07/2012	15555	   m.gamazaychikov	Fixed the implementation for New Proximity Alarm.
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class NewAlarmDlg extends CaveSWTDialog {

    private Text productId;

    private VerifyListener productIdVerifier = new VerifyListener() {
        @Override
        public void verifyText(VerifyEvent e) {
            e.text = e.text.toUpperCase();
        }
    };

    private Combo alarmAlert;

    private Combo actionCmd;

    private Button AOR;

    private Text AORDistance;

    private Text ugcList;

    private Font font;

    private Composite shellComp;

    private AlarmAlertProduct prod = null;

    private Text searchString;

    private Combo distanceLabel;

    private Button alarmCheck;

    private ProductType productType;

    private Point minSize;

    private String alarm_or_alert;

    private boolean okEvent = false;

    /**
     * @param parentShell
     */
    protected NewAlarmDlg(Shell parentShell, String alarm_or_alert,
            AlarmAlertProduct product) {
        super(parentShell, SWT.PRIMARY_MODAL | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        if ("PROXIMITY".equals(alarm_or_alert)) {
            productType = ProductType.Proximity_Alarm;
            setText("New Proximity Alarm Product");
            minSize = new Point(350, 200);
        } else {
            productType = ProductType.Alarm_Alert;
            setText("New Alarm/Alert Product");
            minSize = new Point(350, 150);
        }

        this.alarm_or_alert = alarm_or_alert;
        prod = product;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
        setReturnValue(prod);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setMinimumSize(minSize);
        font = new Font(shell.getDisplay(), "Helvetica", 11, SWT.BOLD);

        if (shellComp == null) {
            shellComp = shell.getShell();
        }

        if ("PROXIMITY".equals(alarm_or_alert)) {
            // Initialize all of the controls and layouts for proximity alarms
            // add dialog
            initializeProximityComponents();
        } else {
            // Initialize all of the controls and layouts for regular
            // alarm/alert add dialog
            initializeAlarmComponents();
        }
    }

    /**
     * 
     */
    private void initializeProximityComponents() {
        createProximityDialog();
    }

    private void initializeAlarmComponents() {
        createAlarmDialog();
    }

    /**
     * 
     */
    private void createProximityDialog() {
        createProductID();
        createAlarms();
        createLocations();
        createButtons();
    }

    /**
     * 
     * @param prods
     * @param initValue
     * @return
     */
    private Text createProductIdField(Composite prods, String initValue) {
        Text prodIdField = new Text(prods, SWT.SINGLE | SWT.BORDER);
        if (initValue != null) {
            prodIdField.setText(initValue);
        }
        prodIdField.addVerifyListener(productIdVerifier);
        return prodIdField;
    }

    private void createAlarmDialog() {
        Composite prods = new Composite(shellComp, SWT.NONE);
        GridLayout prodLayout = new GridLayout(3, false);
        prods.setLayout(prodLayout);
        GridData prodLabelData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                false);
        prods.setLayoutData(prodLabelData);

        Label prodId = new Label(prods, SWT.BOLD);
        prodId.setText("Product ID:");

        prodLabelData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prodId.setLayoutData(prodLabelData);

        productId = createProductIdField(prods, prod.getProductId());

        prodLabelData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prodLabelData.grabExcessHorizontalSpace = true;
        prodLabelData.widthHint = 150;
        productId.setLayoutData(prodLabelData);

        alarmCheck = new Button(prods, SWT.CHECK);
        alarmCheck.setText("Alarm");
        if (prod != null) {
            alarmCheck.setSelection(prod.isAlarm());
        }

        // new composite for the search part of this dialog
        Composite searches = new Composite(shellComp, SWT.NONE);
        GridLayout searchLayout = new GridLayout(2, false);
        searches.setLayout(searchLayout);

        // label for search string
        Label searchLabel = new Label(searches, SWT.BOLD);
        prodLabelData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        searchLabel.setText("Optional Search String:");
        searchLabel.setLayoutData(prodLabelData);

        // actual box for search string to enter values
        prodLabelData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        searchString = new Text(searches, SWT.SINGLE | SWT.BORDER);
        prodLabelData.grabExcessHorizontalSpace = true;
        prodLabelData.widthHint = 150;
        if (prod != null) {
            searchString.setText(prod.getSearchString());
        }
        searchString.setLayoutData(prodLabelData);
        createButtons();
    }

    /**
     * Creates the product id portion of the dialog box
     */
    private void createProductID() {
        Composite prods = new Composite(shellComp, SWT.NONE);
        GridLayout prodLayout = new GridLayout(2, false);
        prods.setLayout(prodLayout);
        GridData prodLabelData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                false);
        prods.setLayoutData(prodLabelData);

        // label for the product id
        Label prodId = new Label(prods, SWT.BOLD);
        prodId.setText("Product ID:");

        prodLabelData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        productId = createProductIdField(prods, prod.getProductId());
        productId.setLayoutData(prodLabelData);

        // separator
        GridData fullLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                true);
        Label separator1 = new Label(shellComp, SWT.SEPARATOR | SWT.HORIZONTAL
                | SWT.FILL);
        separator1.setLayoutData(fullLayoutData);
    }

    /**
     * Creating the boxes in the proximity alarm dialog for determining what
     * kind of product it will be
     */
    private void createAlarms() {
        // Composite for the alarms portion of the dialog
        Composite alarms = new Composite(shellComp, SWT.NONE);
        GridLayout alarmLayout = new GridLayout(4, true);
        alarms.setLayout(alarmLayout);
        Label alarmAlertLabel = new Label(alarms, SWT.BOLD);
        alarmAlertLabel.setText("Alarm/Alert:");
        alarmAlert = new Combo(alarms, SWT.READ_ONLY);
        alarmAlert.setItems(new String[] { "Alarm", "Alert", "None" });
        if (prod != null) {
            for (int i = 0; i < alarmAlert.getItems().length; i++) {
                if (prod.getAlarmType().equals(alarmAlert.getItems()[i])) {
                    alarmAlert.select(i);
                }
            }
        } else {
            alarmAlert.select(2);
        }

        // Action Command
        Label actionLabel = new Label(alarms, SWT.BOLD);
        actionLabel.setText("Action Cmd:");
        actionCmd = new Combo(alarms, SWT.READ_ONLY | SWT.RIGHT);
        actionCmd.setItems(new String[] { "None" });
        actionCmd.select(0);
        actionCmd.setEnabled(false);
        if (prod != null) {
            actionCmd.setText(prod.getActionCmd());
        }
        // separator
        GridData fullLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                true);
        Label separator2 = new Label(shellComp, SWT.SEPARATOR | SWT.HORIZONTAL
                | SWT.FILL);
        separator2.setLayoutData(fullLayoutData);
    }

    /**
     * Portion of the dialog for determining where to filter for each product...
     * certain area by wfo, by wfo + distance, or by other wfos as well
     */
    private void createLocations() {
        Composite aor = new Composite(shellComp, SWT.NONE);
        GridLayout aorLayout = new GridLayout(5, false);
        aor.setLayout(aorLayout);

        Label AORLabel = new Label(aor, SWT.BOLD);
        GridData aorData = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        AORLabel.setText("AOR:");
        AORLabel.setLayoutData(aorData);

        AOR = new Button(aor, SWT.CHECK);
        aorData = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        AOR.setLayoutData(aorData);
        /*
         * DR15555 - Set the AOR selection to:
         * 1. true - for new alarm
         * 2. product's state - for existing alarm
         */
        if (prod.getProductId() != "") {
            AOR.setSelection(prod.isAor());
        }
        else {
        	AOR.setSelection(true);
        	AOR.setBackground(Display.getCurrent().getSystemColor(
                    SWT.COLOR_YELLOW));
        }
        AOR.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (AOR.getSelection() == true) {
                	/*
                	 * DR15555 - handle the AOR selection:
                	 *  - set the background yellow
                	 *  - set the AORDistance text to empty string
                	 *  - set the ugcList text to empty string
                	 */
                    AOR.setBackground(Display.getCurrent().getSystemColor(
                            SWT.COLOR_YELLOW));
                    AORDistance.setText("");
                    ugcList.setText("");
                    
                } else {
                    AOR.setBackground(Display.getCurrent().getSystemColor(
                            SWT.COLOR_WIDGET_BACKGROUND));
                }
            }
        });

        Label AORDistanceLabel = new Label(aor, SWT.BOLD);
        aorData = new GridData(SWT.END, SWT.CENTER, false, false);
        AORDistanceLabel.setText("AOR+Distance:");
        aorData.horizontalAlignment = SWT.RIGHT;
        AORDistanceLabel.setLayoutData(aorData);

        AORDistance = new Text(aor, SWT.SINGLE | SWT.BORDER);
        aorData = new GridData(SWT.END, SWT.DEFAULT, false, false);
        aorData.horizontalAlignment = SWT.RIGHT;
        aorData.widthHint = 60;
        AORDistance.setLayoutData(aorData);
        /*
         * DR15555 - Set the AORDistance selection to:
         * 1. empty string - for new alarm
         * 2. product's distance - for existing alarm
         */
        if (prod.getProductId() != "") {
            AORDistance.setText(prod.getAorDistance());
        }
        else {
        	AORDistance.setText("");
        }

        AORDistance.addListener(SWT.KeyDown, new Listener() {
            @Override
			public void handleEvent(Event e) {
				/*
				 * DR15555 - handle the AOR selection: 
				 * - set the AOR background gray
				 * - un-select the  AOR 
				 * - set the ugcList text to empty string
				 */
				AOR.setSelection(false);				
				AOR.setBackground(Display.getCurrent().getSystemColor(
						SWT.COLOR_WIDGET_BACKGROUND));
				ugcList.setText("");				
			}
        });

        distanceLabel = new Combo(aor, SWT.READ_ONLY);
        aorData = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        distanceLabel.setItems(new String[] { "mi", "km" });
        aorData.horizontalAlignment = SWT.RIGHT;
        distanceLabel.setLayoutData(aorData);
        /*
         * DR15555 - Set the distanceLabel selection to:
         * 1. 'mi' - for new alarm
         * 2. product's label - for existing alarm
         */
        if (prod.getProductId() != "") {
            distanceLabel.setText(prod.getAorLabel());
        } else {
            distanceLabel.select(0);
        }

        // Composite for the ugc portion of the dialog
        Composite ugc = new Composite(shellComp, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.NONE, true, true);
        GridLayout ugcLayout = new GridLayout(2, false);
        ugc.setLayout(ugcLayout);
        ugc.setLayoutData(gd);
        Label ugcLabel = new Label(ugc, SWT.BOLD);
        ugcLabel.setText("UGC List:");
        ugcList = new Text(ugc, SWT.SINGLE | SWT.BORDER);
        ugcList.setLayoutData(gd);
        /*
         * DR15555 - Set the ugcList selection to:
         * 1. empty string - for new alarm
         * 2. product's ugcList - for existing alarm
         */
        if (prod.getProductId() != "") {
            ugcList.setText(prod.getUgcList());
        }
        else {
        	ugcList.setText("");
        }
        ugcList.addListener(SWT.KeyDown, new Listener() {
            @Override
            public void handleEvent(Event e) {
            	/*
				 * DR15555 - handle the ugcList selection: 
				 * - set the AOR background gray
				 * - un-select the  AOR 
				 * - set the AORDistance text to empty string
				 */
				AOR.setBackground(Display.getCurrent().getSystemColor(
						SWT.COLOR_WIDGET_BACKGROUND));
				AOR.setSelection(false);
				AORDistance.setText("");
            }
        });

        // separator
        GridData fullLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                true);
        Label separator3 = new Label(shellComp, SWT.SEPARATOR | SWT.HORIZONTAL
                | SWT.FILL);
        separator3.setLayoutData(fullLayoutData);
    }

    private void createButtons() {
        Composite ackButtons = new Composite(shellComp, SWT.NONE);
        GridLayout ackLayout = new GridLayout(2, false);
        GridData ackGd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        ackButtons.setLayout(ackLayout);
        ackButtons.setLayoutData(ackGd);
        ackGd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        Button okButton = new Button(ackButtons, SWT.PUSH);
        okButton.setText("  Ok  ");
        okButton.setLayoutData(ackGd);
        okButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okEvent = valid();
                if (okEvent) {
                    buildAlarmAlertObject();
                    shell.dispose();
                } else {
                    MessageBox m = new MessageBox(getShell(), SWT.ERROR);
                    m.setMessage("ProductID must contain an entry.");
                    m.open();
                }
            }
        });

        GridData canGD = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        canGD.horizontalAlignment = GridData.END;
        Button cancelButton = new Button(ackButtons, SWT.PUSH);
        cancelButton.setLayoutData(canGD);
        cancelButton.setText("Cancel");
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private boolean valid() {
        boolean result = true;
        if (productId.getText().trim().isEmpty()) {
            result = false;
        }
        return result;
    }

    public void buildAlarmAlertObject() {
        prod.setProductType(productType);
        prod.setProductId(productId.getText());
        if (productType == ProductType.Proximity_Alarm) {
            prod.setAlarmType(alarmAlert.getText());
            prod.setActionCmd(actionCmd.getText());
            prod.setAor(AOR.getSelection());
            prod.setAorDistance(AORDistance.getText());
            prod.setAorLabel(distanceLabel.getText());
            prod.setUgcList(ugcList.getText());
            if ("Alarm".equals(alarmAlert.getText())) {
                prod.setAlarm(true);
            }
        } else {
            prod.setSearchString(searchString.getText());
            prod.setAlarm(alarmCheck.getSelection());
            if (alarmCheck.getSelection()) {
                prod.setAlarmType("Alarm");
            } else {
                prod.setAlarmType("Alert");
            }
        }
    }

    public void setDialogFocus() {
        shell.setFocus();
    }

    public boolean haveOkEvent() {
        return okEvent;
    }

}
