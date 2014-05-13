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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.awt.Point;
import java.util.LinkedHashMap;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanThresholdColor;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Displays the Color threshold dialog used by CELL, DMD, MESO, and TVS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2009  #3039      lvenable     Initial creation
 * 24 Jul 2013  #2143      skorolev    Changes non-blocking dialogs.
 * Aug 15, 2013 #2143      mpduff      Remove resize.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANColorThreshDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    /**
     * Scan table identifier.
     */
    private final ScanTables scanTable;

    /**
     * Attribute combo control.
     */
    private Combo attributeCbo;

    /**
     * Attribute unit label.
     */
    private Label attrUnit;

    /**
     * Upper threshold value.
     */
    private Text upperTF;

    /**
     * Mid threshold value.
     */
    private Text midTF;

    /**
     * Lower threshold value.
     */
    private Text lowerTF;

    /**
     * Upper value.
     */
    private Double upperVal = Double.NaN;

    /**
     * Mid value.
     */
    private Double midVal = Double.NaN;

    /**
     * Lower value.
     */
    private Double lowerVal = Double.NaN;

    /**
     * Map of attribute names and the associated unit.
     */
    private LinkedHashMap<String, String> attrUnitMap;

    /**
     * Threshold callback called when the thresholds are updated.
     */
    private final IThresholdUpdate thresholdCB;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param scanTable
     *            Scan table identifier.
     * @param thresholdCB
     *            Threshold callback.
     */
    public SCANColorThreshDlg(Shell parentShell, ScanTables scanTable,
            IThresholdUpdate thresholdCB) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(scanTable.name() + " Color Threshold");

        this.thresholdCB = thresholdCB;
        this.scanTable = scanTable;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createAttributeControls();
        createBottomButtons();

        attributeSelectionAction();
    }

    /**
     * Create the attribute controls.
     */
    private void createAttributeControls() {
        SCANConfig scanCfg = SCANConfig.getInstance();

        attrUnitMap = scanCfg.getThreshAttributes(scanTable);

        int labelWidth = 100;

        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 10;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        /*
         * Attribute control.
         */
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label attrLbl = new Label(controlComp, SWT.RIGHT);
        attrLbl.setText("Attribute:");
        attrLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        attributeCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateAttributeCombo();
        attributeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                attributeSelectionAction();
            }
        });

        gd = new GridData(90, SWT.DEFAULT);
        attrUnit = new Label(controlComp, SWT.NONE);
        attrUnit.setLayoutData(gd);

        /*
         * Upper threshold controls.
         */
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label upperLbl = new Label(controlComp, SWT.RIGHT);
        upperLbl.setText("Upper:");
        upperLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        upperTF = new Text(controlComp, SWT.BORDER);
        upperTF.setLayoutData(gd);

        /*
         * Mid threshold controls.
         */
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label midLbl = new Label(controlComp, SWT.RIGHT);
        midLbl.setText("Mid:");
        midLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        midTF = new Text(controlComp, SWT.BORDER);
        midTF.setLayoutData(gd);

        /*
         * Lower threshold controls.
         */
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label lowerLbl = new Label(controlComp, SWT.RIGHT);
        lowerLbl.setText("Lower:");
        lowerLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        lowerTF = new Text(controlComp, SWT.BORDER);
        lowerTF.setLayoutData(gd);
    }

    /**
     * Create the button at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (validateEntries() == false) {
                    return;
                }

                updateThresholdValues();

                closeDialog();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (validateEntries() == false) {
                    return;
                }

                updateThresholdValues();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                closeDialog();
            }
        });
    }

    /**
     * Validate the threshold entries.
     * 
     * @return True if the entries are valid, false otherwise.
     */
    private boolean validateEntries() {

        SCANConfig scanCfg = SCANConfig.getInstance();

        String attrName = attributeCbo
                .getItem(attributeCbo.getSelectionIndex());

        upperVal = textIsANumber(upperTF.getText());
        if (upperVal.isNaN() == true) {
            displayMessageDialog("Upper value is not a number.");
            return false;
        }

        midVal = textIsANumber(midTF.getText());
        if (midVal.isNaN() == true) {
            displayMessageDialog("Mid value is not a number.");
            return false;
        }

        lowerVal = textIsANumber(lowerTF.getText());
        if (lowerVal.isNaN() == true) {
            displayMessageDialog("Lower value is not a number.");
            return false;
        }

        if (upperVal < midVal) {
            displayMessageDialog("Upper value is less than mid value.");
            return false;
        }

        if (midVal < lowerVal) {
            displayMessageDialog("Mid value is less than lower value.");
            return false;
        }

        Point pt = scanCfg.getMinMaxValues(scanTable, attrName);

        if (pt != null) {

            if (upperVal > pt.getY()) {
                displayMessageDialog("Upper value is greater than max allowed value: "
                        + pt.getY() + " for attrib: " + attrName);
                return false;
            }

            if (lowerVal < pt.getX()) {
                displayMessageDialog("Lower value is less than min allowed value: "
                        + pt.getX() + " for attrib: " + attrName);
                return false;
            }
        }

        return true;
    }

    /**
     * Check if the specified text is a valid number.
     * 
     * @param text
     *            Text to test.
     * @return The text as a double.
     */
    private Double textIsANumber(String text) {
        try {
            Double ds = Double.parseDouble(text);
            return ds;
        } catch (NumberFormatException nfe) {
            return Double.NaN;
        }
    }

    /**
     * Display a message dialog.
     * 
     * @param message
     *            Message to diaply in the dialog.
     */
    private void displayMessageDialog(String message) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Threshold Error");
        mb.setMessage(message);
        mb.open();
    }

    /**
     * Action taken when the user changes attributes.
     */
    private void attributeSelectionAction() {
        SCANConfig scanCfg = SCANConfig.getInstance();

        String attribute = attributeCbo.getItem(attributeCbo
                .getSelectionIndex());

        upperTF.setText(String.valueOf(scanCfg.getUpperThreshold(scanTable,
                attribute)));
        upperTF.setBackground(scanCfg.getColorThresholdDialogColor(scanTable,
                attribute, ScanThresholdColor.Upper));

        midTF.setText(String.valueOf(scanCfg.getMidThreshold(scanTable,
                attribute)));
        midTF.setBackground(scanCfg.getColorThresholdDialogColor(scanTable,
                attribute, ScanThresholdColor.Mid));

        lowerTF.setText(String.valueOf(scanCfg.getLowerThreshold(scanTable,
                attribute)));
        lowerTF.setBackground(scanCfg.getColorThresholdDialogColor(scanTable,
                attribute, ScanThresholdColor.Lower));

        attrUnit.setText("(" + attrUnitMap.get(attribute) + ")");
    }

    /**
     * Populate the attribute combo control.
     */
    private void populateAttributeCombo() {
        Set<String> keys = attrUnitMap.keySet();

        for (String key : keys) {
            attributeCbo.add(key);
        }

        attributeCbo.select(0);
    }

    /**
     * Update the configuration with the new threshold values.
     */
    private void updateThresholdValues() {
        String attrName = attributeCbo
                .getItem(attributeCbo.getSelectionIndex());

        thresholdCB.thresholdsUpdated(attrName, upperVal, midVal, lowerVal);
    }

    /**
     * Close dialog method.
     */
    @Override
    public void closeDialog() {
        close();
    }

    /**
     * Method that refreshes color threshold when storm cell or display filter
     * is updated.
     */
    public void refresh() {
        attributeSelectionAction();
    }
}
