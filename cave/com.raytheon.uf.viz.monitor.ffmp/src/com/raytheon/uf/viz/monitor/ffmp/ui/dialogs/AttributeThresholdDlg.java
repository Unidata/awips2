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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.TableCellColor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.ThreshColNames;
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPTableColumnXML;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * FFMP Basin Table Threshold attribute display dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Dec 6, 2012  1353       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class AttributeThresholdDlg extends CaveSWTDialog {

    private Text upperText;

    private Text midText;

    private Text lowerText;

    private Text filterText;

    private ThreshColNames threshCol;

    private Double upperVal = Double.NaN;

    private Double midVal = Double.NaN;

    private Double lowerVal = Double.NaN;

    private Double filterVal = Double.NaN;

    private IThreshDisplay threshActionCB;

    public AttributeThresholdDlg(Shell parent, ThreshColNames threshCol,
            IThreshDisplay threshActionCB) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);

        this.threshCol = threshCol;
        this.threshActionCB = threshActionCB;
        setText("Attributes " + threshCol.name());
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createControls();
        createBottomButtons();
    }

    private void createControls() {
        int labelWidth = 75;

        FFMPConfig ffmpCfg = FFMPConfig.getInstance();

        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        /*
         * Upper threshold controls.
         */
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label upperLbl = new Label(controlComp, SWT.RIGHT);
        upperLbl.setText("Upper:");
        upperLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        upperText = new Text(controlComp, SWT.BORDER);
        upperText.setLayoutData(gd);
        upperText.setBackground(ffmpCfg.getCellColor(TableCellColor.Upper));

        /*
         * Mid threshold controls.
         */
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label midLbl = new Label(controlComp, SWT.RIGHT);
        midLbl.setText("Mid:");
        midLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        midText = new Text(controlComp, SWT.BORDER);
        midText.setLayoutData(gd);
        midText.setBackground(ffmpCfg.getCellColor(TableCellColor.Mid));

        /*
         * Lower threshold controls.
         */
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label lowerLbl = new Label(controlComp, SWT.RIGHT);
        lowerLbl.setText("Lower:");
        lowerLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        lowerText = new Text(controlComp, SWT.BORDER);
        lowerText.setLayoutData(gd);
        lowerText.setBackground(ffmpCfg.getCellColor(TableCellColor.Lower));

        /*
         * Filter controls.
         */
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label filterLbl = new Label(controlComp, SWT.RIGHT);
        filterLbl.setText("Filter:");
        filterLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        filterText = new Text(controlComp, SWT.BORDER);
        filterText.setLayoutData(gd);

        updateTextControls(ffmpCfg);
    }

    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
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

                shell.dispose();
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
                shell.dispose();
            }
        });
    }

    private void updateTextControls(FFMPConfig ffmpCfg) {
        FFMPTableColumnXML tcXML;

        tcXML = ffmpCfg.getTableColumnData(this.threshCol);
        upperText.setText(String.valueOf(tcXML.getUpper()));
        tcXML = ffmpCfg.getTableColumnData(this.threshCol);
        midText.setText(String.valueOf(tcXML.getMid()));
        tcXML = ffmpCfg.getTableColumnData(this.threshCol);
        lowerText.setText(String.valueOf(tcXML.getLow()));
        tcXML = ffmpCfg.getTableColumnData(this.threshCol);
        filterText.setText(String.valueOf(tcXML.getFilter()));
    }

    private boolean validateEntries() {
        upperVal = textIsANumber(upperText.getText());
        if (upperVal.isNaN() == true) {
            displayMessage("Upper value is not a number.");
            return false;
        }

        midVal = textIsANumber(midText.getText());
        if (midVal.isNaN() == true) {
            displayMessage("Mid value is not a number.");
            return false;
        }

        lowerVal = textIsANumber(lowerText.getText());
        if (lowerVal.isNaN() == true) {
            displayMessage("Lower value is not a number.");
            return false;
        }

        filterVal = textIsANumber(filterText.getText());
        if (filterVal.isNaN() == true) {
            displayMessage("Filter value is not a number.");
            return false;
        }

        if (upperVal < midVal) {
            displayMessage("Upper value is less than mid value.");
            return false;
        }

        if (midVal < lowerVal) {
            displayMessage("Mid value is less than lower value.");
            return false;
        }

        return true;
    }

    private void updateThresholdValues() {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        ffmpCfg.updateThresholdValues(this.threshCol, upperVal, midVal,
                lowerVal, filterVal);

        threshActionCB.thresholdUpdated(this.threshCol);
    }

    private Double textIsANumber(String text) {
        try {
            Double ds = Double.parseDouble(text);
            return ds;
        } catch (NumberFormatException nfe) {
            return Double.NaN;
        }
    }

    private void displayMessage(String message) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Threshold Error");
        mb.setMessage(message);
        mb.open();
    }

    public void newThreshold(ThreshColNames threshCol) {
        this.threshCol = threshCol;
        shell.setText("Attributes " + threshCol.name());

        FFMPConfig ffmpCfg = FFMPConfig.getInstance();
        updateTextControls(ffmpCfg);
    }
}