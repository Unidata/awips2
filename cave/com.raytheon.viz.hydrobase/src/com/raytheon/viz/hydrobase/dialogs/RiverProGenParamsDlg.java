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
package com.raytheon.viz.hydrobase.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.RPFParamData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the RiverPro General Parameters dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RiverProGenParamsDlg extends CaveSWTDialog {

    /**
     * Look back hours for observed data text control.
     */
    private Text lookbackTF;

    /**
     * Look forward hours for observed data text control.
     */
    private Text lookforwardTF;

    /**
     * Data value text control.
     */
    private Text dataValueTF;

    /**
     * Stage category text control.
     */
    private Text stageCatTF;

    /**
     * Data time text control.
     */
    private Text dataTimeTF;

    /**
     * RVS text control.
     */
    private Text rvsTF;

    /**
     * FLS text control.
     */
    private Text flsTF;

    /**
     * FLW text control.
     */
    private Text flwTF;

    /**
     * Update button.
     */
    private Button updateBtn;

    /**
     * Data for the dialog.
     */
    private RPFParamData paramData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RiverProGenParamsDlg(Shell parent) {
        super(parent);
        setText("RiverPro General Parameters");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize all of the controls and layouts
        createTimeSpanGroup();

        createStringAndDefalutGroups();

        createUpdateButton();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createCloseButton();

        getDialogData();

    }

    /**
     * Create the time span group and controls.
     */
    private void createTimeSpanGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group timeSpanGroup = new Group(shell, SWT.NONE);
        timeSpanGroup.setLayout(new GridLayout(2, false));
        timeSpanGroup.setLayoutData(gd);
        timeSpanGroup.setText(" Time-Span Parameters ");

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 75;
        Label lookbackLbl = new Label(timeSpanGroup, SWT.RIGHT);
        lookbackLbl.setText("Number of Lookback Hours for Observed Data:");
        lookbackLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        lookbackTF = new Text(timeSpanGroup, SWT.BORDER);
        lookbackTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 75;
        Label lookforwardLbl = new Label(timeSpanGroup, SWT.RIGHT);
        lookforwardLbl
                .setText("Number of Lookforward Hours for Forecast Data:");
        lookforwardLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        lookforwardTF = new Text(timeSpanGroup, SWT.BORDER);
        lookforwardTF.setLayoutData(gd);
    }

    /**
     * Create the 'String to Use for Missing' and 'Default Number of Hrs Before
     * Expiration of' groups and controls.
     */
    private void createStringAndDefalutGroups() {
        Composite groupComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        gl.marginWidth = 0;
        groupComp.setLayout(gl);

        // ---------------------------------------------
        // String to Use group
        // ---------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group stringUseGroup = new Group(groupComp, SWT.NONE);
        stringUseGroup.setLayout(new GridLayout(2, false));
        stringUseGroup.setLayoutData(gd);
        stringUseGroup.setText(" String to Use for Missing ");

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 35;
        Label dataValueLbl = new Label(stringUseGroup, SWT.RIGHT);
        dataValueLbl.setText("Data Value:");
        dataValueLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        dataValueTF = new Text(stringUseGroup, SWT.BORDER);
        dataValueTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 35;
        Label stageCatLbl = new Label(stringUseGroup, SWT.RIGHT);
        stageCatLbl.setText("Stage Category:");
        stageCatLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        stageCatTF = new Text(stringUseGroup, SWT.BORDER);
        stageCatTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 35;
        Label dataTimeLbl = new Label(stringUseGroup, SWT.RIGHT);
        dataTimeLbl.setText("Data Time:");
        dataTimeLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        dataTimeTF = new Text(stringUseGroup, SWT.BORDER);
        dataTimeTF.setLayoutData(gd);

        // ---------------------------------------------
        // Default Number group
        // ---------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group defaultNumGroup = new Group(groupComp, SWT.NONE);
        defaultNumGroup.setLayout(new GridLayout(2, false));
        defaultNumGroup.setLayoutData(gd);
        defaultNumGroup.setText(" Default Number of Hrs Before Expiration of ");

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 80;
        Label rvsLbl = new Label(defaultNumGroup, SWT.RIGHT);
        rvsLbl.setText("RVS:");
        rvsLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        rvsTF = new Text(defaultNumGroup, SWT.BORDER);
        rvsTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 80;
        Label flsLbl = new Label(defaultNumGroup, SWT.RIGHT);
        flsLbl.setText("FLS:");
        flsLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        flsTF = new Text(defaultNumGroup, SWT.BORDER);
        flsTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 80;
        Label flwLbl = new Label(defaultNumGroup, SWT.RIGHT);
        flwLbl.setText("FLW:");
        flwLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        flwTF = new Text(defaultNumGroup, SWT.BORDER);
        flwTF.setLayoutData(gd);
    }

    /**
     * Create the update button.
     */
    private void createUpdateButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        updateBtn = new Button(buttonComp, SWT.PUSH);
        updateBtn.setText("Update");
        updateBtn.setLayoutData(gd);
        updateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });
    }

    /**
     * Create the close button and the bottom of the dialog.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private void getDialogData() {
        try {
            ArrayList<RPFParamData> data = HydroDBDataManager.getInstance()
                    .getData(RPFParamData.class);

            if (data != null && data.size() > 0) {
                // There should only be one record
                paramData = data.get(0);
            } else {
                paramData = null;
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        updateDialogDisplay();
    }

    private void updateDialogDisplay() {
        if (paramData != null) {
            lookbackTF.setText(HydroDataUtils.getDisplayString(paramData
                    .getObservationHours()));
            lookforwardTF.setText(HydroDataUtils.getDisplayString(paramData
                    .getForecastHours()));

            dataValueTF.setText(paramData.getMissingValue());
            stageCatTF.setText(paramData.getMissingCategory());
            dataTimeTF.setText(paramData.getMissingTime());

            rvsTF.setText(HydroDataUtils.getDisplayString(paramData
                    .getRvsHours()));
            flsTF.setText(HydroDataUtils.getDisplayString(paramData
                    .getFlsHours()));
            flwTF.setText(HydroDataUtils.getDisplayString(paramData
                    .getFlwHours()));
        }
    }

    private void saveRecord() {
        RPFParamData newData = new RPFParamData();

        Integer temp;

        // Observed Hours
        temp = HydroDataUtils.getIntegerFromTF(shell, lookbackTF,
                "Number of Lookback Hours for Observed Data", true);
        if (temp == null) {
            return;
        }
        newData.setObservationHours(temp);

        // Forecast Hours
        temp = HydroDataUtils.getIntegerFromTF(shell, lookforwardTF,
                "Number of Lookforward Hours for Forecast Data", true);
        if (temp == null) {
            return;
        }
        newData.setForecastHours(temp);

        // Missing Val
        newData.setMissingValue(dataValueTF.getText());

        // Missing Cat
        newData.setMissingCategory(stageCatTF.getText());

        // Missing Time
        newData.setMissingTime(dataTimeTF.getText());

        // Expire RVS
        temp = HydroDataUtils.getIntegerFromTF(shell, rvsTF, "RVS", true);
        if (temp == null) {
            return;
        }
        newData.setRvsHours(temp);

        // Expire FLS
        temp = HydroDataUtils.getIntegerFromTF(shell, flsTF, "FLS", true);
        if (temp == null) {
            return;
        }
        newData.setFlsHours(temp);

        // Expire FLW
        temp = HydroDataUtils.getIntegerFromTF(shell, flwTF, "FLW", true);
        if (temp == null) {
            return;
        }
        newData.setFlwHours(temp);

        try {
            HydroDBDataManager.getInstance().putData(newData);

            getDialogData();
        } catch (VizException e) {
            e.printStackTrace();
        }
    }
}
