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
package com.raytheon.viz.mpe.ui.dialogs;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.DisplayMeanArealPrecipResource;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResource;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResourceData.ArealDisplay;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class MultiHourPrecipAccDialog extends CaveSWTDialog {

    private Font font;

    private Scale durScale;

    private Label durScaleValLbl;

    private Spinner daySpinner;

    private Calendar cal;

    private Date prevDate;

    private Spinner hourSpinner;

    private Text yearText;

    private Text monthText;

    private Button valChk;

    private Button idChk;

    private Combo prodSetCbo;

    private final MPEDisplayManager displayMgr = MPEDisplayManager.getCurrent();

    private final MPEDataManager dataMgr = MPEDataManager.getInstance();

    private String[] accumAreaTypes = { "Grid", "Basin", "County", "Zone" };

    private ArealDisplay[] arealTypeObjects = { ArealDisplay.GRID,
            ArealDisplay.BASIN, ArealDisplay.COUNTY, ArealDisplay.ZONE };

    private String[] precipAccumInterval = { "1 Hour", "3 Hour", "6 Hour",
            "12 Hour", "24 Hour", "36 Hour", "48 Hour", "72 Hour", "Other..." };

    private int[] precipAccumInt = { 1, 3, 6, 12, 24, 36, 48, 72 };

    private String[] dispTypeName;

    private int accum_interval = 1;

    private DisplayFieldData[] displayTypes;

    private Combo dispCbo;

    private DisplayMeanArealPrecipResource dma;

    public MultiHourPrecipAccDialog(Shell parentShell) {
        super(parentShell);
        setText("Multi-Hour Precipitation Accumulation");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
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
        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        prevDate = displayMgr.getCurrentEditDate();
        cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(prevDate);

        displayTypes = MPEDisplayManager.mpe_qpe_fields;
        dispTypeName = new String[displayTypes.length];
        for (int i = 0; i < displayTypes.length; i++) {
            dispTypeName[i] = displayTypes[i].toString();
        }

        createProdListComp(shell);
        createIntervalComp(shell);
        createEndTimeComp(shell);
        updateTimeControls();
        createAccDispComp(shell);
        createButtonComp(shell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        font.dispose();
    }

    /**
     * Create the data options group and controls.
     */
    private void createProdListComp(Shell shell) {

        // Create a container to hold the label and the combo box.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite prodListComp = new Composite(shell, SWT.NONE);
        GridLayout prodListCompLayout = new GridLayout(2, false);
        prodListComp.setLayout(prodListCompLayout);
        prodListComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label prodSetsLbl = new Label(prodListComp, SWT.CENTER);
        prodSetsLbl.setText("Product to Accumulate:");
        prodSetsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prodSetCbo = new Combo(prodListComp, SWT.LEFT | SWT.DROP_DOWN
                | SWT.READ_ONLY);

        int selector = -1;
        DisplayFieldData currData = displayMgr.getDisplayFieldType();
        if (currData == null) {
            currData = DisplayFieldData.Xmrg;
        }
        for (selector = 0; selector < displayTypes.length; ++selector) {
            if (displayTypes[selector] == currData) {
                break;
            }
        }

        prodSetCbo.setTextLimit(35);
        prodSetCbo.setLayoutData(gd);
        prodSetCbo.setItems(dispTypeName);
        prodSetCbo.select(selector);
    }

    private void createIntervalComp(Shell shell) {
        Group intervalOptionsGroup = new Group(shell, SWT.NONE);
        intervalOptionsGroup.setText("Accumulation Interval Setup");
        GridLayout groupLayout = new GridLayout(1, false);
        groupLayout.marginWidth = 1;
        intervalOptionsGroup.setLayout(groupLayout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        intervalOptionsGroup.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite accIntComp = new Composite(intervalOptionsGroup, SWT.NONE);
        GridLayout accIntCompLayout = new GridLayout(3, false);
        accIntComp.setLayout(accIntCompLayout);
        accIntComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label durLbl = new Label(accIntComp, SWT.LEFT);
        durLbl.setText("Duration:");
        durLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        final Combo durCbo = new Combo(accIntComp, SWT.DEFAULT);
        durCbo.setItems(precipAccumInterval);
        durCbo.select(0);
        durCbo.setLayoutData(gd);
        durCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (durCbo.getText() == "Other...") {
                    durScale.setVisible(true);
                    durScaleValLbl.setVisible(true);
                } else {
                    durScale.setSelection(1);
                    durScaleValLbl.setText(" 1 Hr");
                    durScale.setVisible(false);
                    durScaleValLbl.setVisible(false);
                    accum_interval = precipAccumInt[durCbo.getSelectionIndex()];
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        durScaleValLbl = new Label(accIntComp, SWT.LEFT);
        durScaleValLbl.setAlignment(SWT.LEFT);
        durScaleValLbl.setLayoutData(gd);
        durScaleValLbl.setVisible(false);
        durScaleValLbl.setText(" 1 Hr");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.horizontalSpan = 2;
        durScale = new Scale(accIntComp, SWT.BORDER);
        durScale.setMinimum(1);
        durScale.setMaximum(72);
        durScale.setSelection(0);
        durScale.setLayoutData(gd);
        durScale.setVisible(false);
        durScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = durScale.getSelection();
                durScaleValLbl.setText(String.format("%2d Hr", sel));
                accum_interval = sel;
            }
        });
        durScale.addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                int sel = durScale.getSelection();
                durScaleValLbl.setText(String.format("%2d Hr", sel));
                accum_interval = sel;
            }
        });

    }

    private void createEndTimeComp(Shell shell) {
        // create date area
        Group dateComp = new Group(shell, SWT.SHADOW_ETCHED_IN);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dateComp.setLayoutData(data);
        GridLayout gl = new GridLayout(4, false);
        dateComp.setLayout(gl);
        dateComp.setText("Ending Time");

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label yearLabel = new Label(dateComp, SWT.NONE);
        yearLabel.setLayoutData(gd);
        yearLabel.setText("Year");

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label monthLabel = new Label(dateComp, SWT.NONE);
        monthLabel.setLayoutData(gd);
        monthLabel.setText("Month");

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label dayLabel = new Label(dateComp, SWT.NONE);
        dayLabel.setLayoutData(gd);
        dayLabel.setText("Day");

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label hrLabel = new Label(dateComp, SWT.NONE);
        hrLabel.setLayoutData(gd);
        hrLabel.setText("Hour");

        yearText = new Text(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 100;
        yearText.setLayoutData(data);

        monthText = new Text(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 50;
        monthText.setLayoutData(data);

        daySpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 50;
        daySpinner.setLayoutData(data);
        daySpinner.setMinimum(0);
        daySpinner.setMaximum(32);
        daySpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int day = daySpinner.getSelection();

                cal.set(Calendar.DAY_OF_MONTH, day);
                updateTimeControls();
            }

        });
        hourSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 50;
        hourSpinner.setLayoutData(data);
        hourSpinner.setMinimum(-1); // this works in eclipse 3.4+
        hourSpinner.setMaximum(24);
        hourSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int hour = hourSpinner.getSelection();

                cal.set(Calendar.HOUR_OF_DAY, hour);
                updateTimeControls();
            }

        });
    }

    private void createAccDispComp(Shell shell) {
        Group accDispCompGroup = new Group(shell, SWT.SHADOW_ETCHED_IN);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        accDispCompGroup.setLayoutData(data);
        GridLayout gl = new GridLayout(1, false);
        accDispCompGroup.setLayout(gl);
        accDispCompGroup.setText("Accumulation Display Control");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite dispComp = new Composite(accDispCompGroup, SWT.NONE);
        GridLayout dispCompLayout = new GridLayout(3, false);
        dispComp.setLayout(dispCompLayout);
        dispComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label dispLbl = new Label(dispComp, SWT.LEFT);
        dispLbl.setText("Display As:");
        dispLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        dispCbo = new Combo(dispComp, SWT.DEFAULT);
        dispCbo.setItems(accumAreaTypes);
        dispCbo.select(0);
        dispCbo.setLayoutData(gd);
        dispCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dispCbo.getText().equalsIgnoreCase("Grid")) {
                    valChk.setEnabled(false);
                    idChk.setEnabled(false);
                } else {
                    valChk.setEnabled(true);
                    idChk.setEnabled(true);
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label annLbl = new Label(dispComp, SWT.LEFT);
        annLbl.setText("Annotate:");
        annLbl.setLayoutData(gd);

        valChk = new Button(dispComp, SWT.CHECK);
        valChk.setText("Values");
        valChk.setData(0);
        valChk.setEnabled(false);

        idChk = new Button(dispComp, SWT.CHECK);
        idChk.setText("Ids");
        idChk.setData(1);
        idChk.setEnabled(false);
    }

    private void createButtonComp(Shell shell) {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite okBtnComp = new Composite(shell, SWT.NONE);
        GridLayout okBtnCompLayout = new GridLayout(2, true);
        okBtnComp.setLayout(okBtnCompLayout);
        okBtnComp.setLayoutData(gd);

        GridData bd = new GridData(110, 30);
        Button okBtn = new Button(okBtnComp, SWT.PUSH);
        okBtn.setText("Show Data");
        okBtn.setLayoutData(bd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                showAccumulationData();
            }
        });

        bd = new GridData(110, 30);
        Button cancelBtn = new Button(okBtnComp, SWT.PUSH);
        cancelBtn.setText("Close");
        cancelBtn.setLayoutData(bd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dma != null) {
                    displayMgr.getRenderableDisplay().getDescriptor()
                            .getResourceList().removeRsc(dma);
                }
                close();
            }
        });
    }

    private void updateTimeControls() {
        if (cal.getTime().before(dataMgr.getEarliestDate())
                || cal.getTime().after(dataMgr.getLatestDate())) {
            cal.setTime(prevDate);
        }
        prevDate = cal.getTime();

        yearText.setText(Integer.toString(cal.get(Calendar.YEAR)));
        monthText.setText(Integer.toString(cal.get(Calendar.MONTH) + 1));
        daySpinner.setSelection(cal.get(Calendar.DAY_OF_MONTH));
        hourSpinner.setSelection(cal.get(Calendar.HOUR_OF_DAY));
    }

    private void showAccumulationData() {
        int accumHrs = accum_interval;
        Date editTime = cal.getTime();
        ArealDisplay arealDisplay = arealTypeObjects[dispCbo
                .getSelectionIndex()];
        DisplayFieldData displayField = displayTypes[prodSetCbo
                .getSelectionIndex()];

        IRenderableDisplay display = displayMgr.getRenderableDisplay();
        IDescriptor descriptor = display.getDescriptor();
        if (displayMgr.setCurrentEditDate(editTime)) {
            if (dma != null) {
                descriptor.getResourceList().removeRsc(dma);
                dma = null;
            }

            if (arealDisplay == ArealDisplay.GRID) {
                displayMgr.displayFieldData(displayField, accumHrs,
                        arealDisplay);
                MPEFieldResource resource = displayMgr
                        .getDisplayedFieldResource();
                resource.getResourceData().setDisplayIds(idChk.getSelection());
                resource.getResourceData().setDisplayValues(
                        valChk.getSelection());
                resource.issueRefresh();
            } else {
                // TODO: Move functionality of this resource to
                // AbstractMPEGriddedResource and delete this one
                displayMgr.displayFieldData(displayField);
                dma = new DisplayMeanArealPrecipResource(displayMgr,
                        arealDisplay.name(), displayField, accumHrs);
                DisplayMeanArealPrecipResource.vals = valChk.getSelection();
                DisplayMeanArealPrecipResource.ids = idChk.getSelection();
                descriptor.getResourceList().add(dma);
            }
        }
    }
}
