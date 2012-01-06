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
package com.raytheon.viz.skewt.ui;

/**
 * SkewTControlsDlg.java Nov 28, 2007
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  Nov 28, 2007		     Lee Venable Initial Creation
 *  Nov 29, 2007          Eric Babin  Updated for D2D look and feel.
 *                        Tied in fuctions back to SkewtDisplay. 
 *  09Sept2008            dhladky     Made interactive.
 *  09Jan2009             dhladky     hardened it for release.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

import java.util.Iterator;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.ParcelLift.PARCEL_TYPE;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.skewt.SkewTDescriptor;
import com.raytheon.viz.skewt.SkewtDisplay;
import com.raytheon.viz.skewt.rsc.InteractiveSkewTResource;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;

public class SkewtControlsDialog extends CaveJFACEDialog {

    private static final UnitConverter celciusToFahrenheit = SI.CELSIUS
            .getConverterTo(NonSI.FAHRENHEIT);

    private static final UnitConverter celciusToKelvin = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    protected static final UnitConverter fahrenheitToCelcius = NonSI.FAHRENHEIT
            .getConverterTo(SI.CELSIUS);

    protected static final UnitConverter knotsToMetersPerSecond = NonSI.KNOT
            .getConverterTo(SI.METERS_PER_SECOND);

    public String dialogTitle;

    private Composite top = null;

    private String modelName;

    private Button pmaxRdo;

    private Button surfaceRdo;

    private Button meanTempRdo;

    private Button userSelectRdo;

    private Button useFcstMaxChk;

    private Text userSelectTF;

    private Button liftParcelBtn;

    private Text pTempTF;

    private Text tTempTF;

    private Text tdTempTF;

    private Button celsiusRdo;

    private Button fahrenheitRdo;

    private Button addPntSkewtBtn;

    private Text pSpdTF;

    private Text dirSpdTF;

    private Text spdSpdTF;

    private Button ktsRdo;

    private Button msRdo;

    private Button addPntHodoBtn;

    private Button helicityStormChk;

    private Button wetBulbChk;

    private Button resetHodoSkewtBtn;

    private boolean isParcel = false;

    private String pressureError = "Pressure must be between 973 mb && 100 mb";

    private String tempDwptError = "Temp && dewpoint must be between -115 && 45 deg C (-175 && 113 deg F)";

    private String windDirectionError = "Wind direction must be between 0 && 360 deg";

    private String windSpeedError = "Wind speed must be between 0 && 250 kts (0 & 129 m/s)";

    private SkewtDisplay theDisplay = null;

    static private SkewtControlsDialog instance = null;

    private static Double PRESSURE_MAX = 973.0;

    private static Double PRESSURE_MIN = 100.0;

    private static PARCEL_TYPE ptype;

    private Shell shell = null;

    InteractiveSkewTResource rsc = null;

    AbstractEditor editor = null;

    // make it a singleton
    public static SkewtControlsDialog getInstance(Shell parShell,
            String modelName) {
        if (instance == null) {
            try {
                instance = new SkewtControlsDialog(parShell, modelName); // ,
                // display);
            } catch (VizException ve) {
                ve.printStackTrace();
            }
        }

        return instance;
    }

    /**
     * Skew-T control default constructor.
     * 
     * @param parShell
     * @param modelName
     * @param SkewtDisplay
     * @throws VizException
     */
    private SkewtControlsDialog(Shell parShell, String modelName)
            throws VizException {
        super(parShell);
        shell = parShell;
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.modelName = modelName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        // Initialize all of the menus, controls, and layouts
        initializeComponents();

        return top;
    }

    private void initializeComponents() {

        createMenus();
        createModelName();
        createLiftingMethod();
        createAddChangePointSkewT();
        createAddChangePointHodograph();
        createAdditionalInformation();
        createRestoreOriginalData();
        getRsc();
        ptype = rsc.getPtype();
        switch (ptype) {
        case PMAX:
            pmaxRdo.setSelection(true);
            break;
        case USERSELECT:
            userSelectRdo.setSelection(true);
            break;
        case SURFACE:
            surfaceRdo.setSelection(true);
            useFcstMaxChk.setEnabled(true);
            useFcstMaxChk.setSelection(rsc.getFcstMax());
            break;
        case MEANTEMP:
            meanTempRdo.setSelection(true);
            break;
        }
    }

    /**
     * create the Menu bar
     */
    private void createMenus() {
        Menu menuBar = new Menu(this.getShell(), SWT.BAR);
        createFileMenu(menuBar);
        this.getShell().setMenuBar(menuBar);
    }

    /**
     * file operations menu
     */
    private void createFileMenu(Menu menuBar) {
        MenuItem fileI = new MenuItem(menuBar, SWT.CASCADE);
        fileI.setText("File");

        Menu fileMenu = new Menu(menuBar);
        fileI.setMenu(fileMenu);

        // Open file menu item
        MenuItem openMI = new MenuItem(fileMenu, SWT.NONE);
        openMI.setText("&Open\tCtrl+O");
        openMI.setAccelerator(SWT.CTRL + 'O');
        openMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FileDialog fd = new FileDialog(shell, SWT.OPEN);
                String path = fd.open();
                if (path == null) {
                    return;
                }
                rsc.loadSounding(path);
            }
        });

        // Save file menu item
        MenuItem saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("&Save\tCtrl+S");
        saveMI.setAccelerator(SWT.CTRL + 'S');
        saveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FileDialog fd = new FileDialog(shell, SWT.SAVE);
                String path = fd.open();
                if (path == null) {
                    return;
                }
                rsc.saveSounding(path);
            }
        });

        // Save as... file menu item
        MenuItem saveasMI = new MenuItem(fileMenu, SWT.NONE);
        saveasMI.setText("Save &As...");
        saveasMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FileDialog fd = new FileDialog(shell, SWT.SAVE);
                String path = fd.open();
                if (path == null) {
                    return;
                }
                rsc.saveSounding(path);
            }
        });

        // Exit menu item
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("E&xit\tCtrl+X");
        exitMI.setAccelerator(SWT.CTRL + 'X');
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

    }

    /**
     * create model name.
     */
    private void createModelName() {
        Composite labelComp = new Composite(top, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        labelComp.setLayout(gl);

        createCenteredLabel(labelComp, 2, modelName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        // do nothing
    }

    /**
     * Create centered Label.
     * 
     * @param c
     * @param numCols
     * @param text
     * @return Label
     */
    private Label createCenteredLabel(Composite c, int numCols, String text) {
        Label label = new Label(c, SWT.NONE | SWT.CENTER);
        label.setText(text);

        GridData gridData = new GridData();
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalSpan = numCols;
        gridData.horizontalAlignment = GridData.FILL;
        gridData.verticalAlignment = GridData.FILL;
        label.setLayoutData(gridData);

        return label;
    }

    /**
     * Create the lifting method.
     */
    private void createLiftingMethod() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group liftingGroup = new Group(top, SWT.NONE);

        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        liftingGroup.setLayout(gl);
        liftingGroup.setLayoutData(gd);

        createCenteredLabel(liftingGroup, 2, "Lifting Method");

        pmaxRdo = new Button(liftingGroup, SWT.RADIO);
        pmaxRdo.setText("PMAX");
        pmaxRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (pmaxRdo.getSelection() == true) {
                    ptype = PARCEL_TYPE.PMAX;
                    useFcstMaxChk.setSelection(false);
                    useFcstMaxChk.setEnabled(false);
                    rsc.setFcstMax(false);
                }
            }
        });

        // filler
        new Label(liftingGroup, SWT.NONE);

        surfaceRdo = new Button(liftingGroup, SWT.RADIO);
        surfaceRdo.setText("Surface");
        surfaceRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (surfaceRdo.getSelection() == true) {
                    ptype = PARCEL_TYPE.SURFACE;
                    useFcstMaxChk.setEnabled(true);
                    useFcstMaxChk.setSelection(rsc.getFcstMax());
                }
            }
        });

        useFcstMaxChk = new Button(liftingGroup, SWT.CHECK);
        useFcstMaxChk.setText("Use Fcst Max Temp");
        useFcstMaxChk.setEnabled(false);
        useFcstMaxChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (useFcstMaxChk.getSelection() == true) {
                    rsc.setFcstMax(true);
                } else {
                    rsc.setFcstMax(false);
                }
            }
        });

        meanTempRdo = new Button(liftingGroup, SWT.RADIO);
        meanTempRdo.setText("Mean Temp");
        meanTempRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (meanTempRdo.getSelection() == true) {
                    ptype = PARCEL_TYPE.MEANTEMP;
                    useFcstMaxChk.setSelection(false);
                    useFcstMaxChk.setEnabled(false);
                    rsc.setFcstMax(false);
                }
            }
        });
        // filler
        new Label(liftingGroup, SWT.NONE);

        userSelectRdo = new Button(liftingGroup, SWT.RADIO);
        userSelectRdo.setText("User Select");
        userSelectRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (userSelectRdo.getSelection() == true) {
                    userSelectTF.setEnabled(true);
                    ptype = PARCEL_TYPE.USERSELECT;
                    useFcstMaxChk.setSelection(false);
                    useFcstMaxChk.setEnabled(false);
                    rsc.setFcstMax(false);
                } else {
                    userSelectTF.setEnabled(false);
                }
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        userSelectTF = new Text(liftingGroup, SWT.BORDER);
        userSelectTF.setLayoutData(gd);
        userSelectTF.setEnabled(false);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        gd.widthHint = 150;
        liftParcelBtn = new Button(liftingGroup, SWT.PUSH);
        liftParcelBtn.setText("Lift Parcel");
        liftParcelBtn.setLayoutData(gd);

        liftParcelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                isParcel = true;
                if (!ptype.equals(PARCEL_TYPE.USERSELECT)) {
                    rsc.changeParcel(ptype, 0);
                } else {
                    rsc.changeParcel(ptype, Float.parseFloat(userSelectTF
                            .getText()));
                }
            }
        });
    }

    private void createAddChangePointSkewT() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group addChangeGroup = new Group(top, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 10;
        addChangeGroup.setLayout(gl);
        addChangeGroup.setLayoutData(gd);

        int lblTextWidth = 75;

        createCenteredLabel(addChangeGroup, 3, "Add/Change Point to Skew-T");
        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        Label pLbl = new Label(addChangeGroup, SWT.CENTER);
        pLbl.setText("P:");
        pLbl.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        Label tLbl = new Label(addChangeGroup, SWT.CENTER);
        tLbl.setText("T:");
        tLbl.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        Label tdLbl = new Label(addChangeGroup, SWT.CENTER);
        tdLbl.setText("Td:");
        tdLbl.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        pTempTF = new Text(addChangeGroup, SWT.BORDER);
        pTempTF.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        tTempTF = new Text(addChangeGroup, SWT.BORDER);
        tTempTF.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        tdTempTF = new Text(addChangeGroup, SWT.BORDER);
        tdTempTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        Label tempsLbl = new Label(addChangeGroup, SWT.CENTER);
        tempsLbl.setText("Temps Entered As:");
        tempsLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        gd.widthHint = 100;
        celsiusRdo = new Button(addChangeGroup, SWT.RADIO);
        celsiusRdo.setText("Celsius");
        celsiusRdo.setLayoutData(gd);
        celsiusRdo.setSelection(true);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        gd.widthHint = 100;
        fahrenheitRdo = new Button(addChangeGroup, SWT.RADIO);
        fahrenheitRdo.setText("Fahrenheit");
        fahrenheitRdo.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        gd.widthHint = 175;
        addPntSkewtBtn = new Button(addChangeGroup, SWT.PUSH);
        addPntSkewtBtn.setText("Add Point to Skew-T");
        addPntSkewtBtn.setLayoutData(gd);
        addPntSkewtBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                double min = SkewTConstants.TEMPERATURE_MIN;
                double max = SkewTConstants.TEMPERATURE_MAX;

                double press = SoundingLayer.MISSING;
                double temp = SoundingLayer.MISSING;
                double dwpt = SoundingLayer.MISSING;

                if (fahrenheitRdo.getSelection() == true) {
                    min = celciusToFahrenheit
                            .convert(SkewTConstants.TEMPERATURE_MIN);
                    max = celciusToFahrenheit
                            .convert(SkewTConstants.TEMPERATURE_MAX);
                }

                press = fieldIsValid(pTempTF, PRESSURE_MIN, PRESSURE_MAX,
                        pressureError);
                temp = fieldIsValid(tTempTF, min, max, tempDwptError);
                dwpt = fieldIsValid(tdTempTF, min, max, tempDwptError);

                if ((press != SoundingLayer.MISSING
                        && temp != SoundingLayer.MISSING && dwpt != SoundingLayer.MISSING)) {

                    if (fahrenheitRdo.getSelection() == true) {
                        dwpt = fahrenheitToCelcius.convert(dwpt);
                        temp = fahrenheitToCelcius.convert(temp);
                    }
                    // Layer values need to be in Kelvin
                    dwpt = celciusToKelvin.convert(dwpt);
                    temp = celciusToKelvin.convert(temp);

                    // can't happen
                    if (dwpt > temp) {
                        MessageDialog.openError(new Shell(), "Error on field",
                                "Dewpoint can't exceed Temperature");
                        tdTempTF.setFocus();
                    } else {
                        rsc.addSkewtPointAction(press, temp, dwpt);
                        resetFields();
                    }
                }
            }
        });
    }

    /**
     * Changing a point on the Hodo.
     */
    private void createAddChangePointHodograph() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group addChangeGroup = new Group(top, SWT.NONE);

        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 10;
        addChangeGroup.setLayout(gl);
        addChangeGroup.setLayoutData(gd);

        createCenteredLabel(addChangeGroup, 3, "Add/Change Point to Hodograph");

        int lblTextWidth = 75;

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        Label pLbl = new Label(addChangeGroup, SWT.CENTER);
        pLbl.setText("P:");
        pLbl.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        Label dirLbl = new Label(addChangeGroup, SWT.CENTER);
        dirLbl.setText("Dir:");
        dirLbl.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        Label speedLbl = new Label(addChangeGroup, SWT.CENTER);
        speedLbl.setText("Spd:");
        speedLbl.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        pSpdTF = new Text(addChangeGroup, SWT.BORDER);
        pSpdTF.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        dirSpdTF = new Text(addChangeGroup, SWT.BORDER);
        dirSpdTF.setLayoutData(gd);

        gd = new GridData(lblTextWidth, SWT.DEFAULT);
        spdSpdTF = new Text(addChangeGroup, SWT.BORDER);
        spdSpdTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        Label tempsLbl = new Label(addChangeGroup, SWT.CENTER);
        tempsLbl.setText("Speed Entered As:");
        tempsLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        gd.widthHint = 100;
        ktsRdo = new Button(addChangeGroup, SWT.RADIO);
        ktsRdo.setText("kts");
        ktsRdo.setLayoutData(gd);
        ktsRdo.setSelection(true);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        gd.widthHint = 100;
        msRdo = new Button(addChangeGroup, SWT.RADIO);
        msRdo.setText("m/s");
        msRdo.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        gd.widthHint = 200;
        addPntHodoBtn = new Button(addChangeGroup, SWT.PUSH);
        addPntHodoBtn.setText("Add Point to Hodograph");
        addPntHodoBtn.setLayoutData(gd);

        addPntHodoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                double max = SkewTConstants.WIND_SPEED_MAX;
                double min = SkewTConstants.WIND_SPEED_MIN;

                double press = SoundingLayer.MISSING;
                double spd = SoundingLayer.MISSING;
                double dir = SoundingLayer.MISSING;

                if (msRdo.getSelection()) {
                    max = knotsToMetersPerSecond
                            .convert(SkewTConstants.WIND_SPEED_MAX);
                }

                press = fieldIsValid(pSpdTF, PRESSURE_MIN, PRESSURE_MAX,
                        pressureError);
                dir = fieldIsValid(dirSpdTF, SkewTConstants.WIND_DIR_MIN,
                        SkewTConstants.WIND_DIR_MAX, windDirectionError);
                spd = fieldIsValid(spdSpdTF, min, max, windSpeedError);

                if ((press != SoundingLayer.MISSING
                        && spd != SoundingLayer.MISSING && dir != SoundingLayer.MISSING)) {

                    if (ktsRdo.getSelection()) {
                        // Speed must be in m/s to add new layer
                        spd = knotsToMetersPerSecond.convert(spd);
                    }
                    rsc.addChangePointAction(press, spd, dir);
                    resetFields();
                }
            }
        });

    }

    /**
     * Is the field valid?
     * 
     * @param theField
     * @param minimum
     * @param max
     * @param errorMsg
     * @return double
     */
    private double fieldIsValid(Text theField, double minimum, double max,
            String errorMsg) {
        double value = SoundingLayer.MISSING;

        try {
            value = Integer.parseInt(theField.getText());
            if ((value < minimum) || value > max) {
                MessageDialog
                        .openError(new Shell(), "Error on field", errorMsg);
                theField.setFocus();
                return SoundingLayer.MISSING;
            }
        } catch (NumberFormatException nfe) {
            MessageDialog.openError(new Shell(), "Error on field", errorMsg);

            return SoundingLayer.MISSING;
        }

        return value;
    }

    /**
     * Create additional information
     */
    private void createAdditionalInformation() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group addInfoGroup = new Group(top, SWT.NONE);

        GridLayout gl = new GridLayout(1, false);
        addInfoGroup.setLayout(gl);
        addInfoGroup.setLayoutData(gd);

        createCenteredLabel(addInfoGroup, 3,
                "Additional Hodo/Skew-T Information");

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        helicityStormChk = new Button(addInfoGroup, SWT.CHECK);
        helicityStormChk.setText("Helicity/Storm Inflow");
        helicityStormChk.setLayoutData(gd);
        helicityStormChk.setSelection(false);
        helicityStormChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setStormInflow(((Button) e.getSource()).getSelection());
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        wetBulbChk = new Button(addInfoGroup, SWT.CHECK);
        wetBulbChk.setText("Wet-bulb Temp Profile");
        wetBulbChk.setLayoutData(gd);
        wetBulbChk.setSelection(false);
        wetBulbChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setWetBulb(((Button) e.getSource()).getSelection());
            }
        });
    }

    /**
     * Create and restore original data.
     */
    private void createRestoreOriginalData() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group restoreGroup = new Group(top, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        restoreGroup.setLayout(gl);
        restoreGroup.setLayoutData(gd);

        createCenteredLabel(restoreGroup, 2, "Restore to Original Data Profile");

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        gd.widthHint = 200;
        resetHodoSkewtBtn = new Button(restoreGroup, SWT.PUSH);
        resetHodoSkewtBtn.setText("Reset Data");
        resetHodoSkewtBtn.setLayoutData(gd);
        resetHodoSkewtBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                resetSkewtAction();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Skew-T Controls");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected Point getInitialSize() {
        return new Point(320, 835);
    }

    /**
     * reset
     */
    private void resetSkewtAction() {
        rsc.resetSounding();
        // if (this.theDisplay.getTheEditableSkewtResource().isEditable()) {
        // this.theDisplay.getTheEditableSkewtResource().getSounding().reset();
        // }
    }

    /**
     * reset the fields
     */
    private void resetFields() {
        pTempTF.setText("");
        tTempTF.setText("");
        tdTempTF.setText("");
        pSpdTF.setText("");
        dirSpdTF.setText("");
        spdSpdTF.setText("");
    }

    /**
     * wet bulb line draw
     * 
     * @return
     */
    public void setWetBulb(boolean wetBulb) {
        rsc.setWb(wetBulb);
        editor.refresh();
    }

    /**
     * storm relative inflow line draw
     * 
     * @return
     */
    public void setStormInflow(boolean storm) {
        rsc.set_plotStorm(storm);
        editor.refresh();
    }

    /**
     * override of close method
     */
    @Override
    public boolean close() {
        if (rsc.isEditable() == true) {
            rsc.setEditable(false);
            isParcel = false;
        }
        return super.close();
    }

    /**
     * A hard normal exit
     */
    public void exit() {
        super.close();
    }

    /**
     * Gets the display you are using
     * 
     * @return SkewTDisplay
     */
    private SkewtDisplay getSkewtDisplay() {

        editor = (AbstractEditor) PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().getActiveEditor();
        if (editor.getActiveDisplayPane().getDescriptor() instanceof SkewTDescriptor) {
            // the ordering in the editor is paramount here!!!!!
            if (editor.getActiveDisplayPane().getRenderableDisplay() instanceof SkewtDisplay) {
                return ((SkewtDisplay) (editor.getActiveDisplayPane()
                        .getRenderableDisplay()));
            }
        }
        return null;
    }

    private InteractiveSkewTResource getRsc() {
        SkewtDisplay st = getSkewtDisplay();
        if (st == null) {
            return null;
        }
        ResourceList rl = st.getDescriptor().getResourceList();
        Iterator<?> it = rl.iterator();
        while (it.hasNext()) {
            ResourcePair rp = (ResourcePair) it.next();
            if (rp.getResource() instanceof InteractiveSkewTResource) {
                rsc = (InteractiveSkewTResource) rp.getResource();
                break;
            }
        }
        return rsc;
    }

}
