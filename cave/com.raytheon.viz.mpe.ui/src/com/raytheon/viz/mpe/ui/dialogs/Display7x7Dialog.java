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

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;
import com.raytheon.viz.mpe.ui.Activator;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class Display7x7Dialog extends CaveSWTDialog {

    private Label gageID;

    private Label gageValue;

    private Scale valueScale;

    private Label valueLabel;

    private Button setBad;

    private static String gageIdent;

    private static String gageVal;

    private static int scaleVal;

    private static int xOrig;

    private static int yOrig;

    private static int ht;

    private static int width;

    private static String scaleValLab;

    private final Label[][] grid = new Label[7][7];

    private static Button undoMissing;

    private static Boolean undoEn = true;

    private static Rectangle extent;

    private MPEFieldResource resource;
    
    private static MPEGageData selectedGage;

    private static MPEGageData workingGage;

    private final Map<String, MPEGageData> editGage = new HashMap<String, MPEGageData>();

    private final ArrayList<String> badGage = new ArrayList<String>();

    private final ArrayList<String> notBadGage = new ArrayList<String>();

    private short[][] xmGrid;

    private ColorMapParameters parameters;

    private DisplayFieldData[] displayTypes = MPEDisplayManager.mpe_qpe_fields;

    private Combo prodSetCbo;

    private Font font;

    private Composite comp2;

    private Composite compG;

    private UnitConverter cvt;

    private Label lbl;

    private Composite gridComp;

    private MPEGageData gData;

    private Button setMissing;

    private DisplayFieldData selectedFieldData;

    private static boolean oldManedit = false;

    public Display7x7Dialog(Shell parentShell, MPEGageData data) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Display 7 X 7 Gage Editing Utility");
        selectedGage = data;
        gData = MPEDataManager.getInstance().getEditedGage(selectedGage);
        ArrayList<String> bg = MPEDataManager.getInstance().readBadGageList();
        if (bg.size() > 0) {
            for (String gageId : bg) {
                badGage.add(gageId);
            }
        }
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

        undoEn = false;

        if (gData != null) {
            workingGage = new MPEDataManager.MPEGageData();
            workingGage = gData;
            undoEn = true;
        } else if (editGage.containsKey(selectedGage.getId())) {
            workingGage = editGage.get(selectedGage.getId());
            undoEn = true;
        } else {
            workingGage = new MPEDataManager.MPEGageData();
            workingGage = selectedGage;
            undoEn = false;
        }
        gageIdent = workingGage.getId();
        xOrig = ((int) (workingGage.getHrap().x - 3));
        yOrig = ((int) (workingGage.getHrap().y - 3));
        ht = 7;
        width = 7;
        extent = new Rectangle(xOrig, yOrig, ht, width);

        populateGrid();

        createProductListComp();
        createGageGridComp();
        createGageComp();
        create7x7GridComp();
        createScaleComp();
        createButtonBar();
    }

    @Override
    protected void disposed() {
        super.disposed();
        font.dispose();

        if (!editGage.isEmpty()) {
            Iterator<MPEGageData> x = editGage.values().iterator();
            for (int i = 0; i < editGage.size(); i++) {
                MPEGageData gd = x.next();
                MPEDataManager.getInstance().addEditedGage(gd);
            }
        }
        if (!badGage.isEmpty() && !editGage.isEmpty()) {
            for (int i = 0; i < badGage.size(); i++) {
                String gd = badGage.get(i);
                MPEDataManager.getInstance().addBadGage(gd);
            }
        }
        if (!notBadGage.isEmpty() && !editGage.isEmpty()) {
            for (int i = 0; i < notBadGage.size(); i++) {
                String gd = notBadGage.get(i);
                MPEDataManager.getInstance().removeBadGage(gd);
            }
        }
        if ((!notBadGage.isEmpty() || !badGage.isEmpty())
                && !editGage.isEmpty()) {
            MPEDataManager.getInstance().writeBadGageList();
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /**
     * 
     */
    private void populateGrid() {
        resource = MPEDisplayManager.getCurrent().getDisplayedFieldResource();
        if (resource == null) {
            return;
        }
        parameters = resource.getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        short[][] data = new short[7][7];
        if (resource != null) {
            Rectangle rect = resource.getHrapExtent();

            if (rect == null) {
                return;
            }

            try {
                short[] xmData = resource.getData();
                for (int i = 0; i < 7; ++i) {
                    for (int j = 0; j < 7; ++j) {
                        short val = -999;
                        int tmpJ = extent.x - rect.x + j;
                        int tmpI = rect.height - 1
                                - (extent.y + extent.height - 1 - rect.y) + i;
                        if (tmpI >= 0 && tmpJ >= 0 && tmpI < rect.height
                                && tmpJ < rect.width) {
                            val = xmData[tmpI * rect.width + tmpJ];
                        }
                        data[i][j] = val;
                    }
                }
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        "Error getting displayed MPE field data", e);
            }
        }
        xmGrid = data;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    private void createProductListComp() {
        // Create a container to hold the label and the combo box.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite prodListComp = new Composite(shell, SWT.NONE);
        GridLayout prodListCompLayout = new GridLayout(2, false);
        prodListComp.setLayout(prodListCompLayout);
        prodListComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label prodSetsLbl = new Label(prodListComp, SWT.CENTER);
        prodSetsLbl.setText("Field:");
        prodSetsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prodSetCbo = new Combo(prodListComp, SWT.LEFT | SWT.DROP_DOWN
                | SWT.READ_ONLY);
        int selector = 0;
        DisplayFieldData dstype = MPEDisplayManager.getCurrent()
                .getDisplayFieldType();
        selectedFieldData = dstype;
        for (; selector < displayTypes.length; ++selector) {
            if (displayTypes[selector] == dstype) {
                break;
            }
        }
        String[] displayTypeNames = new String[displayTypes.length];
        for (int i = 0; i < displayTypes.length; ++i) {
            displayTypeNames[i] = displayTypes[i].toString();
        }

        prodSetCbo.setTextLimit(35);
        prodSetCbo.setLayoutData(gd);
        prodSetCbo.setItems(displayTypeNames);
        prodSetCbo.select(selector);
        prodSetCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateGridField(displayTypes[prodSetCbo.getSelectionIndex()]);
            }
        });
    }

    private void createGageGridComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        compG = new Composite(shell, SWT.NONE);
        GridLayout compGLayout = new GridLayout(2, false);
        compG.setLayout(compGLayout);
        compG.setLayoutData(gd);

    }

    private void createGageComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite comp1 = new Composite(compG, SWT.NONE);
        GridLayout prodListCompLayout = new GridLayout(1, true);
        comp1.setLayout(prodListCompLayout);
        comp1.setLayoutData(gd);

        if ((workingGage.getGval() == -999.f)
                || (workingGage.getGval() == -9999.f)) {
            gageVal = "missing";
            scaleVal = 0;
            scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
        } else if (workingGage.isIs_bad()) {
            gageVal = "bad";
            scaleVal = 0;
            scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
        }

        if (!workingGage.getEdit().isEmpty()) {

            switch (workingGage.getEdit().charAt(0)) {

            case 'M':
            case 'm':
                gageVal = "missing";
                break;

            case 'b':
            case 'B':
                gageVal = "bad";
                break;

            default:

                if (workingGage.getId().contains("PSEUDO")) {
                    if (workingGage.getGval() != -999.f) {
                        UnitConverter conv = SI.MILLIMETER
                                .getConverterTo(NonSI.INCH);
                        float gval = (float) conv
                                .convert(workingGage.getGval());
                        gageVal = String.format("%.2f", gval) + " in.";
                        scaleVal = ((int) (100 * gval));
                        scaleValLab = String.format("%4.2f",
                                (scaleVal / 100.0f));
                    } else {
                        gageVal = "missing";
                        scaleVal = 0;
                        scaleValLab = String.format("%4.2f",
                                (scaleVal / 100.0f));
                    }

                } else {
                    gageVal = workingGage.getEdit();
                    scaleVal = ((int) (100 * Float.parseFloat((workingGage
                            .getEdit()))));
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                }
            }

        } else {

            if (workingGage.getId().contains("PSEUDO")) {
                if (workingGage.getGval() != -999.f) {
                    UnitConverter conv = SI.MILLIMETER
                            .getConverterTo(NonSI.INCH);
                    float gval = (float) conv.convert(workingGage.getGval());
                    gageVal = String.format("%.2f", gval) + " in.";
                    scaleVal = ((int) (100 * gval));
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                } else {
                    gageVal = "missing";
                    scaleVal = 0;
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                }
            } else {
                if ((workingGage.getGval() == -999.f)
                        || (workingGage.getGval() == -9999.f)) {
                    gageVal = "missing";
                    scaleVal = 0;
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                } else {
                    gageVal = String.format("%.2f", workingGage.getGval())
                            + " in.";
                    if (workingGage.getGval() == 0) {
                        scaleVal = (0);
                        scaleValLab = String.format("%4.2f", 0.0);
                    } else {
                        scaleVal = ((int) (100 * workingGage.getGval() - 0.01));
                        if (scaleVal == 0) {
                            scaleValLab = String.format("%4.2f", 0.0);
                        } else {
                            scaleValLab = String.format("%4.2f",
                                    (scaleVal / 100.0f));
                        }
                    }
                }
            }
        }

        gageID = new Label(comp1, SWT.NONE);
        gageID.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        gageID.setAlignment(SWT.CENTER);
        gageID.setText(gageIdent);

        gageValue = new Label(comp1, SWT.BORDER);
        gageValue
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        gageValue.setAlignment(SWT.CENTER);
        gageValue.setText(gageVal);

        // leave some space
        new Label(comp1, SWT.NONE);

        Button setValue = new Button(comp1, SWT.PUSH);
        setValue.setText("Set Value");
        setValue.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        setValue.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                undoEn = true;
                int selVal = valueScale.getSelection();
                String sval = String.format("%4.2f", selVal / 100.0f);
                String xval = sval + " in.";
                gageValue.setText(xval);
                workingGage.setEdit(sval);
                valueLabel.setText(sval);
                undoMissing.setEnabled(undoEn);
                String wid = workingGage.getId();
                editGage.put(wid, workingGage);
            }

        });

        undoMissing = new Button(comp1, SWT.PUSH);
        undoMissing.setEnabled(undoEn);
        undoMissing.setText("Undo Missing");
        undoMissing.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        undoMissing.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                String gval = null;
                undoEn = false;
                undoMissing.setEnabled(undoEn);
                String wid = workingGage.getId();
                editGage.remove(wid);
                if ((workingGage.getGval() == -999.f)
                        || (workingGage.getGval() == -9999.f)) {
                    gval = "missing";
                    gageValue.setText(gval);
                    workingGage.setEdit("");
                    workingGage.setManedit(oldManedit);
                    valueLabel.setText("0.00");
                    valueScale.setSelection(0);
                } else {
                    gval = String.format("%.2f", workingGage.getGval());
                    String xval = gval + " in.";
                    gageValue.setText(xval);
                    workingGage.setEdit("");
                    workingGage.setManedit(oldManedit);
                    valueLabel.setText(gval);
                    valueScale.setSelection(((int) (100 * Float
                            .parseFloat(gval))));
                }

            }

        });

        setMissing = new Button(comp1, SWT.PUSH);
        setMissing.setText("Set Missing");
        setMissing.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        setMissing.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                String gval = null;
                undoEn = true;
                undoMissing.setEnabled(undoEn);
                String wid = workingGage.getId();
                gval = "missing";
                valueLabel.setText("0.00");
                valueScale.setSelection(0);
                gageValue.setText(gval);
                workingGage.setEdit("m");
                oldManedit = workingGage.isManedit();
                workingGage.setManedit(true);
                editGage.put(wid, workingGage);
            }
        });

        Button close = new Button(comp1, SWT.PUSH);
        close.setText("Close");
        close.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        close.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }

        });

        setBad = new Button(comp1, SWT.PUSH);
        if ((workingGage.getId().contains("PSEUDO"))) {
            setBad.setText("Set Bad");
            setBad.setEnabled(false);
        } else {
            if (workingGage.isIs_bad() == true) {
                setBad.setText("Set Not Bad");
            } else if (workingGage.isIs_bad() == false) {
                setBad.setText("Set Bad");
            }
        }
        setBad.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        setBad.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (setBad.getText() == "Set Not Bad") {
                    workingGage.setIs_bad(false);
                    String gval = null;
                    String wid = workingGage.getId();
                    badGage.remove(wid);
                    notBadGage.add(wid);
                    if ((workingGage.getGval() == -999.f)
                            || (workingGage.getGval() == -9999.f)) {
                        gval = "missing";
                        gageValue.setText(gval);
                        workingGage.setEdit("");
                        workingGage.setManedit(oldManedit);
                        valueLabel.setText("0.00");
                        valueScale.setSelection(0);
                    } else {
                        gval = String.format("%.2f", workingGage.getGval());
                        String xval = gval + " in.";
                        gageValue.setText(xval);
                        workingGage.setEdit("");
                        workingGage.setManedit(oldManedit);
                        valueLabel.setText(gval);
                        valueScale.setSelection(((int) (100 * Float
                                .parseFloat(gval))));
                    }
                    setBad.setText("Set Bad");
                } else {
                    workingGage.setIs_bad(true);
                    String gval = null;
                    String wid = workingGage.getId();
                    gval = "bad";
                    valueLabel.setText("0.00");
                    valueScale.setSelection(0);
                    gageValue.setText(gval);
                    workingGage.setEdit("b");
                    oldManedit = workingGage.isManedit();
                    workingGage.setManedit(true);
                    editGage.put(wid, workingGage);
                    if (!badGage.contains(wid)) {
                        badGage.add(wid);
                    }
                    if (notBadGage.contains(wid)) {
                        notBadGage.remove(wid);
                    }
                    setBad.setText("Set Not Bad");
                }
            }
        });
    }

    private void create7x7GridComp() {
        comp2 = new Composite(compG, SWT.NONE);
        comp2.setLayout(new GridLayout(1, true));
        parameters = resource.getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        gridComp = new Composite(comp2, SWT.NONE);
        GridLayout layout = new GridLayout(7, true);
        gridComp.setLayout(layout);

        for (int i = 0; i < 7; i++) {
            for (int j = 0; j < 7; j++) {
                lbl = new Label(gridComp, SWT.BORDER);
                lbl.setAlignment(SWT.RIGHT);
                lbl.setText(String.format(" %4.2f ", i + j / 100.0f));
                grid[i][j] = lbl;
            }
        }
        gridComp.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                cvt = parameters.getDataToDisplayConverter();
                for (int i = 0; i < 7; i++) {
                    for (int j = 0; j < 7; j++) {
                        float xmval = 0;
                        if (xmGrid[i][j] < 0) {
                            xmval = xmGrid[i][j];
                        } else {
                            short ao = xmGrid[i][j];
                            if (ao < 30 && ao > 24) {
                                ao = 26;
                            } else if (ao > 0 && ao <= 24) {
                                ao = 0;
                            }
                            xmval = (float) cvt.convert(ao);
                        }

                        if (xmval < 0) {
                            grid[i][j].setText(String.format(" M "));
                        } else {
                            float aa = (float) ((int) (xmval * 100) / 100.0);
                            grid[i][j].setText(String.format(" %4.2f ", aa));
                        }
                        RGB color = setColor(xmval);

                        org.eclipse.swt.graphics.Color c;
                        c = new org.eclipse.swt.graphics.Color(getDisplay(),
                                color);
                        Label l = grid[i][j];
                        l.setBackground(c);

                        c.dispose();
                        RGB contrastRGB = getContrast(color);
                        c = new org.eclipse.swt.graphics.Color(getDisplay(),
                                contrastRGB);
                        l.setForeground(c);
                        c.dispose();

                        grid[i][j] = l;
                    }
                }

            }

        });
    }

    private void createScaleComp() {
        // leave some space
        new Label(comp2, SWT.NONE);

        valueLabel = new Label(comp2, SWT.NONE);
        valueLabel.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        valueLabel.setAlignment(SWT.CENTER);
        valueLabel.setText(scaleValLab);

        valueScale = new Scale(comp2, SWT.HORIZONTAL);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        valueScale.setLayoutData(data);
        valueScale.setMinimum(0);
        valueScale.setMaximum(350);
        valueScale.setSelection(scaleVal);
        valueScale.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = valueScale.getSelection();
                valueLabel.setText(String.format("%4.2f", sel / 100.0f));
            }

        });

        Label l = new Label(comp2, SWT.NONE);
        l.setText("Edit Gage Value");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonBar(org.eclipse.swt.widgets
     * .Composite)
     */
    protected Control createButtonBar() {
        Composite composite = new Composite(shell, SWT.NONE);
        composite.setLayoutData(new GridData(0, 0));
        return composite;
    }

    private RGB setColor(float val) {
        float value = val;

        if (value == -999.0 || value == -9.0) {
            value = -9999.0f;
        }
        if (value == -899.0) {
            value = -8888.0f;
        }

        Color color = parameters.getColorByValue(value);
        return new RGB((int) (color.getRed() * 255),
                (int) (color.getGreen() * 255), (int) (color.getBlue() * 255));
    }

    public void updateGageData(MPEGageData data) {
        selectedGage = data;
        gData = MPEDataManager.getInstance().getEditedGage(selectedGage);

        if (gData != null) {
            workingGage = new MPEDataManager.MPEGageData();
            workingGage = gData;
            undoEn = true;
        } else if (editGage.containsKey(selectedGage.getId())) {
            workingGage = editGage.get(selectedGage.getId());
            undoEn = true;
        } else {
            workingGage = new MPEDataManager.MPEGageData();
            workingGage = selectedGage;
            undoEn = false;
        }
        gageIdent = workingGage.getId();
        xOrig = ((int) (workingGage.getHrap().x - 3));
        yOrig = ((int) (workingGage.getHrap().y - 3));
        ht = 7;
        width = 7;
        extent = new Rectangle(xOrig, yOrig, ht, width);

        if ((workingGage.getGval() == -999.f)
                || (workingGage.getGval() == -9999.f)) {
            gageVal = "missing";
            scaleVal = 0;
            scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
        } else if (workingGage.isIs_bad()) {
            gageVal = "bad";
            scaleVal = 0;
            scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
        }

        if (!workingGage.getEdit().isEmpty()) {

            switch (workingGage.getEdit().charAt(0)) {

            case 'M':
            case 'm':
                gageVal = "missing";
                break;

            case 'B':
            case 'b':
                gageVal = "bad";
                break;

            default:

                if (workingGage.getId().contains("PSEUDO")) {
                    if (workingGage.getGval() != -999.f) {
                        UnitConverter conv = SI.MILLIMETER
                                .getConverterTo(NonSI.INCH);
                        float gval = (float) conv
                                .convert(workingGage.getGval());
                        gageVal = String.format("%.2f", gval) + " in.";
                        scaleVal = ((int) (100 * gval));
                        scaleValLab = String.format("%4.2f",
                                (scaleVal / 100.0f));
                        setBad.setText("Set Bad");
                        setBad.setEnabled(false);
                    } else {
                        gageVal = "missing";
                        scaleVal = 0;
                        scaleValLab = String.format("%4.2f",
                                (scaleVal / 100.0f));
                    }

                } else {
                    gageVal = workingGage.getEdit();
                    scaleVal = ((int) (100 * Float.parseFloat((workingGage
                            .getEdit()))));
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                }
            }

        } else {

            if (workingGage.getId().contains("PSEUDO")) {
                if (workingGage.getGval() != -999.f) {
                    UnitConverter conv = SI.MILLIMETER
                            .getConverterTo(NonSI.INCH);
                    float gval = (float) conv.convert(workingGage.getGval());
                    gageVal = String.format("%.2f", gval) + " in.";
                    scaleVal = ((int) (100 * gval));
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                    setBad.setText("Set Bad");
                    setBad.setEnabled(false);
                } else {
                    gageVal = "missing";
                    scaleVal = 0;
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                }
            } else {
                if ((workingGage.getGval() == -999.f)
                        || (workingGage.getGval() == -9999.f)) {
                    gageVal = "missing";
                    scaleVal = 0;
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                } else {
                    gageVal = String.format("%.2f", workingGage.getGval())
                            + " in.";
                    scaleVal = ((int) (100 * workingGage.getGval()));
                    scaleValLab = String.format("%4.2f", (scaleVal / 100.0f));
                }
            }
        }
        gageID.setText(gageIdent);
        gageValue.setText(gageVal);
        valueScale.setSelection(scaleVal);
        valueLabel.setText(String.format("%4.2f", scaleVal / 100.0f));
        if (gageVal.equalsIgnoreCase("bad")) {
            setBad.setText("Set Not Bad");
        }
        if (gageVal.equalsIgnoreCase("missing")) {
            setMissing.setEnabled(false);
        }
        updateGridField(displayTypes[prodSetCbo.getSelectionIndex()]);
    }

    private void updateGridField(DisplayFieldData fieldType) {
        MPEDisplayManager mgr = MPEDisplayManager.getCurrent();
        if (selectedFieldData != fieldType) {
            selectedFieldData = fieldType;
            mgr.displayFieldData(fieldType);
            populateGrid();
            gridComp.notifyListeners(SWT.Paint, new Event());
        }
    }

    /**
     * Get the contrasting color to specified color
     * 
     * @param RGB
     * 
     * @return RGB contrast color
     */
    private static RGB getContrast(RGB rgb) {
        RGB xc;
        int r = rgb.red;
        int g = rgb.green;
        int b = rgb.blue;

        // If color is near black set the contrast to white
        if ((r + g + b) / 3 == 0) {
            xc = new RGB(255, 255, 255);
        } else {

            if (rgb.green <= 127) {
                g = 255;
            } else {
                g = 10;
            }

            if (rgb.blue <= 127) {
                b = 255;
            } else {
                b = 10;
            }
            xc = new RGB(r, g, b);
        }
        return xc;
    }

}
