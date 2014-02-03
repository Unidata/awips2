/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.GraphToGridParamDialog
 * 
 * January 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphtogrid;

import gov.noaa.nws.ncep.gempak.parameters.core.contourinterval.CINT;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.ContoursAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.OutlookAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourCircle;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.contours.IContours;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.viz.gempak.nativelib.LibraryLoader;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Dialog to get inputs for Graph-to-Grid prameters.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#215		J. Wu   	Initial Creation.
 * 11/10		#345		J. Wu   	Added support for ContourCircle.
 * 10/11        #450        G. Hull     use localization names from NcPathConstants
 * 10/11		?			J. Wu   	Remove entry if the given table does not exist.
 * 08/13		TTR778		J. Wu		Load libg2g when this dialog is created.
 * 12/13        1090        J. Wu       Allow either table or element applied to g2g.
 * 
 * </pre>
 * 
 * @author J. Wu
 */

public class GraphToGridParamDialog extends CaveJFACEDialog {

    private static LinkedHashMap<String, String> productMaps = null;

    private static HashMap<String, String> currentProductParams = null;

    private static ArrayList<String> productNames = null;

    private static String grphgdTblName = PgenStaticDataProvider.getProvider()
            .getPgenLocalizationRoot() + "grphgd.tbl"; // "grphgd.tbl";

    private static ArrayList<HashMap<String, String>> productDefaults = null;

    private static final int BASIC_ADV_ID = IDialogConstants.CLIENT_ID + 7585;

    private static final String BASIC_LABEL = "Basic...";

    private static final String ADVANCED_LABEL = "Advanced...";

    private static final int SHOW_EXT_ID = IDialogConstants.CLIENT_ID + 7586;

    private static final String SHOW_EXT_LABEL = "Show Extensions";

    private static final int MAKE_GRID_ID = IDialogConstants.CLIENT_ID + 7587;

    private static final String MAKE_GRID_LABEL = "Make Grid";

    private static final int CANCEL_ID = IDialogConstants.CLIENT_ID + 7588;

    private static final String CANCEL_LABEL = "Cancel";

    private static final int H_SPACING = 12;

    private static int NCYCLES = 4;

    private static int CYCLE_INTERVAL = 12;

    private static int NFCSTHRS = 13;

    private static int FCSTHUR_INTREVAL = 6;

    private Color[] extColor = new Color[] { Color.blue };

    private Composite top = null;

    private Combo prdCombo = null;

    private Combo cycleCombo = null;

    private Combo fcstCombo = null;

    private Group advancedGrp = null;

    private ArrayList<Text> paramText = null;

    private String currentPrd = null;

    private Button displayOption = null;

    private AttrDlg cntAttrDlg = null;

    private boolean applyTableInfoToOutput = true;

    private Button infoOptBtn = null;

    /**
     * Constructor
     * 
     * @param parShell
     * @throws VizException
     */
    public GraphToGridParamDialog(Shell parShell) throws VizException {

        super(parShell);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);

        // Load the native library.
        LibraryLoader.load("g2g");

        if (productMaps == null) {
            productMaps = GraphToGrid.loadParameters(grphgdTblName);
            productNames = new ArrayList<String>(productMaps.keySet());
        }

        if (productDefaults == null) {

            productDefaults = new ArrayList<HashMap<String, String>>();

            for (String str : productNames) {

                String value = productMaps.get(str);
                String fileName = value.substring(value.lastIndexOf('/') + 1);

                HashMap<String, String> map = GraphToGrid
                        .loadParameters(PgenStaticDataProvider.getProvider()
                                .getPgenLocalizationRoot()
                                + File.separator
                                + fileName);

                if (map.size() > 0) {
                    productDefaults.add(map);
                } else {
                    productMaps.remove(str);
                }

            }

        }

    }

    /*
     * Add buttons to the button bar.
     */
    @Override
    public void createButtonsForButtonBar(Composite parent) {

        // used to display/hide the advanced info
        createButton(parent, BASIC_ADV_ID, ADVANCED_LABEL, true);

        // used to show the contours extensions
        createButton(parent, SHOW_EXT_ID, SHOW_EXT_LABEL, true);

        // used to make grid
        createButton(parent, MAKE_GRID_ID, MAKE_GRID_LABEL, true);

        // used to exit the dialog
        createButton(parent, CANCEL_ID, CANCEL_LABEL, true);

    }

    /**
     * Creates the dialog area
     */
    @Override
    public Control createDialogArea(Composite parent) {

        top = (Composite) super.createDialogArea(parent);
        this.getShell().setText("GRAPH-to-GRID Processing");

        // Create the main layout for the dialog.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        // Menu to choose a product.
        Composite prdComp = new Composite(top, SWT.NONE);
        GridLayout layout = new GridLayout(3, false);
        layout.horizontalSpacing = 25;
        prdComp.setLayout(layout);

        Label prdLbl = new Label(prdComp, SWT.LEFT);
        prdLbl.setText("PRODUCT");

        prdCombo = new Combo(prdComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (String st : productMaps.keySet()) {
            prdCombo.add(st);
        }
        prdCombo.add("-none-");

        prdCombo.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub
            }

            @Override
            public void widgetSelected(SelectionEvent e) {

                String newPrd = ((Combo) e.widget).getText();
                if (!newPrd.equals(currentPrd)) {
                    currentPrd = newPrd;
                    applyTableInfoToOutput = true;
                    if (advancedGrp != null && infoOptBtn != null) {
                        infoOptBtn.setText("Apply Elem Info");
                    }
                    currentProductParams = retrievePrdMap(currentPrd);
                    currentProductParams.putAll(generateParameters());
                    setParameters(currentProductParams);
                }
            }
        });

        prdCombo.select(0);
        currentPrd = prdCombo.getText();

        displayOption = new Button(prdComp, SWT.CHECK);
        displayOption.setText("Show Result as a Ghost Contours");
        displayOption.setSelection(true);

        createBasicInfo(top);

        currentProductParams = retrievePrdMap(currentPrd);
        currentProductParams.putAll(generateParameters());

        return top;

    }

    /*
     * open the dialog
     */
    @Override
    public int open() {

        if (this.getShell() == null) {
            this.create();
        }

        Point pt = this.getShell().getParent().getLocation();
        this.getShell().setLocation(pt.x + 400, pt.y);

        return super.open();
    }

    /**
     * Close the dialogs.
     */
    @Override
    public boolean close() {

        if (advancedGrp != null && paramText != null) {

            Control[] wids = advancedGrp.getChildren();

            if (wids != null) {
                for (int kk = 0; kk < wids.length; kk++) {
                    wids[kk].dispose();
                }
            }

            advancedGrp.dispose();

            advancedGrp = null;

            paramText = null;

        }

        return super.close();

    }

    /*
     * 
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {

        if (buttonId == BASIC_ADV_ID) {
            // Toggle between the basic and advance info panel.
            if (getButton(BASIC_ADV_ID).getText().equals(ADVANCED_LABEL)) {
                createAdvancedInfo(top);
                getButton(BASIC_ADV_ID).setText(BASIC_LABEL);

                // Update parameters for writing out grid to GEMPAK grid file
                currentProductParams.putAll(generateParameters());

                setParameters(currentProductParams);

            } else {
                disposeAdvancedInfo();
                getButton(BASIC_ADV_ID).setText(ADVANCED_LABEL);
            }
        } else if (buttonId == SHOW_EXT_ID) {
            showExtension();
        }

        else if (buttonId == MAKE_GRID_ID) {
            makeGrid();
        }

        else if (buttonId == CANCEL_ID) {
            disposeAdvancedInfo();
            this.close();
        }

    }

    /*
     * Create the basic info panel
     */
    private void createBasicInfo(Composite comp) {

        Group g1 = new Group(comp, SWT.SHADOW_ETCHED_IN);
        g1.setText("Basic Info");
        g1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        GridLayout layout = new GridLayout(4, false);
        layout.horizontalSpacing = H_SPACING;
        g1.setLayout(layout);

        // Cycle dropdown
        Label cycleLbl = new Label(g1, SWT.LEFT);
        cycleLbl.setText("CYCLE");

        GridData gdata1 = new GridData(75, 20);
        GridData gdata2 = new GridData(190, 25);

        cycleLbl.setLayoutData(gdata1);

        cycleCombo = new Combo(g1, SWT.DROP_DOWN);
        cycleCombo.setLayoutData(gdata2);
        for (String str : buildCycles(NCYCLES, CYCLE_INTERVAL)) {
            cycleCombo.add(str);
        }
        cycleCombo.select(0);

        // Forecast hour dropdown
        Label fcstLbl = new Label(g1, SWT.LEFT);
        fcstLbl.setText("FCST_HR");
        cycleLbl.setLayoutData(gdata1);

        fcstCombo = new Combo(g1, SWT.DROP_DOWN);
        fcstCombo.setLayoutData(gdata2);
        for (String str : buildFcstHrs(NFCSTHRS, FCSTHUR_INTREVAL)) {
            fcstCombo.add(str);
        }

        fcstCombo.select(0);

        if (cntAttrDlg instanceof ContoursAttrDlg) {
            Contours curCnt = ((ContoursAttrDlg) cntAttrDlg)
                    .getCurrentContours();
            String fcsthr = "";
            if (curCnt != null) {
                fcsthr = curCnt.getForecastHour();
            } else {
                fcsthr = ((ContoursAttrDlg) cntAttrDlg).getForecastHour();
            }

            int index = -1;
            boolean found = false;
            for (String str : fcstCombo.getItems()) {
                if (str.equals(fcsthr)) {
                    found = true;
                    break;
                }

                index++;
            }

            if (found) {
                fcstCombo.select(index + 1);
            }

        }

    }

    /*
     * Create the advanced info panel
     */
    private void createAdvancedInfo(Composite comp) {

        advancedGrp = new Group(comp, SWT.SHADOW_ETCHED_IN);
        advancedGrp.setText("Advanced Info");
        advancedGrp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        GridLayout layout = new GridLayout(1, false);
        advancedGrp.setLayout(layout);

        paramText = new ArrayList<Text>();
        createPanelFromList(advancedGrp, "Grid Navigation",
                GraphToGrid.getGridCalcParams(), paramText, 9, false);
        createPanelFromList(advancedGrp, "Grid Display",
                GraphToGrid.getGridDisplayParams(), paramText, 1, false);
        createPanelFromList(advancedGrp, "Grid Output",
                GraphToGrid.getGridOutputParams(), paramText, 9, true);

        this.getShell().pack();

        this.getShell().layout();

    }

    /*
     * Create a set of Text from a list of strings and group them together.
     */
    private void createPanelFromList(Group parent, String title,
            String[] params, ArrayList<Text> textList, int active,
            boolean addOpt) {

        Group grp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        grp.setText(title);
        GridLayout layout1 = new GridLayout(4, false);
        layout1.horizontalSpacing = H_SPACING;
        grp.setLayout(layout1);

        GridData gdata1 = new GridData(70, 15);
        GridData gdata2 = new GridData(180, 15);

        int ii = 0;
        for (String str : params) {

            Label label = new Label(grp, SWT.LEFT);
            label.setText(str);
            label.setLayoutData(gdata1);

            Text txt = new Text(grp, SWT.SINGLE | SWT.BORDER | SWT.SHADOW_IN);

            txt.setLayoutData(gdata2);
            txt.setEditable(true);

            txt.setData(str);
            if (str.equals("PATH")) {
                txt.setToolTipText("The directory must exist and has lower case only!");
            }

            txt.addKeyListener(new KeyAdapter() {
                public void keyReleased(KeyEvent e) {
                    // Text etxt = (Text)e.widget;
                    // if ( etxt.getData().toString().equals( "PATH" ) ) {
                    // etxt.setText( etxt.getText().toLowerCase() );
                    // }
                }
            });

            // Disable it if it is not implemented yet.....
            if (ii >= active) {
                label.setEnabled(false);
                txt.setEnabled(false);
            }

            ii++;

            textList.add(txt);

        }

        if (addOpt) {
            infoOptBtn = new Button(grp, SWT.TOGGLE);
            infoOptBtn.setSelection(applyTableInfoToOutput);
            if (applyTableInfoToOutput) {
                infoOptBtn.setText("Apply Elem Info");
            } else {
                infoOptBtn.setText("Apply Table Info");
            }
            infoOptBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button btn = (Button) e.widget;
                    if (btn.getSelection()) {
                        applyTableInfoToOutput = true;
                        btn.setText("Apply Elem Info");
                    } else {
                        applyTableInfoToOutput = false;
                        btn.setText("Apply Table Info");
                    }

                    setOutPutParams();
                }

            });
        }

    }

    /*
     * Dispose the advanced info panel.
     */
    private void disposeAdvancedInfo() {

        retrievePrdMap(currentPrd);

        if (advancedGrp != null && paramText != null) {

            Control[] wids = advancedGrp.getChildren();

            if (wids != null) {
                for (int kk = 0; kk < wids.length; kk++) {
                    wids[kk].dispose();
                }
            }

            advancedGrp.dispose();

            advancedGrp = null;

            paramText = null;

        }

        this.getShell().pack();

        this.getShell().layout();

    }

    /*
     * Set the current time for "CYCLE" text field and pulldown menu items.
     * There are _nCycs positions for CYCLE times... in this case, starting w/
     * 12 hrs forward and working backwards.
     */
    private String[] buildCycles(int ncycles, int interval) {

        Calendar cntTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        cntTime.add(Calendar.HOUR_OF_DAY, interval);

        String[] cycleTimes = new String[ncycles];

        String cycle = "";

        int year = cntTime.get(Calendar.YEAR);

        for (int ii = 0; ii < ncycles; ii++) {
            cycle += year - year / 100 * 100;

            if (cntTime.get(Calendar.MONTH) + 1 < 10) {
                cycle += "0";
            }
            cycle += cntTime.get(Calendar.MONTH) + 1;

            if (cntTime.get(Calendar.DAY_OF_MONTH) < 10) {
                cycle += "0";
            }
            cycle += cntTime.get(Calendar.DAY_OF_MONTH);

            cycle += "/";

            int hour = cntTime.get(Calendar.HOUR_OF_DAY) / 12 * 12;
            if (hour < 10) {
                cycle += "0";
            }
            cycle += hour;

            cycle += "00";

            cycleTimes[ii] = new String(cycle.toString());

            cycle = "";

            cntTime.add(Calendar.HOUR_OF_DAY, -interval);

        }

        return cycleTimes;
    }

    /*
     * Set the current time for "CYCLE" text field and pulldown menu items.
     * There are _nCycs positions for CYCLE times... in this case, starting w/
     * 12 hrs forward and working backwards.
     */
    private String[] buildFcstHrs(int nhours, int interval) {

        String[] fcsthrs = new String[nhours];

        String hourStr = "f";
        int value = 0;

        for (int ii = 0; ii < nhours; ii++) {

            value = ii * interval;
            if (value < 10) {
                hourStr += "00";
            } else if (value < 100) {
                hourStr += "0";
            }

            hourStr += value;

            fcsthrs[ii] = new String(hourStr);

            hourStr = "f";

        }

        return fcsthrs;
    }

    /**
     * Set the values for parameters.
     */
    private void setParameters(HashMap<String, String> params) {

        String value;

        value = params.get("CYCLE");
        if (value != null) {
            cycleCombo.setText(value.toString());
        }

        value = params.get("FCST_HR");
        if (value != null) {
            fcstCombo.setText(value.toString());
        }

        if (advancedGrp != null && paramText != null) {

            for (Text txt : paramText) {

                txt.setText("");
                value = params.get(txt.getData().toString());
                if (value != null) {
                    txt.setText(value);
                }
            }
        }

    }

    /**
     * Retrieve the parameter inputs from the dialog.
     */
    public HashMap<String, String> getParameters() {

        HashMap<String, String> params = new HashMap<String, String>();
        if (currentProductParams != null) {
            params.putAll(currentProductParams);
        }

        params.put("PRODUCT", prdCombo.getText());
        params.put("CYCLE", cycleCombo.getText());
        params.put("FCST_HR", fcstCombo.getText());
        if (displayOption.getSelection() == true) {
            params.put("DISPOPT", "TRUE");
        } else {
            params.put("DISPOPT", "FALSE");
        }

        if (advancedGrp != null && paramText != null) {

            for (Text txt : paramText) {
                params.put(txt.getData().toString(), txt.getText());
            }
        }

        // Parse CINT if we are going to use cint from contour element.
        if (!applyTableInfoToOutput) {
            List<String> cints = parseCints(params.get("CINT"));
            String cintStr = "";
            for (String str : cints) {
                cintStr += ";" + str;
            }

            if (cintStr.length() > 0)
                params.put("CINT", cintStr);
        }

        return params;
    }

    /**
     * update the parameters for current product.
     */
    private HashMap<String, String> retrievePrdMap(String prdName) {

        HashMap<String, String> prdMap = new HashMap<String, String>();

        int index = -1;
        for (int ii = 0; ii < productNames.size(); ii++) {
            if (prdName.equals(productNames.get(ii))) {
                index = ii;
                break;
            }
        }

        if (index >= 0) {
            prdMap.putAll(productDefaults.get(index));
        }

        prdMap.put("PRODUCT", prdName);
        prdMap.put("CYCLE", cycleCombo.getText());
        prdMap.put("FCST_HR", fcstCombo.getText());

        if (advancedGrp != null && paramText != null) {
            for (Text txt : paramText) {
                currentProductParams.put(txt.getData().toString(),
                        txt.getText());
            }
        }

        return prdMap;

    }

    /**
     * @param cntAttrDlg
     *            the cntAttrDlg to set
     */
    public void setCntAttrDlg(AttrDlg cntAttrDlg) {
        this.cntAttrDlg = cntAttrDlg;
    }

    /**
     * @return the cntAttrDlg
     */
    public AttrDlg getCntAttrDlg() {
        return cntAttrDlg;
    }

    /**
     * Extend the open contour lines to the boundary of the specified area.
     */
    private void showExtension() {

        // AbstractEditor currentEditor =
        // NmapUiUtils.getActiveNatlCntrsEditor();
        AbstractEditor currentEditor = PgenUtil.getActiveEditor();
        PgenResource drawingLayer = PgenSession.getInstance().getPgenResource();

        Contours cnt = getCurrentContours();

        if (cnt != null) {

            HashMap<String, String> prm = getParameters();

            String proj = GraphToGrid.getParamValues(prm, "PROJ");
            String garea = GraphToGrid.getParamValues(prm, "GRDAREA");
            String kxky = GraphToGrid.getParamValues(prm, "KXKY");

            String[] nkxky = kxky.split(";");

            int kx = 63;
            int ky = 28;
            if (nkxky.length > 1) {
                kx = Integer.parseInt(nkxky[0]);
                ky = Integer.parseInt(nkxky[1]);
            } else {
                // logger.warn( "Invalid input for kx;ky - default to 63;28" );
            }

            CoordinateTransform gtrans = new CoordinateTransform(proj, garea,
                    kx, ky);

            Contours extContours = cnt.copy();

            DECollection extLines = new DECollection("Contours Extensions");

            /*
             * Note: extend the line only, so the label of the line doesn't
             * matter and thus we do not need CatMap here.
             */
            ContoursExtension cntExt = new ContoursExtension(extContours,
                    gtrans, kx, ky, extColor, null);

            extLines.add(cntExt.getExtLines());

            drawingLayer.setGhostLine(extLines);

            currentEditor.refresh();

        }

    }

    /**
     * Graph-to-grid calculation.
     */
    private void makeGrid() {

        Contours cnt = getCurrentContours();

        if (cnt != null) {

            ContoursToGrid cnt2grd = new ContoursToGrid(cnt, getParameters());

            if (cnt2grd != null) {
                cnt2grd.makeGrid();
            }

        }

    }

    /**
     * Retrieve the current contours.
     */
    private Contours getCurrentContours() {

        Contours curCnt = null;
        PgenResource drawingLayer = PgenSession.getInstance().getPgenResource();

        if (cntAttrDlg instanceof ContoursAttrDlg) {

            curCnt = ((ContoursAttrDlg) cntAttrDlg).getCurrentContours();

            if (curCnt == null) {
                DrawableElement de = drawingLayer.getSelectedDE();

                if (de != null
                        && (de.getParent() instanceof ContourLine
                                || de.getParent() instanceof ContourMinmax || de
                                    .getParent() instanceof ContourCircle)) {

                    curCnt = (Contours) de.getParent().getParent();
                }
            }
        } else if (cntAttrDlg instanceof OutlookAttrDlg) {
            curCnt = ((OutlookAttrDlg) cntAttrDlg).getCurrentOtlk();
        }

        return curCnt;
    }

    /**
     * Set the the input for a parameter.
     */
    public void setSingleParameter(String param, String value) {

        if (paramText != null) {
            for (Text txt : paramText) {
                if (txt.getData().toString().equals(param)) {
                    txt.setText(value);
                    break;
                }
            }
        }
    }

    /**
     * generate the default inputs for some parameters.
     */
    private HashMap<String, String> generateParameters() {

        HashMap<String, String> prm = new HashMap<String, String>(
                currentProductParams);

        String gdt = null;
        String gparm = null;
        String level = null;
        String gfunc = null;
        String gdoutf = null;
        String cint = null;

        IContours dlg = (IContours) cntAttrDlg;
        gdt = PgenUtil.calendarToGempakDattim(dlg.getTime1());

        // get values for GPARM, GLEVEL, GFUNC - from table and/or element.
        gparm = dlg.getParm();
        HashMap<String, String> curPrdMapFromTable = getPrdMapFromTable(currentPrd);
        gfunc = curPrdMapFromTable.get("GFUNC");

        if (applyTableInfoToOutput) {
            level = curPrdMapFromTable.get("GLEVEL");
            if (level == null)
                level = dlg.getLevel();

            cint = curPrdMapFromTable.get("CINT");
            if (cint == null)
                cint = dlg.getCint();

        } else {
            level = dlg.getLevel();
            cint = dlg.getCint();
        }

        if (gfunc == null) {
            gfunc = gparm;
        }

        gdoutf = new String(gparm.toLowerCase() + "_"
                + dlg.getTime1().get(Calendar.YEAR) + gdt.substring(2, 6)
                + gdt.substring(7, 9) + fcstCombo.getText() + ".grd");

        prm.put("GPARM", gparm);
        prm.put("GLEVEL", level);
        prm.put("GDATTIM", gdt + fcstCombo.getText());
        prm.put("GFUNC", gfunc);
        prm.put("GDOUTF", gdoutf);

        prm.put("CINT", cint);

        if (cntAttrDlg instanceof OutlookAttrDlg) {
            OutlookAttrDlg odlg = (OutlookAttrDlg) cntAttrDlg;
            String cmap = odlg.getCatmapForType(odlg.getOutlookType());
            prm.put("CATMAP", cmap);
        }

        return prm;

    }

    /**
     * get the table parameters for current product.
     */
    private HashMap<String, String> getPrdMapFromTable(String prdName) {

        HashMap<String, String> prdMap = new HashMap<String, String>();

        int index = -1;
        for (int ii = 0; ii < productNames.size(); ii++) {
            if (prdName.equals(productNames.get(ii))) {
                index = ii;
                break;
            }
        }

        if (index >= 0) {
            prdMap.putAll(productDefaults.get(index));
        }

        prdMap.put("PRODUCT", prdName);
        prdMap.put("CYCLE", cycleCombo.getText());
        prdMap.put("FCST_HR", fcstCombo.getText());

        return prdMap;

    }

    /**
     * Set grid output parameters for current product from element or table.
     */
    private void setOutPutParams() {
        HashMap<String, String> tableParams = generateParameters();
        ArrayList<String> outParams = new ArrayList<String>(
                Arrays.asList(GraphToGrid.getGridOutputParams()));
        for (String outp : tableParams.keySet()) {
            if (inList(outp, outParams)) {
                String value = tableParams.get(outp);
                if (value != null) {
                    currentProductParams.put(outp, value);
                    setSingleParameter(outp, value);
                }
            }
        }

        setSingleParameter("CINT", tableParams.get("CINT"));
    }

    /**
     * Check if a string is in a list of strings.
     */
    private boolean inList(String item, List<String> strList) {
        boolean inlist = false;
        if (item == null || strList == null || strList.size() == 0)
            return inlist;

        for (String str : strList) {
            if (item.equals(str)) {
                inlist = true;
                break;
            }
        }

        return inlist;
    }

    /**
     * Parse a cint string into a list of cint labels.
     */
    public static List<String> parseCints(String contourCint) {

        List<String> cints = new ArrayList<String>();

        CINT cd = new CINT(contourCint);
        if (cd.isCINTStringParsed()) {

            // First figure out how many decimal digits are specified.
            String cint = cd.getCINTString(CINT.FIRST_ZOOM_LEVEL);

            if (cint.contains(";")) {
                cints.addAll(cd
                        .getContourLabelsForZoomLevel(CINT.FIRST_ZOOM_LEVEL));
            } else {

                String[] values = cint.split("/");
                int ndecimals = 0;
                for (String str : values) {
                    int pos = str.indexOf(".");
                    if (pos >= 0) {
                        int nd = str.length() - pos - 1;
                        if (ndecimals < nd)
                            ndecimals = nd;
                    }
                }

                for (String dbl : cd
                        .getContourLabelsForZoomLevel(CINT.FIRST_ZOOM_LEVEL)) {

                    // Format the label to the specified number of decimal
                    // digits.
                    String lblstr = new String(dbl);
                    if (ndecimals == 0) {
                        lblstr = lblstr.substring(0, lblstr.indexOf("."));
                    } else {
                        int len = lblstr.length() - lblstr.indexOf(".") - 1;
                        if (len < ndecimals) {
                            for (int ii = 0; ii < (ndecimals - len); ii++) {
                                lblstr += "0";
                            }
                        }
                    }

                    cints.add(lblstr);
                }

            }
        }

        return cints;
    }

}
