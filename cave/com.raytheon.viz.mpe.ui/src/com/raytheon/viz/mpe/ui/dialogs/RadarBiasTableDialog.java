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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwradarresult;
import com.raytheon.uf.common.dataplugin.shef.tables.DAARadarResult;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData.RadarAvailability;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.radartable.ReadBiasTableParam;

/**
 * Dialog to display Radar bias Table for editing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2009            snaples     Initial creation
 * Jun 18, 2013  16053     snaples     Removed reference to setRadarEditFlag
 * Aug 06, 2013  16243                 Changed the Gui to a ScrolledComposite.
 * Feb 2, 2014   16201     snaples     Added saved data flag support
 * Apr 4, 2014   17223     snaples     Updated other_office_id and rfc_bias to object 
 *                                     array so that called procedure can update and return values properly.
 * May 1, 2014   16626     snaples     Updated the Manual Bias button to allow revert to original value.
 * May 8, 2014   DCS167    cgobs       Updated Dialog for DualPol features
 * May 23, 2014  DCS167    cgobs       Resolved merge conflict
 * Jul 29, 2015  17471     snaples     Added logging for radar result table query date value.
 * Apr 05, 2016  5504      bkowal      Fix GUI sizing issues. Cleanup code - notify users of errors,
 *                                     use status handler, etc.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RadarBiasTableDialog extends Dialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    public static class Zerocoef_Data {
        float mlt_zrcoef;

        float pwr_zrcoef;
    }

    private final Zerocoef_Data abzerocoef = new Zerocoef_Data();

    private Shell shell;

    private Button applyBtn;

    private Button closeBtn;

    Composite bcLblComp;

    private final int retval = 0;

    private final String[] radIds;

    String rid = "";

    private static String[] biasLabelStrings = { "Radar", "SP Bias", "DP Bias",
            "Man. SP Bias", "Man. DP Bias", "A", "B", "Bias", "Other Office" };

    private final Label[] colHdr = new Label[biasLabelStrings.length];

    private String abias = "";

    private String bbias = "";

    private String obias = "";

    private String ooffice = "";

    private String biasSP = "";

    private String mbiasSP = "";

    private String biasDP = "";

    private String mbiasDP = "";

    public static int default_bias = 0;

    private ArrayList<Rwbiasstat> bList = new ArrayList<Rwbiasstat>();

    public static Rwbiasstat rwBias = new Rwbiasstat();

    private Map<String, MPERadarData> radarIdToSPDataMap;

    private Map<String, MPERadarData> radarIdToDPDataMap;

    private MPERadarData radarresultdata = new MPERadarData();

    private MPERadarData daaradarresultdata = new MPERadarData();

    public static String dt = "";

    private static final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyyMMddHH");

    private static final SimpleDateFormat pgsdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private static final SimpleDateFormat st3sdf;

    private Button[] spManEditButtonArray = null;

    private Button[] dpManEditButtonArray = null;

    private Text[] spBiasValueTextArray = null;

    private final Map<String, Integer> spBiasChangeMap = new HashMap<String, Integer>();

    private Text[] dpBiasValueTextArray = null;

    private final Map<String, Integer> dpBiasChangeMap = new HashMap<String, Integer>();

    static {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        pgsdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        String date_form = appsDefaults.getToken("st3_date_form");
        if ((date_form == null) || date_form.isEmpty()
                || date_form.equals("mdY")) {
            st3sdf = new SimpleDateFormat("MMM dd yyyy HH");
            st3sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        } else {
            st3sdf = sdf;
        }
    }

    float[] oldSPBiasValue = new float[60];

    float[] editedSPBiasValue = new float[60];

    float[] oldDPBiasValue = new float[60];

    float[] editedDPBiasValue = new float[60];

    public RadarBiasTableDialog(Shell parentShell, final String[] radIds) {
        super(parentShell);
        this.radIds = radIds;
    }

    /**
     * Open method used to display the Radar Bias Table dialog == Edit Bias
     * Table Dialog in A1.
     * 
     * @return Null.
     */
    public int open() {
        Shell parent = this.getParent();
        Display display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Edit Bias Table");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return retval;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        String fxa_local_site = appsDefaults.getToken("fxa_local_site");
        String where = "WHERE office_id = '" + fxa_local_site + "'";
        bList = IHFSDbGenerated.GetRWBiasstat(where);
        if (!bList.isEmpty()) {
            rwBias = bList.get(0);
            default_bias = rwBias.getNpairBiasSelect();
        }

        createDateComp();
        createBiasColLblComp();
        createBiasListComp();
        createButtonComp();
    }

    private void createButtonComp() {

        // Create a container to hold the buttons.
        GridData gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        Composite applyBtnComp = new Composite(shell, SWT.NONE);
        GridLayout applyBtnCompLayout = new GridLayout(2, true);
        applyBtnComp.setLayout(applyBtnCompLayout);
        applyBtnComp.setLayoutData(gd);

        final int minimumButtonWidth = applyBtnComp.getDisplay().getDPI().x;

        GridData bd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bd.minimumWidth = minimumButtonWidth;
        applyBtn = new Button(applyBtnComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(bd);
        applyBtn.setEnabled(false);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                applyBiasUpdate(dt);
                MPEDisplayManager mgr = MPEDisplayManager.getCurrent();
                mgr.setSavedData(false);
            }
        });

        bd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bd.minimumWidth = minimumButtonWidth;
        closeBtn = new Button(applyBtnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(bd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }

        });
    }

    private void createDateComp() {
        GridData bd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        Composite dtLblComp = new Composite(shell, SWT.NONE);
        GridLayout dtLblCompLayout = new GridLayout(1, false);
        dtLblComp.setLayout(dtLblCompLayout);
        dtLblComp.setLayoutData(bd);
        Label tmslotLbl = new Label(dtLblComp, SWT.CENTER);
        tmslotLbl.setLayoutData(bd);
        Date dt = MPEDisplayManager.getCurrent().getCurrentEditDate();
        String dt3 = st3sdf.format(dt);
        tmslotLbl.setText(dt3 + "z");

    }

    /**
     * Create the composite to hold the data column labels
     */
    private void createBiasColLblComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bcLblComp = new Composite(shell, SWT.NONE);
        GridLayout bcLblCompLayout = new GridLayout(9, true);
        bcLblComp.setLayout(bcLblCompLayout);
        bcLblComp.setLayoutData(gd);
        for (int i = 0; i < biasLabelStrings.length; i++) {
            colHdr[i] = new Label(bcLblComp, SWT.FILL);
            colHdr[i].setText(biasLabelStrings[i]);
            gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            colHdr[i].setLayoutData(gd);
        }

    }

    /**
     * Create the data options group and controls.
     */
    private void createBiasListComp() {
        final ScrolledComposite biasListScrollComp = new ScrolledComposite(
                shell, SWT.BORDER | SWT.V_SCROLL);
        // Create a container to hold the label and the combo box.
        final Composite biasListComp = new Composite(biasListScrollComp,
                SWT.BORDER);

        // dual pol version of table has 9 columns; previous table had 7 columns
        final int numColumns = 9;
        GridLayout biasListCompLayout = new GridLayout(numColumns, true);
        biasListCompLayout.marginWidth = 0;
        biasListComp.setLayout(biasListCompLayout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.horizontalSpan = numColumns;
        biasListComp.setLayoutData(gd);
        biasListComp
                .setSize(biasListComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        Date dt3 = MPEDisplayManager.getCurrent().getCurrentEditDate();

        dt = pgsdf.format(dt3);
        /*
         * This lets us know what date is being requested from radar result
         * tables
         */
        statusHandler.debug("Radar Bias table query using time: " + dt3);

        radarIdToSPDataMap = MPEDataManager.getInstance().readSPRadarData(dt3);
        radarIdToDPDataMap = MPEDataManager.getInstance().readDPRadarData(dt3);

        spBiasValueTextArray = new Text[radIds.length];
        dpBiasValueTextArray = new Text[radIds.length];

        spManEditButtonArray = new Button[radIds.length];
        dpManEditButtonArray = new Button[radIds.length];

        int biasRowHeight = 0;

        if (radIds.length == 0) {
            /*
             * Display "No Data Available" message.
             */
            final Label noDataAvailableLabel = new Label(biasListComp, SWT.NONE);
            noDataAvailableLabel.setText("No Data Available");

            gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            gd.horizontalSpan = numColumns;
            noDataAvailableLabel.setLayoutData(gd);

            GC gc = new GC(noDataAvailableLabel);
            biasRowHeight = gc.getFontMetrics().getHeight()
                    + noDataAvailableLabel.getBorderWidth()
                    + biasListComp.getBorderWidth() + biasListComp.getSize().y;
            gc.dispose();
        }

        for (int i = 0; i < radIds.length; i++) {
            // get A and B coefficients from SP radar (does not apply to DP )
            abzerocoef.mlt_zrcoef = 0.0f;
            abzerocoef.pwr_zrcoef = 0.0f;

            rid = radIds[i];

            if (radarIdToDPDataMap.size() != 0) {
                daaradarresultdata = radarIdToDPDataMap.get(rid);
            }

            if (radarIdToSPDataMap.size() != 0) {
                radarresultdata = radarIdToSPDataMap.get(rid);
            } else {
                continue;
            }

            if (radarresultdata == null) {
                continue;
            }
            if (!radarresultdata.getRadAvail()
                    .equals(RadarAvailability.MISSING)) {
                float[] dpaz = new float[2];
                try {
                    dpaz = ReadBiasTableParam.getDpaadaptcoef(rid, dt);
                } catch (VizException e1) {
                    statusHandler.error(
                            "Failed to retrieve the dpaadapt cofficients for radar: "
                                    + radIds[i] + ". Defaulting to 0.", e1);
                }
                if (dpaz.length != 0) {
                    abzerocoef.mlt_zrcoef = dpaz[0];
                    abzerocoef.pwr_zrcoef = dpaz[1];
                }
            }

            // -----------------------------------------------------

            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);

            // radar id button
            final Button ridBtn = new Button(biasListComp, SWT.PUSH);
            ridBtn.setText(rid);
            ridBtn.setData(rid);
            ridBtn.setLayoutData(gd);

            ridBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    RadarSpanDialog rsd = new RadarSpanDialog(shell,
                            (String) ridBtn.getData());
                    rsd.open();
                }

            });

            biasSP = String
                    .format("%-1.2f", radarresultdata.getRwBiasValUsed());
            oldSPBiasValue[i] = (float) radarresultdata.getRwBiasValUsed();
            editedSPBiasValue[i] = 1.0f;

            biasDP = String.format("%-1.2f",
                    daaradarresultdata.getRwBiasValUsed());
            oldDPBiasValue[i] = (float) daaradarresultdata.getRwBiasValUsed();
            editedDPBiasValue[i] = 1.0f;

            ridBtn.setEnabled(true);

            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);

            // single pol
            final Text lbiasSPTxt = new Text(biasListComp, SWT.SINGLE
                    | SWT.CENTER | SWT.BORDER);

            if (radarresultdata.getEditBias() != null) {
                lbiasSPTxt.setText(biasSP.trim());
            } else {
                lbiasSPTxt.setText("DATA MISSING");
                lbiasSPTxt.setEnabled(false);
            }

            lbiasSPTxt.setLayoutData(gd);
            lbiasSPTxt.setData(i);

            lbiasSPTxt.addModifyListener(new ModifyListener() {
                @Override
                public void modifyText(ModifyEvent e) {
                    final int ei = (Integer) lbiasSPTxt.getData();
                    try {
                        float parsedFloat = Float.parseFloat(lbiasSPTxt
                                .getText());
                        editedSPBiasValue[ei] = parsedFloat;
                        spManEditButtonArray[ei].setText("YES");
                        lbiasSPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_WHITE));
                        spBiasChangeMap.put(radIds[ei], ei);
                        applyBtn.setEnabled(true);
                    } catch (NumberFormatException e1) {
                        lbiasSPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_RED));
                        applyBtn.setEnabled(false);

                    }
                }
            });

            // dual pol
            final Text lbiasDPTxt = new Text(biasListComp, SWT.SINGLE
                    | SWT.CENTER | SWT.BORDER);

            if (daaradarresultdata.getEditBias() != null) {
                lbiasDPTxt.setText(biasDP.trim());
            } else {
                lbiasDPTxt.setText("DATA MISSING");
                lbiasDPTxt.setEnabled(false);
            }
            lbiasDPTxt.setLayoutData(gd);
            lbiasDPTxt.setData(i);

            // -------------------------------------------------------------

            lbiasDPTxt.addModifyListener(new ModifyListener() {
                @Override
                public void modifyText(ModifyEvent e) {
                    final int ei = (Integer) lbiasDPTxt.getData();
                    try {
                        float parsedFloat = Float.parseFloat(lbiasDPTxt
                                .getText());
                        editedDPBiasValue[ei] = parsedFloat;
                        dpManEditButtonArray[ei].setText("YES");
                        lbiasDPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_WHITE));
                        dpBiasChangeMap.put(radIds[ei], ei);
                        applyBtn.setEnabled(true);
                    } catch (NumberFormatException e1) {
                        lbiasDPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_RED));
                        applyBtn.setEnabled(false);
                    }
                }
            });

            // single pol
            spBiasValueTextArray[i] = lbiasSPTxt;
            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            final Button mbiasSPBtn = new Button(biasListComp, SWT.TOGGLE);
            mbiasSP = ("n".equalsIgnoreCase(radarresultdata.getEditBias()) || radarresultdata
                    .getEditBias() == null) ? "NO" : "YES";
            mbiasSPBtn.setText(mbiasSP);
            mbiasSPBtn.setLayoutData(gd);
            mbiasSPBtn.setData(i);
            spManEditButtonArray[i] = mbiasSPBtn;

            mbiasSPBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    final int ai = (Integer) mbiasSPBtn.getData();
                    if ("YES".equalsIgnoreCase(mbiasSPBtn.getText())) {
                        spManEditButtonArray[ai].setSelection(false);
                        editedSPBiasValue[ai] = oldSPBiasValue[ai];
                        spBiasValueTextArray[ai].setText(String.format(
                                "%-1.2f", editedSPBiasValue[ai]));
                        spBiasChangeMap.put(radIds[ai], ai);
                        spManEditButtonArray[ai].setText("NO");
                    }
                }
            });

            // dual pol
            dpBiasValueTextArray[i] = lbiasDPTxt;
            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            final Button mbiasDPBtn = new Button(biasListComp, SWT.TOGGLE);
            mbiasDP = ("n".equalsIgnoreCase(daaradarresultdata.getEditBias()) || daaradarresultdata
                    .getEditBias() == null) ? "NO" : "YES";
            mbiasDPBtn.setText(mbiasDP);
            mbiasDPBtn.setLayoutData(gd);
            mbiasDPBtn.setData(i);
            dpManEditButtonArray[i] = mbiasDPBtn;

            mbiasDPBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    final int ai = (Integer) mbiasDPBtn.getData();
                    if ("YES".equalsIgnoreCase(mbiasDPBtn.getText())) {
                        dpManEditButtonArray[ai].setSelection(false);
                        editedDPBiasValue[ai] = oldDPBiasValue[ai];
                        dpBiasValueTextArray[ai].setText(String.format(
                                "%-1.2f", editedDPBiasValue[ai]));
                        dpBiasChangeMap.put(radIds[ai], ai);
                        applyBtn.setEnabled(false);
                        dpManEditButtonArray[ai].setText("NO");
                    }
                }
            });

            // ---------------------------------------------------------

            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            Label acoefLbl = new Label(biasListComp, SWT.CENTER);
            if (abzerocoef.mlt_zrcoef == 0.0) {
                abias = "N/A";
            } else {
                abias = String.format("%-3.0f", abzerocoef.mlt_zrcoef);
            }
            acoefLbl.setText(abias);
            acoefLbl.setLayoutData(gd);

            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            Label bcoefLbl = new Label(biasListComp, SWT.CENTER);
            if (abzerocoef.pwr_zrcoef == 0.0) {
                bbias = "N/A";
            } else {
                bbias = String.format("%-1.2f", abzerocoef.pwr_zrcoef);
            }
            bcoefLbl.setText(bbias);
            bcoefLbl.setLayoutData(gd);

            String[] oid = new String[1];
            String office_id = "";
            oid[0] = office_id;
            Float[] obias_value = new Float[1];
            Float other_bias_value = 0.00f;
            obias_value[0] = other_bias_value;
            int bias_found = ReadBiasTableParam.get_rfc_bias_value(rid, oid,
                    obias_value);

            if (bias_found == 0) {
                obias = "N/A";
                ooffice = "N/A";
            } else {
                obias = String.format("%-1.2f", obias_value[0]);
                ooffice = oid[0];
            }
            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            Label obiasLbl = new Label(biasListComp, SWT.CENTER);
            obiasLbl.setText(obias);
            obiasLbl.setLayoutData(gd);

            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            Label offcLbl = new Label(biasListComp, SWT.CENTER);
            offcLbl.setText(ooffice);
            offcLbl.setLayoutData(gd);

            if (biasRowHeight == 0) {
                /*
                 * Calculate the height of a single row.
                 */
                biasRowHeight = lbiasDPTxt.getLineHeight()
                        + lbiasDPTxt.getBorderWidth()
                        + biasListComp.getBorderWidth()
                        + biasListComp.getSize().y;
            }
        }

        biasListScrollComp.setContent(biasListComp);

        /*
         * Minimum number of rows to display in the dialog.
         */
        final int minDisplayedRows = 8;

        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.heightHint = biasRowHeight * minDisplayedRows;
        biasListScrollComp.setLayout(gl);
        biasListScrollComp.setLayoutData(gd);
        biasListScrollComp.setExpandVertical(true);
        biasListScrollComp.setExpandHorizontal(true);
        biasListScrollComp.addControlListener(new ControlAdapter() {
            public void controlResized(ControlEvent e) {
                Rectangle r = biasListScrollComp.getClientArea();
                biasListScrollComp.setMinSize(biasListComp.computeSize(r.width,
                        SWT.DEFAULT));
            }
        });
        /*
         * Set the grid layout on the header row composite to allow for optional
         * additional right margin to accommodate a scroll bar. The margin will
         * ensure that all header labels will line up correctly even if a
         * scrollbar is present.
         */
        if (radIds.length > minDisplayedRows
                && biasListScrollComp.getVerticalBar() != null
                && biasListScrollComp.getVerticalBar().getSize() != null
                && bcLblComp.getLayout() instanceof GridLayout) {
            ((GridLayout) bcLblComp.getLayout()).marginRight = biasListScrollComp
                    .getVerticalBar().getSize().x;
        }
    }

    private void applyBiasUpdate(String obstime) {
        applySPBiasUpdate(obstime);
        applyDPBiasUpdate(obstime);
    }

    private void applySPBiasUpdate(String obstime) {
        String where = "";
        final float memspan = -99.0f;
        List<Rwradarresult> rwRadarResultList = new ArrayList<>();
        Rwradarresult rwRadarResult = new Rwradarresult();
        Iterator<String> bi = spBiasChangeMap.keySet().iterator();
        while (bi.hasNext()) {
            String rid = bi.next();
            where = String.format("WHERE radid='%s' AND obstime='%s'", rid,
                    obstime);
            rwRadarResultList = IHFSDbGenerated.GetRWRadarResult(where);
            if (rwRadarResultList.size() != 0) {
                rwRadarResult = rwRadarResultList.get(0);
            } else {
                continue;
            }
            int indexval = spBiasChangeMap.get(rid);
            if ("YES"
                    .equalsIgnoreCase(spManEditButtonArray[indexval].getText())) {
                rwRadarResult.setEditBias("y");
                rwRadarResult.setMemSpanUsed((double) memspan);
            } else {
                rwRadarResult.setEditBias("n");
            }
            rwRadarResult
                    .setRwBiasValUsed((double) editedSPBiasValue[indexval]);
            IHFSDbGenerated.UpdateRWRadarResult(rwRadarResult);
        }
        spBiasChangeMap.clear();
    }

    // ---------------------------------------------------------------------

    private void applyDPBiasUpdate(String obstime) {
        String where = "";
        final float memspan = -99.0f;
        List<DAARadarResult> daaRadarResultList = new ArrayList<>();
        DAARadarResult daaRadarResult = new DAARadarResult();
        Iterator<String> bi = dpBiasChangeMap.keySet().iterator();
        while (bi.hasNext()) {
            String rid = bi.next();
            where = String.format("WHERE radid='%s' AND obstime='%s'", rid,
                    obstime);
            daaRadarResultList = IHFSDbGenerated.GetDAARadarResult(where);
            if (daaRadarResultList.size() != 0) {
                daaRadarResult = daaRadarResultList.get(0);
            } else {
                continue;
            }
            int indexval = dpBiasChangeMap.get(rid);
            if ("YES"
                    .equalsIgnoreCase(dpManEditButtonArray[indexval].getText())) {
                daaRadarResult.setEditBias("y");
                daaRadarResult.setMemSpanUsed((double) memspan);
            } else {
                daaRadarResult.setEditBias("n");
            }
            daaRadarResult
                    .setRwBiasValUsed((double) editedDPBiasValue[indexval]);
            IHFSDbGenerated.UpdateDAARadarResult(daaRadarResult);
        }
        dpBiasChangeMap.clear();
    }

}
