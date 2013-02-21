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
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
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
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData.RadarAvailability;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.GageTableDataManager;
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
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RadarBiasTableDialog extends Dialog {

    public static class Zerocoef_Data {
        float mlt_zrcoef;

        float pwr_zrcoef;
    }

    private final Zerocoef_Data abzerocoef = new Zerocoef_Data();

    private Shell shell;

    private Font font;

    private Button applyBtn;

    private Button closeBtn;

    Composite bcLblComp;

    private final int retval = 0;

    String[] radIds;

    String rid = "";

    private static String[] biasLabelStrings = { " Radar", "Bias :",
            "Manually Specified", "A", "B", "Bias :", "Other Office" };

    private final Label[] colHdr = new Label[biasLabelStrings.length];

    private String abias = "";

    private String bbias = "";

    private String obias = "";

    private String ooffice = "";

    private String bias = "";

    private String mbias = "";

    public static int default_bias = 0;

    private ArrayList<Rwbiasstat> bList = new ArrayList<Rwbiasstat>();

    public static Rwbiasstat rwBias = new Rwbiasstat();

    private Map<String, MPERadarData> rsList;

    private MPERadarData radarresultdata = new MPERadarData();

    public static String dt = "";

    private static final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyyMMddHH");

    private static final SimpleDateFormat pgsdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private static final SimpleDateFormat st3sdf;

    Button mbiasBtn = null;

    Button[] manEdit = null;

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

    float[] oldbias = new float[60];

    float[] editbias = new float[60];

    public RadarBiasTableDialog(Shell parentShell) {
        super(parentShell);
    }

    /**
     * Open method used to display the Group Edit Stations dialog.
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
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        font.dispose();

        return retval;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {

        try {
            radIds = GageTableDataManager.getInstance().getActiveRadarIds();
        } catch (VizException e) {
            e.printStackTrace();
        }
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

        GridData bd = new GridData(110, SWT.DEFAULT);
        applyBtn = new Button(applyBtnComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(bd);
        applyBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                applyBiasUpdate(dt);
                shell.dispose();
            }
        });

        bd = new GridData(110, SWT.DEFAULT);
        closeBtn = new Button(applyBtnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(bd);
        closeBtn.addSelectionListener(new SelectionAdapter() {

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
        GridLayout bcLblCompLayout = new GridLayout(7, true);
        bcLblCompLayout.marginWidth = 1;
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

        // Create a container to hold the label and the combo box.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        Composite biasListComp = new Composite(bcLblComp, SWT.V_SCROLL);
        GridLayout biasListCompLayout = new GridLayout(7, true);
        biasListComp.setLayout(biasListCompLayout);
        gd.horizontalSpan = 7;
        biasListComp.setLayoutData(gd);
        biasListComp
                .setSize(biasListComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        Date dt3 = MPEDisplayManager.getCurrent().getCurrentEditDate();
        dt = pgsdf.format(dt3);
        rsList = new HashMap<String, MPERadarData>(radIds.length);
        rsList = MPEDataManager.getInstance().readRadarData(dt3);
        Text[] lbTxts = new Text[radIds.length];
        manEdit = new Button[radIds.length];

        for (int i = 0; i < radIds.length; i++) {

            abzerocoef.mlt_zrcoef = 0.0f;
            abzerocoef.pwr_zrcoef = 0.0f;
            rid = radIds[i];

            if (rsList.size() != 0) {
                radarresultdata = rsList.get(rid);
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
                    e1.printStackTrace();
                }
                if (dpaz.length != 0) {
                    abzerocoef.mlt_zrcoef = dpaz[0];
                    abzerocoef.pwr_zrcoef = dpaz[1];
                }
            }

            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
            final Button ridBtn = new Button(biasListComp, SWT.PUSH);
            ridBtn.setText(rid);
            ridBtn.setData(rid);
            ridBtn.setLayoutData(gd);
            ridBtn.addSelectionListener(new SelectionAdapter() {

                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org
                 * .eclipse .swt.events.SelectionEvent)
                 */
                @Override
                public void widgetSelected(SelectionEvent e) {
                    RadarSpanDialog rsd = new RadarSpanDialog(shell,
                            (String) ridBtn.getData());
                    rsd.open();
                }

            });

            bias = String.format("%-1.2f", radarresultdata.getRwBiasValUsed());
            oldbias[i] = (float) radarresultdata.getRwBiasValUsed();
            editbias[i] = 0.0f;

            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            final Text lbiasTxt = new Text(biasListComp, SWT.SINGLE
                    | SWT.CENTER);
            if (radarresultdata.getProductDate() == null) {
                ridBtn.setEnabled(false);
            }
            if (radarresultdata.getEditBias() != null) {
                lbiasTxt.setText(bias.trim());
            } else {
                lbiasTxt.setText("DATA MISSING");
                lbiasTxt.setEnabled(false);
            }

            lbiasTxt.setLayoutData(gd);
            lbiasTxt.setData(i);

            lbiasTxt.addModifyListener(new ModifyListener() {

                @Override
                public void modifyText(ModifyEvent e) {
                    final int ei = (Integer) lbiasTxt.getData();
                    try {
                        float parsedFloat = Float.parseFloat(lbiasTxt.getText());
                        editbias[ei] = parsedFloat;
                        manEdit[ei].setSelection(!mbiasBtn.getSelection());
                        manEdit[ei].setText("YES");
                        lbiasTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_WHITE));
                        applyBtn.setEnabled(true);
                    } catch (NumberFormatException e1) {
                        lbiasTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_RED));
                        applyBtn.setEnabled(false);

                    }
                }
            });

            lbTxts[i] = lbiasTxt;
            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            mbiasBtn = new Button(biasListComp, SWT.TOGGLE | SWT.READ_ONLY);
            // mbiasBtn.setEnabled(false);
            mbias = ("n".equalsIgnoreCase(radarresultdata.getEditBias()) || radarresultdata
                    .getEditBias() == null) ? "NO" : "YES";
            mbiasBtn.setText(mbias);
            mbiasBtn.setLayoutData(gd);
            mbiasBtn.setData(i);
            mbiasBtn.setSelection(false);
            manEdit[i] = mbiasBtn;

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

            String office_id = "";
            float other_bias_value = 0;
            int bias_found = ReadBiasTableParam.get_rfc_bias_value(rid,
                    office_id, other_bias_value);

            if (bias_found == 0) {
                obias = "N/A";
                ooffice = "N/A";
            } else {
                obias = String.format("%-1.2f", other_bias_value);
                ooffice = office_id;
            }
            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            Label obiasLbl = new Label(biasListComp, SWT.CENTER);
            obiasLbl.setText(obias);
            obiasLbl.setLayoutData(gd);

            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            Label offcLbl = new Label(biasListComp, SWT.CENTER);
            offcLbl.setText(ooffice);
            offcLbl.setLayoutData(gd);
        }
    }

    private void applyBiasUpdate(String obstime) {
        String where = "";
        final float memspan = -99.0f;
        ArrayList<Rwradarresult> rwr = new ArrayList<Rwradarresult>();
        Rwradarresult rwrr = new Rwradarresult();
        for (int i = 0; i < radIds.length; i++) {
            if (radIds[i].equals("ZZZ")) {
                continue;
            }
            if (manEdit[i] != null
                    && "YES".equalsIgnoreCase(manEdit[i].getText())) {
                where = String.format("WHERE radid='%s' AND obstime='%s'",
                        radIds[i], obstime);
                rwr = (ArrayList<Rwradarresult>) IHFSDbGenerated
                        .GetRWRadarResult(where);
                if (rwr.size() != 0) {
                    rwrr = rwr.get(0);
                } else {
                    continue;
                }
                rwrr.setEditBias("y");
                rwrr.setMemSpanUsed((double) memspan);
                rwrr.setRwBiasValUsed((double) editbias[i]);
                IHFSDbGenerated.UpdateRWRadarResult(rwrr);
                MPEDataManager.getInstance().setRadarEditFlag(true);
            } else {
                continue;
            }
        }
        return;
    }
}
