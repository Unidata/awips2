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
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Rectangle;
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
import com.raytheon.uf.common.dataplugin.shef.tables.DAARadarResult;
// com.raytheon.uf.common.dataplugin.shef.tables.DAARadarResultId;
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
 * Jun 18, 2013  16053     snaples     Removed reference to setRadarEditFlag
 * Aug 06, 2013  16243                 Changed the Gui to a ScrolledComposite.
 * Feb 2, 2014   16201     snaples     Added saved data flag support
 * Apr 4, 2014   17223     snaples     Updated other_office_id and rfc_bias to object 
 *                                     array so that called procedure can update and return values properly.
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

    private static String[] biasLabelStrings = { " Radar", "SP Bias", "DP Bias",
            "Man. SP Bias", "Man. DP Bias",  "A", "B", "Bias", "Other Office" };

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

    private MPERadarData radarresultdata    = new MPERadarData();
    private MPERadarData daaradarresultdata = new MPERadarData();
    
    public static String dt = "";

    private static final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyyMMddHH");

    private static final SimpleDateFormat pgsdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private static final SimpleDateFormat st3sdf;

    Button mbiasSPBtn = null; //this is used only locally to create the button and then assigned to the array
    Button[] manEditSP = null;

    Button mbiasDPBtn = null; //this is used only locally to create the button and then assigned to the array
    Button[] manEditDP = null;
    
    //used only for code taken from HydroTimeUtility
    private static ThreadLocal<SimpleDateFormat> sqlSdf = new ThreadLocal<SimpleDateFormat>() {

		@Override
		protected SimpleDateFormat initialValue() {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
			return sdf;
		}

	};
    
    static
    {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        pgsdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        String date_form = appsDefaults.getToken("st3_date_form");
        if ((date_form == null) || date_form.isEmpty()
                || date_form.equals("mdY"))
        {
            st3sdf = new SimpleDateFormat("MMM dd yyyy HH");
            st3sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        }
        else
        {
            st3sdf = sdf;
        }
    }

    float[] oldbiasSP = new float[60];
    float[] editbiasSP = new float[60];

    
    float[] oldbiasDP = new float[60];
    float[] editbiasDP = new float[60];
    
    public RadarBiasTableDialog(Shell parentShell) {
        super(parentShell);
    }

    /**
     * Open method used to display the Radar Bias Table dialog == Edit Bias Table Dialog in A1.
     * 
     * @return Null.
     */
    public int open()
    {
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
    private void initializeComponents()
    {

        try {
            radIds = GageTableDataManager.getInstance().getActiveRadarIds();
        } catch (VizException e) {
            e.printStackTrace();
        }
        String fxa_local_site = appsDefaults.getToken("fxa_local_site");
        String where = "WHERE office_id = '" + fxa_local_site + "'";
        bList = IHFSDbGenerated.GetRWBiasstat(where);
        if (!bList.isEmpty())
        {
            rwBias = bList.get(0);
            default_bias = rwBias.getNpairBiasSelect();
        }

        createDateComp();
        createBiasColLblComp();
        createBiasListComp();
        createButtonComp();
    }

    private void createButtonComp()
    {

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
                MPEDisplayManager mgr = MPEDisplayManager.getCurrent();
                mgr.setSavedData(false);
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

    private void createDateComp()
    {

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
    private void createBiasColLblComp()
    {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bcLblComp = new Composite(shell, SWT.NONE);
        GridLayout bcLblCompLayout = new GridLayout(9, true);
        bcLblCompLayout.marginWidth = 1;
        bcLblComp.setLayout(bcLblCompLayout);
        bcLblComp.setLayoutData(gd);
        for (int i = 0; i < biasLabelStrings.length; i++)
        {
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
        GridLayout gl = new GridLayout(1, false);        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.heightHint = 300;
        biasListScrollComp.setLayout(gl);
        biasListScrollComp.setLayoutData(gd);
      // Create a container to hold the label and the combo box.
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        final Composite biasListComp = new Composite(biasListScrollComp,
                SWT.BORDER);
     
        // new table has 9 columns;  previous table had 7 columns
        GridLayout biasListCompLayout = new GridLayout(9, true);
        biasListComp.setLayout(biasListCompLayout);
        gd.horizontalSpan = 9;
        biasListComp.setLayoutData(gd);
        biasListComp
                .setSize(biasListComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        Date dt3 = MPEDisplayManager.getCurrent().getCurrentEditDate();
      
        
        dt = pgsdf.format(dt3);
        
        radarIdToSPDataMap = MPEDataManager.getInstance().readSPRadarData(dt3);
        radarIdToDPDataMap = MPEDataManager.getInstance().readDPRadarData(dt3);
        
        Text[] biasSPTextArray = new Text[radIds.length];
        Text[] biasDPTextArray = new Text[radIds.length];
        
        manEditSP = new Button[radIds.length];
        manEditDP = new Button[radIds.length];

      
        for (int i = 0; i < radIds.length; i++)
        {
        	   	
       	    // get A and B coefficients from SP radar (does not apply to DP )

            abzerocoef.mlt_zrcoef = 0.0f;
            abzerocoef.pwr_zrcoef = 0.0f;
            
            rid = radIds[i];
            
            if (radarIdToDPDataMap.size() != 0)
            {
                daaradarresultdata = radarIdToDPDataMap.get(rid);
            } 

            if (radarIdToSPDataMap.size() != 0)
            {
                radarresultdata = radarIdToSPDataMap.get(rid);
            }
            else 
            {
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
            
            //-----------------------------------------------------
            
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
            
            //radar id button
            final Button ridBtn = new Button(biasListComp, SWT.PUSH);
            ridBtn.setText(rid);
            ridBtn.setData(rid);
            ridBtn.setLayoutData(gd);
            
            ridBtn.addSelectionListener(new SelectionAdapter()
            {

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

            biasSP = String.format("%-1.2f", radarresultdata.getRwBiasValUsed());
            oldbiasSP[i] = (float) radarresultdata.getRwBiasValUsed();
            editbiasSP[i] = 1.0f;

            biasDP = String.format("%-1.2f", daaradarresultdata.getRwBiasValUsed());
            oldbiasDP[i] = (float) daaradarresultdata.getRwBiasValUsed();
            editbiasDP[i] = 1.0f;
            
            
            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
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

            
            final Text lbiasDPTxt = new Text(biasListComp, SWT.SINGLE
                    | SWT.CENTER | SWT.BORDER);
                  
            ridBtn.setEnabled(true);
            
            if (daaradarresultdata.getEditBias() != null) {
            	lbiasDPTxt.setText(biasDP.trim());
            } else {
            	lbiasDPTxt.setText("DATA MISSING");
            	lbiasDPTxt.setEnabled(false);
            }
            lbiasDPTxt.setLayoutData(gd);
            lbiasDPTxt.setData(i);
            
         
            //--------------------------------------------------------------
            lbiasSPTxt.addModifyListener(new ModifyListener()
            {

                @Override
                public void modifyText(ModifyEvent e) {
                    final int ei = (Integer) lbiasSPTxt.getData();
                    try {
                        float parsedFloat = Float.parseFloat(lbiasSPTxt.getText());
                        editbiasSP[ei] = parsedFloat;
                        manEditSP[ei].setSelection(!mbiasSPBtn.getSelection());
                        manEditSP[ei].setText("YES");
                        lbiasSPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_WHITE));
                        applyBtn.setEnabled(true);
                    } catch (NumberFormatException e1) {
                        lbiasSPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_RED));
                        applyBtn.setEnabled(false);

                    }
                }
            });
            //-------------------------------------------------------------
            
            lbiasDPTxt.addModifyListener(new ModifyListener()
            {

                @Override
                public void modifyText(ModifyEvent e) {
                    final int ei = (Integer) lbiasDPTxt.getData();
                    try {
                        float parsedFloat = Float.parseFloat(lbiasDPTxt.getText());
                        editbiasDP[ei] = parsedFloat;
                        manEditDP[ei].setSelection(!mbiasDPBtn.getSelection());
                        manEditDP[ei].setText("YES");
                        lbiasDPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_WHITE));
                        applyBtn.setEnabled(true);
                    } catch (NumberFormatException e1) {
                    	lbiasDPTxt.setBackground(getParent().getDisplay()
                                .getSystemColor(SWT.COLOR_RED));
                        applyBtn.setEnabled(false);

                    }
                }
            });
            //-------------------------------------------------------------
            gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
            
            
            biasSPTextArray[i] = lbiasSPTxt;
            mbiasSPBtn = new Button(biasListComp, SWT.TOGGLE | SWT.READ_ONLY);
            mbiasSP = ("n".equalsIgnoreCase(radarresultdata.getEditBias()) || radarresultdata
                    .getEditBias() == null) ? "NO" : "YES";
            mbiasSPBtn.setText(mbiasSP);
            mbiasSPBtn.setLayoutData(gd);
            mbiasSPBtn.setData(i);
            mbiasSPBtn.setSelection(false);
            manEditSP[i] = mbiasSPBtn;

            //------------------------------------------------------
            
            biasDPTextArray[i] = lbiasDPTxt;
            mbiasDPBtn = new Button(biasListComp, SWT.TOGGLE | SWT.READ_ONLY);
            mbiasDP = ("n".equalsIgnoreCase(daaradarresultdata.getEditBias()) || daaradarresultdata
                    .getEditBias() == null) ? "NO" : "YES";
            mbiasDPBtn.setText(mbiasDP);
            mbiasDPBtn.setLayoutData(gd);
            mbiasDPBtn.setData(i);
            mbiasDPBtn.setSelection(false);
            manEditDP[i] = mbiasDPBtn;

            //---------------------------------------------------------
            
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
            int bias_found = ReadBiasTableParam.get_rfc_bias_value(rid,
                    oid, obias_value);

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
        }
        biasListScrollComp.setContent(biasListComp);
        biasListScrollComp.setExpandVertical(true);
        biasListScrollComp.setExpandHorizontal(true);
        biasListScrollComp.addControlListener(new ControlAdapter() {
            public void controlResized(ControlEvent e) {
                Rectangle r = biasListScrollComp.getClientArea();
                biasListScrollComp.setMinSize(biasListComp.computeSize(r.width,
                        SWT.DEFAULT));
            }
        });
    }
    
    private void  applyBiasUpdate(String obstime)
    {
    	applySPBiasUpdate(obstime);
    	applyDPBiasUpdate(obstime);
    }

    private void applySPBiasUpdate(String obstime)
    {
        String where = "";
        final float memspan = -99.0f;
        ArrayList<Rwradarresult> rwr = new ArrayList<Rwradarresult>();
        Rwradarresult rwrr = new Rwradarresult();
        for (int i = 0; i < radIds.length; i++) {
            if (radIds[i].equals("ZZZ")) {
                continue;
            }
            if (manEditSP[i] != null
                    && "YES".equalsIgnoreCase(manEditSP[i].getText())) {
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
                rwrr.setRwBiasValUsed((double) editbiasSP[i]);
                IHFSDbGenerated.UpdateRWRadarResult(rwrr);
            } else {
                continue;
            }
        }
        return;
    }
    
	// ---------------------------------------------------------------------

    
    private void applyDPBiasUpdate(String obstime)
    {
        String where = "";
        final float memspan = -99.0f;
        ArrayList<DAARadarResult> rwr = new ArrayList<DAARadarResult>();
        DAARadarResult rwrr = new DAARadarResult();
        for (int i = 0; i < radIds.length; i++) {
            if (radIds[i].equals("ZZZ")) {
                continue;
            }
            if (manEditDP[i] != null
                    && "YES".equalsIgnoreCase(manEditDP[i].getText()))
            {
                where = String.format("WHERE radid='%s' AND obstime='%s'",
                        radIds[i], obstime);
                rwr = (ArrayList<DAARadarResult>) IHFSDbGenerated
                        .GetDAARadarResult(where);
                if (rwr.size() != 0) {
                    rwrr = rwr.get(0);
                } else {
                    continue;
                }
                rwrr.setEditBias("y");
                rwrr.setMemSpanUsed((double) memspan);
                rwrr.setRwBiasValUsed((double) editbiasDP[i]);
                IHFSDbGenerated.UpdateDAARadarResult(rwrr);
            }
            else
            {
                continue;
            }
        }
        return;
    }
    

    
}
