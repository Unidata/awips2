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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Show Adaptable Parameter Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2009  2675       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DisplayDlg extends CaveSWTDialog {
    private static final String CR = "\n";

    private static final String BAD_SCAN = "NO HOURLY ACCUM BECAUSE RATE SCAN FLAGGED BAD";

    private static final String NOT_ENOUGH_DATA = "NO HOURLY ACCUM BECAUSE NOT ENOUGH DATA IN HOUR";

    private static final String DISK_ERROR = "NO SUPPLEMENTAL DATA AVAILABLE DUE TO DISK ERROR";

    private static final String NO_PRECIP_DETECTED = "NO PRECIP DETECTED IN LAST HOUR";

    private static final String PRECIP_DETECTED = "PRECIP DETECTED IN LAST HOUR";

    private static final String CLEAR_AIR = "clear air";

    private static final String PRECIP = "precip";

    private static final String MAINTENANCE = "maintenance";

    private static final String UNKNOWN = "unknown";

    /** Radar Id */
    private String radId = null;

    /** DPA Date Time Group */
    private Date dtg;

    /** Selection text */
    private String selection;

    private Font font;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            This dialog's parent shell
     * @param radId
     *            The radar id
     * @param dtg
     *            The date time group
     * @param selection
     *            The selection, adaptable parameters or supplemental data
     */
    public DisplayDlg(Shell parentShell, String radId, Date dtg,
            String selection) {
        super(parentShell);
        this.radId = radId;
        this.dtg = dtg;
        this.selection = selection;
        if (selection.equalsIgnoreCase("Display Adaptable Param")) {
            setText("Adaptation Parameter Viewer");
        } else {
            setText("Supplemental Data Viewer");
        }
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, true);
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(true);
        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize all of the controls and layout
        initializeComponents();
    }

    /**
     * Initialize the gui widgets
     */
    private void initializeComponents() {
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        comp.setLayout(gl);
        GridData gd = new GridData(600, SWT.DEFAULT);
        comp.setLayoutData(gd);

        String displayText = getDisplayText();

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 350;
        gd.heightHint = 525;
        StyledText textArea = new StyledText(comp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        textArea.setWordWrap(false);
        textArea.setEditable(false);
        textArea.setText(displayText);
        textArea.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 63;
        Button closeBtn = new Button(comp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get the dialog display text.
     * 
     * @return the text
     */
    private String getDisplayText() {
        String text = null;

        if (selection.equalsIgnoreCase("Display Adaptable Param")) {
            text = getAdaptableParamData();
        } else {
            text = getSupplementalData();
        }

        return text;
    }

    /**
     * Get the adaptable parameter dialog text.
     * 
     * @return the text
     */
    private String getAdaptableParamData() {
        StringBuilder buffer = new StringBuilder();
        RadarDataManager rdm = RadarDataManager.getInstance();

        try {
            DPAAdaptableParam adaptData = rdm.getAdaptableParameters(radId, dtg);

            if (adaptData != null)
            {
                buffer.append(String.format(
                                        "          ***** Radar = %s *****\n"
                                                + "          Preprocessing Algorithm Parameters\n"
                                                + "%.2f = min reflectivity for isolated bin test (dBZ)%s"
                                                + "%.2f = max reflectivity before being labeled as outlier (dBZ)%s"
                                                + "%.2f = min precip echo area for tilt test in low elev (km**2)%s"
                                                + "%.2f = min area-weighted reflectivity for tilt test in low elev (dBZ)%s"
                                                + "%.2f = min reflectivity in tilt test (dBZ)%s%.2f = inner range for tilt test (km)%s"
                                                + "%.2f = outer range for tilt test (km)%s"
                                                + "%.2f = min range of biscan maximization (km)%s%.2f = max range of biscan maximization (km)%s"
                                                + "%.2f = max percent area reduction between 2 lowest elevations (%%)%s%.2f = Z-R multiplicative coefficient%s"
                                                + "%.2f = Z-R power coefficient%s%.2f = min reflectivity to convert to rate (dBZ)%s"
                                                + "%.2f = max reflectivity to convert to rate (dBZ)%s"
                                                + "%s"
                                                + "          Rate Algorithm Parameters\n"
                                                + "%.2f = max storm speed (m/s)%s%.2f = max scan-to-scan time difference for time continuity test (min)%s"
                                                + "%.2f = min precip area for performing time continuity (km**2)%s"
                                                + "%.2f = rate of change of volumetric precip rate, limited area (1/hr)%s"
                                                + "%.2f = rate of change of volumetric precip rate, whole umbrella (1/hr)%s%.2f = max rate of echo area change (km**2/hr)%s"
                                                + "%.2f = range beyond which to apply range-effect correction (km)%s%.2f = range effect coefficient 1 (dBR)%s"
                                                + "%.2f = range effect coefficient 2%s%.2f = range effect coefficient 3%s"
                                                + "%.2f = min precip rate (mm/hr)%s%.2f = max precip rate (mm/hr)%s"
                                                + "%s"
                                                + "          Accumulation Algorithm Parameters\n"
                                                + "%.2f = threshold elapsed time to restart (min)%s%.2f = max time difference between scans for interpolation (min)%s"
                                                + "%.2f = min time needed to accumulate hourly total (min)%s%.2f = threshold for hourly accumulation outlier (mm)%s"
                                                + "%.2f = end time for gage accumulation (min)%s%.2f = max scan-to-scan accumulation (mm)%s"
                                                + "%.2f = maximum hourly accumulation (mm)%s"
                                                + "%s"
                                                + "          Adjustment Algorithm Parameters\n"
                                                + "%.2f = minutes after clock hour when bias is updated (min)%s"
                                                + "%.2f = min number of hourly gage-radar pairs needed to calculate bias%s"
                                                + "%.2f = reset value of gage-radar bias estimate%s"
                                                + "%.2f = longest lag for using table (hrs)%s"
                                                + "%s = bias applied flag",
                                        radId, adaptData.getMin_reflth(), CR,
                                        adaptData.getMax_reflth(), CR,
                                        adaptData.getMin_echoar(), CR,
                                        adaptData.getMin_awrefl(), CR,
                                        adaptData.getRef_tltest(), CR,
                                        adaptData.getRng_tltin(), CR, adaptData
                                                .getRng_tltout(), CR, adaptData
                                                .getMin_birng(), CR, adaptData
                                                .getMax_birng(), CR, adaptData
                                                .getMax_pctred(), CR, adaptData
                                                .getMlt_zrcoef(), CR, adaptData
                                                .getPwr_zrcoef(), CR, adaptData
                                                .getMin_zrefl(), CR, adaptData
                                                .getMax_zrefl(), CR, CR,
                                        adaptData.getMax_stmspd(), CR,
                                        adaptData.getMax_timdif(), CR,
                                        adaptData.getMin_artcon(), CR,
                                        adaptData.getTim_p1cont(), CR,
                                        adaptData.getTim_p2cont(), CR,
                                        adaptData.getMax_ecarch(), CR,
                                        adaptData.getRng_cutoff(), CR,
                                        adaptData.getRng_e1coef(), CR,
                                        adaptData.getRng_e2coef(), CR,
                                        adaptData.getRng_e3coef(), CR,
                                        adaptData.getMin_prate(), CR, adaptData
                                                .getMax_prate(), CR, CR,
                                        adaptData.getTim_restrt(), CR,
                                        adaptData.getMax_timint(), CR,
                                        adaptData.getMin_timprd(), CR,
                                        adaptData.getThr_hlyout(), CR,
                                        adaptData.getEnd_timgag(), CR,
                                        adaptData.getMax_prdval(), CR,
                                        adaptData.getMax_hlyval(), CR, CR,
                                        adaptData.getTim_biest(), CR, adaptData
                                                .getThr_nosets(), CR, adaptData
                                                .getRes_bias(), CR, adaptData
                                                .getLongest_lag(), CR,
                                        adaptData.getBias_applied()));
            } else {
                buffer.append("No Data");
            }
        } catch (VizException e) {
            e.printStackTrace();
            buffer.setLength(0);
            buffer.append("No Data");
        }

        return buffer.toString();
    }

    /**
     * function to display the supplemental data found at the end of each DPA
     * radar product the supplemental data is stored in the DPARadar table
     * 
     * Note: the value of the operational weather mode specified in the header
     * is used
     * 
     * @return the text
     */
    private String getSupplementalData() {
        StringBuilder buffer = new StringBuilder();
        RadarDataManager rdm = RadarDataManager.getInstance();

        try {
            DPASupplementalData supData = rdm.getSupplementalData(radId, dtg);

            if (supData != null) {
                int minOff = supData.getMinoff();
                if (minOff > 30) {
                    minOff -= 60;
                }

                String modeString = CLEAR_AIR;
                int operMode = supData.getOpermode();
                if (operMode == 0) {
                    modeString = MAINTENANCE;
                } else if (operMode == 2) {
                    modeString = PRECIP;
                } else if (operMode == -99) {
                    modeString = UNKNOWN;
                }

                String supMsgStr = NO_PRECIP_DETECTED;
                int supMsg = supData.getSupplmess();
                if (supMsg == 1) {
                    supMsgStr = BAD_SCAN;
                } else if (supMsg == 2) {
                    supMsgStr = NOT_ENOUGH_DATA;
                } else if (supMsg == 3) {
                    supMsgStr = DISK_ERROR;
                } else if (supMsg == 4) {
                    supMsgStr = PRECIP_DETECTED;
                }

                /* create string for display in window */
                buffer
                        .append(String
                                .format(
                                        "          ***** Radar = %s *****\n"
                                                + "%d = minutes off the top of the hour of validtime(min)%s"
                                                + "%.2f = maximum value from data (mm)%s"
                                                + "%.2f = maximum value from header (dBA)%s"
                                                + "%d = number of isolated bins%s"
                                                + "%d = number of interpolated outliers%s"
                                                + "%d = number of outliers replaced%s"
                                                + "%d = number of bad scans%s"
                                                + "%d = number of hourly outliers%s"
                                                + "%d = volume coverage pattern%s"
                                                + "%s = operational weather mode%s"
                                                + "%.2f = bias estimate%s"
                                                + "%.2f = hourly mean bi-scan ratio%s"
                                                + "%.2f = hourly mean percent area reduction (%%)%s"
                                                + "%s = product generation time%s"
                                                + "%s" + "%s", radId, minOff,
                                        CR, supData.getMaxvald(), CR, supData
                                                .getMaxvalh(), CR, supData
                                                .getNisolbin(), CR, supData
                                                .getNoutint(), CR, supData
                                                .getNoutrep(), CR, supData
                                                .getNbadscan(), CR, supData
                                                .getNhourout(), CR, supData
                                                .getVolcovpat(), CR,
                                        modeString, CR, supData
                                                .getS1_bias_value(), CR,
                                        supData.getBiscanr(), CR, supData
                                                .getAreared(), CR,
                                        HydroConstants.DATE_FORMAT
                                                .format(supData
                                                        .getProducttime()), CR,
                                        CR, supMsgStr));
            } else {
                buffer.append("No Data");
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        return buffer.toString();
    }
}
