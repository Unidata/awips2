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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.GetClimateSource;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.util.BadValues;
import com.raytheon.viz.mpe.util.CheckConsistency;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Rain;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Ts;
import com.raytheon.viz.mpe.util.EstDailyStations;
import com.raytheon.viz.mpe.util.EstPartStations;
import com.raytheon.viz.mpe.util.QCStations;
import org.locationtech.jts.geom.Coordinate;

/**
 * This is a popup dialog box to edit Precipitation Station values and quality
 * codes
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#      Engineer  Description
 * ------------- ------------ --------- ----------------------------------------
 * Apr 13, 2009               snaples   Initial creation
 * Jun 27, 2013  15859        wkwock    Update this dialog after click Apply
 *                                      button
 * Nov 26, 2014  16889        snaples   Updated to fix SNOTEL display
 * Jun 18, 2015  14298,17388  ptilles   Updated to fix problem with
 *                                      mpe_dqc_6hr_24hr_ste_bad token and
 *                                      problem with changing a 6hr value in
 *                                      24hr mode
 * Sep 11, 2015  17986        snaples   Updated q45bnames array to correct order
 *                                      issue, with Screened and Questionable
 *                                      being reversed.
 * Dec 07, 2015  5171         bkowal    Allow the user to change point quality
 *                                      to verified when the 24-hour value is
 *                                      partial.
 * Dec 10, 2015  18391        snaples   Updated changeCustomFile to not remove
 *                                      grid when EditStations Apply is clicked.
 * Oct 27, 2016  5969         randerso  Add support for locating hydroapps on
 *                                      the correct monitor
 * Mar 01, 2017  6159         dgilling  Fix ArrayOutofBoundsException in
 *                                      getQualityTextFromCode.
 * Jul 13, 2017  6148         bkowal    Correctly size the precipitation value textbox.
 * Jul 24, 2017  6148         bkowal    Correctly handle 'M' values in the consistency text fields.
 * Nov 16, 2017  6525         bkowal    Ensure the display is refreshed after "Apply" is clicked.
 * Dec 15, 2017  6547         bkowal    Correctly recall the previous location of the dialog on the screen.
 * Jan 23, 2018  6547         bkowal    Do not estimate values for partial days after "Apply" is clicked.
 *                                      Immediately refresh display after changes are Applied.
 * Feb 21, 2018  7225         bkowal    The "Partial" quality should only be allowed to transition to:
 *                                      { "Screened", "Bad" }.                                     
 * Mar 05, 2018 7232          bkowal    Eliminated methods checking for/handling partial days. The input data
 *                                      now correctly reflects whether it is for a partial day or not.
 * May 10, 2017  7131         mduff     Changed parent class and other cleanup.
 * Aug  6, 2018  7098         tgurney   Save dialog position when closing the dialog
 * Aug 08, 2018  7388         smanoj    Editing Missing value in one location
 *                                      should not change values in other locations
 * Feb 06, 2019  7131         tgurney   Fix point filtering
 * Feb 06, 2019  7131         tgurney   Add user-friendly message if no stations found
 * Feb 06, 2019  7131         tgurney   Fix point filtering
 * Feb 06, 2019  7131         tgurney   Add user-friendly message if no stations found
 * </pre>
 *
 * @author snaples
 */

public class EditPrecipStationsDialog extends AbstractEditStationsDialog
        implements StationFilter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditPrecipStationsDialog.class);

    /** Index value. */
    private static final int HOURS_24 = 4;

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private Font font;

    private int reset_value = 0;

    private int new_qual = 0;

    private String[] precipValueStringArray = new String[5];

    private Text[] precipValueTextArray = new Text[5];

    private Label[] precipValueLabelArray = new Label[5];

    private Label[] qualityCodeStatusLabelArray = new Label[5]; // chip

    private int time_pos = 0;

    private int pcpn_time_step = MPEDisplayManager.pcpn_time_step;

    private int pcpn_time = DailyQcUtils.pcpn_time;

    private Button snotelButton;

    public Button[] qualityCodeButtonArray;

    public Button[] locationButtonArray;

    public String editVal;

    private boolean snow = false;

    private String[] locationButtonNameArray = { "upper left", "upper right",
            "lower left", "lower right" };

    private TextPositionMgr _textPosMgr = new TextPositionMgr();

    private String[] qualityCodeNameArray = { "Screened (Forced)", "Bad",
            "Manual", "Questionable", "Partial", "Estimated",
            "Time Distributed", "Unknown", "Verified" };

    // prefix "F_" indicates that these quality codes are for FRAIN values.
    private static final int F_SCREENED = 0;

    private static final int F_BAD = 1;

    private static final int F_MANUAL = 2;

    private static final int F_QUESTIONABLE = 3;

    private static final int F_PARTIAL = 4;

    private static final int F_ESTIMATED = 5;

    @SuppressWarnings("unused")
    private static final int F_TIME_DISTRIBUTED = 6;

    @SuppressWarnings("unused")
    private static final int F_UNKNOWN = 7;

    private static final int F_VERIFIED = 8;

    /*
     * = 1 -- Forced good by user = 2 -- Lumped = 3 -- Questionable = 4 --
     * Forced good by user = 5 -- Estimated = 6 -- T = 8 -- Screened/Verified
     */

    private String[] q2bnames = { "Manual", "Reset to Original" };

    private String[] q45bnames = { "Verified", "Questionable",
            "Screened (Forced)", "Bad" };

    private int initial_qual = F_MANUAL;

    private int isave = -1;

    private int dcmode = OtherPrecipOptions.dcmode;

    private int tcmode = OtherPrecipOptions.tcmode;

    private Ts ts[] = DailyQcUtils.ts;

    private int tsmax = DailyQcUtils.tsmax;

    private int isom = DailyQcUtils.isom;

    private int gage_char[] = DailyQcUtils.gage_char;

    private int method = dqc.method;

    private int qflag[] = DailyQcUtils.qflag;

    private Pdata pdata[] = DailyQcUtils.pdata;

    private int dflag[] = DailyQcUtils.dflag;

    private List<Station> precipStationList = DailyQcUtils.precip_stations;

    private int max_stations = DailyQcUtils.precip_stations.size();

    private int[] allowedQualityCodes = dqc.func;

    private int pcpn_day = DailyQcUtils.pcpn_day;

    private int mpe_dqc_6hr_24hr_flag = 1;

    private Coordinate coord = new Coordinate();

    private boolean mpe_dqc_warningpopup_flag = false;

    private static Point savedDialogLocation = null;

    public EditPrecipStationsDialog(Shell parentShell,
            ReferencedCoordinate rcoord) {
        super(parentShell.getDisplay(), SWT.DIALOG_TRIM | SWT.MIN,
                CAVE.DO_NOT_BLOCK);
        setText("Edit Precipitation Stations");

        font = new Font(parentShell.getDisplay(), "Courier", 10, SWT.NORMAL);

        if (rcoord != null) {
            try {
                coord = rcoord.asLatLon();
            } catch (TransformException | FactoryException e) {
                statusHandler
                        .error("Error converting ReferenceCoordinate to a Coordinate ["
                                + rcoord + "].", e);
            }
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

    @Override
    protected void preOpened() {
        super.preOpened();
        if (EditPrecipStationsDialog.savedDialogLocation != null) {
            this.getShell().setLocation(savedDialogLocation);
        }
    }

    @Override
    public boolean shouldClose() {
        EditPrecipStationsDialog.savedDialogLocation = shell.getLocation();
        return super.shouldClose();
    }

    @Override
    protected void disposed() {
        super.disposed();
        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        ClosestStationFinder finder = new ClosestStationFinder(this);

        isave = finder.findClosestStation(precipStationList, coord);
        if (isave == -1) {
            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Composite labelComp = new Composite(shell, SWT.NONE);
            GridLayout dataCompLayout = new GridLayout(1, false);
            labelComp.setLayout(dataCompLayout);
            labelComp.setLayoutData(gd);
            new Label(labelComp, SWT.NONE).setText("No stations found.");
            return;
        }

        Rain frain = pdata[pcpn_day].stn[isave].frain[time_pos];

        reset_value = 0;
        initial_qual = frain.qual;
        new_qual = initial_qual;

        Rain srain = pdata[pcpn_day].stn[isave].srain[time_pos];

        if (srain.data > -98) {
            if (time_pos == HOURS_24 && srain.data >= 0) {
                snow = true;
            }
        }

        // Updated to allow editing of time distributed station as in OB 9.x
        // if (initial_qual == 6) {
        //
        // MessageDialog.openError(shell, "Error Time Distributed Station",
        // "You cannot quality control a time distributed station");
        // return;
        // }

        mpe_dqc_warningpopup_flag = dqc.mpe_dqc_warningpopup_flag;

        createPstationDataComp();
        createStnQualComp();
        createStnLocComp();
        createStnConComp();
        createButtonComp();
    }

    // --------------------------------------------------------
    private String loadPrecipStationText(Station selectedStation) {
        Rain frain = pdata[pcpn_day].stn[isave].frain[time_pos];
        StringBuilder buffer = new StringBuilder();
        buffer.append(selectedStation.hb5).append(" ")
                .append(selectedStation.parm).append("\n");
        buffer.append(selectedStation.name).append("\n");
        buffer.append(String.format("%d ft    ", selectedStation.elev));

        if (selectedStation.tip == 0) {
            buffer.append("tipping");
        } else {
            buffer.append("weighing");
        }

        buffer.append("\n");
        if (method == 2 && selectedStation.isoh[isom] > 0) {
            GetClimateSource gc = new GetClimateSource();
            String pClimateSource = gc.getClimateSource(selectedStation.cparm);

            buffer.append(String.format("monthly normal %5.2f in. source: %s\n",
                    selectedStation.isoh[isom], pClimateSource));
        }
        if (frain.data >= 0) {
            buffer.append(String.format("estimate %5.2f in. dev %5.2f\n",
                    frain.estimate, frain.stddev));
        }

        int frzlvl = pdata[pcpn_day].stn[isave].frzlvl[time_pos];
        if (selectedStation.tip == 0 && time_pos != HOURS_24 && frzlvl > -99) {
            buffer.append(String.format("Freezing level %dft\n", frzlvl));
        }

        short snoflag = pdata[pcpn_day].stn[isave].snoflag[time_pos];

        if (snoflag > 0) {
            buffer.append("SNOTEL error is ");

            if (snoflag == 1) {
                buffer.append(" SNOTEL >> PCPN\n");
            }

            else if (snoflag == 2) {
                buffer.append(" CONTINUATION\n");
            }

            else if (snoflag == 3) {
                buffer.append(" PCPN RESET\n");
            }

            else if (snoflag == 4) {
                buffer.append(" PCPN >> SNOTEL\n");
            }

        }

        Rain srain = pdata[pcpn_day].stn[isave].srain[time_pos];

        if (srain.data > -98) {

            buffer.append(String.format("Snow water change is %5.2f in.",
                    srain.data));
            if (time_pos == HOURS_24 && srain.data >= 0) {
                snow = true;
            }

        }

        // only display in 24-hour mode
        if ((time_pos == HOURS_24) && (dqc.QPEaccum24hr != null)) {

            double accumulatedAmount = get24HourPrecipTotal(dqc.QPEaccum24hr,
                    selectedStation.hrap_x
                            - DailyQcUtils.getHrap_grid().hrap_minx,
                    selectedStation.hrap_y
                            - DailyQcUtils.getHrap_grid().hrap_miny);

            buffer.append(String.format("accumulated amount %5.2f in.",
                    accumulatedAmount));
        }

        return buffer.toString();
    }

    // --------------------------------------------------------

    private double get24HourPrecipTotal(double[][][] qpeAccum24hr, float hrap_x,
            float hrap_y) {
        double value = -9.0;
        int dayIndex = pcpn_day;

        if (qpeAccum24hr != null) {
            int x = (int) Math.floor(hrap_x);
            int y = (int) Math.floor(hrap_y);

            try {
                value = qpeAccum24hr[dayIndex][y][x];
            } catch (Throwable t) {
                statusHandler.error("Failed to get data from qpeAccum24hr["
                        + dayIndex + "][" + y + "][" + x + "]", t);
            }
        }

        return value;
    }

    // --------------------------------------------------------
    @Override
    public boolean shouldFilterOut(int stationIndex) {
        boolean filteredOut = false;
        Station station = precipStationList.get(stationIndex);

        Rain frain = pdata[pcpn_day].stn[stationIndex].frain[time_pos];

        if ((frain.data > dqc.getPrecipReverseFilterValue())
                && (frain.data < 20.00)) {
            return true;
        }

        if (station.elev > 0
                && station.elev < dqc.getPointElevationFilterValue()) {
            return true;
        }

        if (frain.data < 0) {
            return true;
        }

        if (frain.data < dqc.getPrecipFilterValue()) {
            return true;
        }

        if (tcmode == 0 && pdata[pcpn_day].stn[stationIndex].tcons == -1) {
            return true;
        }

        if (tcmode == 1 && pdata[pcpn_day].stn[stationIndex].tcons == 1) {
            return true;
        }

        if (dcmode == 0
                && pdata[pcpn_day].stn[stationIndex].scons[time_pos] == -1) {
            return true;
        }

        if (dcmode == 1
                && pdata[pcpn_day].stn[stationIndex].scons[time_pos] == 1) {
            return true;
        }

        if (station.tip == 0 && gage_char[0] == -1) {
            return true;
        }

        if (station.tip == 1 && gage_char[1] == -1) {
            return true;
        }

        int m = 0;

        for (m = 0; m < tsmax; m++) {
            String kd = station.parm.substring(3, 5);
            if ((kd.compareTo(ts[m].abr) == 0 && dflag[m + 1] == 1)) {
                break;
            }
        }

        if (m == tsmax) {
            return true;
        }

        for (m = 0; m < 9; m++) {
            if (m == frain.qual && qflag[m] == 1) {
                break;
            }
        }

        if (m == 9) {
            return true;
        }

        // if made it this far, then the station is not filtered out

        return filteredOut;

    }

    // -----------------------------------------------------------------

    /**
     * Create the data options group and controls.
     */
    private void createPstationDataComp() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite dataComp = new Composite(shell, SWT.NONE);
        GridLayout dataCompLayout = new GridLayout(1, false);
        dataComp.setLayout(dataCompLayout);
        dataComp.setLayoutData(gd);

        Station selectedStation = precipStationList.get(isave);
        String text = loadPrecipStationText(selectedStation);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);

        Label hb5Lbl = new Label(dataComp, SWT.LEFT);
        hb5Lbl.setText(text);
        hb5Lbl.setLayoutData(gd);

        if (snow) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            snotelButton = new Button(dataComp, SWT.CHECK);
            snotelButton.setText("Use SWD for PPD");
            snotelButton.setLayoutData(gd);

            Rain srain = pdata[pcpn_day].stn[isave].srain[time_pos];
            short sflag = pdata[pcpn_day].stn[isave].sflag[time_pos];

            if (srain.data > -98) {
                if (time_pos == HOURS_24 && srain.data >= 0) {
                    if (sflag == 1) {
                        snotelButton.setSelection(true);
                    } else {
                        snotelButton.setSelection(false);
                    }
                }
            }
        }
    }

    /**
     * Create the data options group and controls.
     */
    private void createStnQualComp() {

        Group stnQualGroup = new Group(shell, SWT.NONE);
        stnQualGroup.setText(" Station quality ");
        GridLayout gl = new GridLayout(1, false);
        stnQualGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stnQualGroup.setLayoutData(gd);

        // Create a container to hold the label and the combo box.
        // GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite stnQualComp = new Composite(stnQualGroup, SWT.NONE);
        GridLayout stnQualCompLayout = new GridLayout(2, true);
        stnQualCompLayout.marginWidth = 0;
        stnQualCompLayout.marginHeight = 0;
        stnQualComp.setLayout(stnQualCompLayout);
        stnQualComp.setLayoutData(gd);
        int naflag = 0;
        if (initial_qual < 0
                || pdata[pcpn_day].stn[isave].frain[time_pos].data < 0) {
            naflag = 1;
        }

        if (initial_qual == F_MANUAL) {
            qualityCodeButtonArray = new Button[2];
            for (int i = 0; i < qualityCodeButtonArray.length; i++) {
                final Button b = new Button(stnQualComp, SWT.RADIO);
                b.setText(q2bnames[i]);
                b.setData(i);
                b.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        resetStationQuality((Integer) b.getData());
                    }
                });
                qualityCodeButtonArray[i] = b;
            }
            qualityCodeButtonArray[0].setSelection(true);

        } else if (initial_qual != F_ESTIMATED) {

            // these buttons don't have actions associated with them.
            // they are used when the Apply button is pressed.

            qualityCodeButtonArray = new Button[4];
            List<Integer> allowableChangeList = getAllowableChangeList(
                    initial_qual);

            for (int i = 0; i < qualityCodeButtonArray.length; i++) {
                final Button b = new Button(stnQualComp, SWT.RADIO);
                b.setText(q45bnames[i]);
                b.setData(i);

                if (allowedQualityCodes[i] == initial_qual && naflag != 1) {
                    b.setSelection(true);
                } else {
                    b.setSelection(false);
                }

                determineAndSetEnabledQualityControlButton(b,
                        allowableChangeList, naflag, q45bnames[i]);

                b.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        changeStationQuality((Integer) b.getData());
                    }
                });
                qualityCodeButtonArray[i] = b;
            }
        }

    }

    // ----------------------------------------------------
    // Chip added start
    private void determineAndSetEnabledQualityControlButton(Button button,
            List<Integer> allowableChangeList, int naflag,
            String buttonQualityCodeName) {
        boolean shouldBeEnabled = false;

        if (naflag == 1) {
            shouldBeEnabled = false;
        } else {
            int buttonQualityCode = getQualityCodeByName(buttonQualityCodeName);

            if (allowableChangeList.contains(buttonQualityCode)) {
                shouldBeEnabled = true;
            }
        }

        button.setEnabled(shouldBeEnabled);

        return;
    }

    // ----------------------------------------------------

    private List<Integer> getAllowableChangeList(int initialQualityCode) {
        // String header =
        // "EditPrecipStationsDialog.getAllowableChangeList(): ";
        Integer[] returnedArray = null;

        // allowable states to change to (and back to)
        final Integer[] verifiedArray = { F_VERIFIED, F_BAD };
        final Integer[] questionableArray = { F_QUESTIONABLE, F_BAD,
                F_SCREENED };
        final Integer[] screenedArray = { F_SCREENED, F_BAD };
        final Integer[] badArray = { F_BAD, F_VERIFIED, F_SCREENED,
                F_QUESTIONABLE };
        final Integer[] partialArray = { F_SCREENED, F_BAD };
        final Integer[] emptyArray = {};

        // determine which array applies
        switch (initialQualityCode) {
        case F_VERIFIED:
            returnedArray = verifiedArray;
            break;
        case F_QUESTIONABLE:
            returnedArray = questionableArray;
            break;
        case F_SCREENED:
            returnedArray = screenedArray;
            break;
        case F_BAD:
            returnedArray = badArray;
            break;
        case F_PARTIAL:
            returnedArray = partialArray;
            break;
        default:

            returnedArray = emptyArray;
        }

        // convert to a collection
        List<Integer> returnedList = Arrays.asList(returnedArray);

        return returnedList;
    }

    // ----------------------------------------------------

    private int getQualityCodeByName(String qualityCodeName) {
        String name = null;
        int index = -1;

        for (int i = 0; i < qualityCodeNameArray.length; i++) {
            name = qualityCodeNameArray[i];
            if (name.equalsIgnoreCase(qualityCodeName)) {
                index = i;
                break;
            }
        }

        return index;
    } // getQualityCodeByName()

    // ----------------------------------------------------

    /**
     * Create the data options group and controls.
     */
    private void createStnLocComp() {

        Station selectedStation = precipStationList.get(isave);
        int initial_pos = _textPosMgr.getTextPosition(selectedStation);

        Group stnLocGroup = new Group(shell, SWT.NONE);
        stnLocGroup.setText(" Station Location ");
        GridLayout gl = new GridLayout(1, false);
        stnLocGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stnLocGroup.setLayoutData(gd);

        // Create a container to hold the label and the combo box.
        // GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite stnLocComp = new Composite(stnLocGroup, SWT.NONE);
        GridLayout stnLocCompLayout = new GridLayout(2, true);
        stnLocCompLayout.marginWidth = 0;
        stnLocCompLayout.marginHeight = 0;
        stnLocComp.setLayout(stnLocCompLayout);
        stnLocComp.setLayoutData(gd);

        locationButtonArray = new Button[4];
        for (int i = 0; i < locationButtonArray.length; i++) {
            final Button b = new Button(stnLocComp, SWT.RADIO);
            b.setText(locationButtonNameArray[i]);
            b.setData(i);
            if (i == initial_pos) {
                b.setSelection(true);
            } else {
                b.setSelection(false);
            }
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Station station = precipStationList.get(isave);
                    int textPositionCode = (Integer) b.getData();
                    _textPosMgr.changeStationLocation(textPositionCode,
                            station);
                }
            });
            locationButtonArray[i] = b;
        }

    }

    /**
     * Create the station consistency group and controls.
     */
    private void createStnConComp() {

        Group stnConGroup = new Group(shell, SWT.NONE);
        stnConGroup.setText(" Station Consistency ");
        GridLayout gl = new GridLayout(1, false);
        stnConGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stnConGroup.setLayoutData(gd);

        Composite stnConComp = new Composite(stnConGroup, SWT.NONE);
        GridLayout stnConCompLayout = new GridLayout(3, false);
        stnConCompLayout.marginWidth = 5;
        stnConCompLayout.marginHeight = 3;
        stnConComp.setLayout(stnConCompLayout);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        stnConComp.setLayoutData(gd);

        GC gc = new GC(stnConComp);
        final int minWidth = gc.textExtent("99999.99").x;
        gc.dispose();

        for (int i = 0; i < 5; i++) {
            String valueString;
            precipValueLabelArray[i] = new Label(stnConComp, SWT.LEFT);
            precipValueLabelArray[i].setText(DailyQcUtils.TIME_FILE[2][i]);
            precipValueTextArray[i] = new Text(stnConComp,
                    SWT.LEFT | SWT.BORDER | SWT.READ_ONLY);
            gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            gd.minimumWidth = minWidth;
            precipValueTextArray[i].setLayoutData(gd);

            qualityCodeStatusLabelArray[i] = new Label(stnConComp, SWT.CENTER);

            int qualityCode = pdata[pcpn_day].stn[isave].frain[i].qual;
            String qualityText = getQualityTextFromCode(qualityCode);

            qualityCodeStatusLabelArray[i].setText(qualityText);

            Rain frainI = pdata[pcpn_day].stn[isave].frain[i];
            precipValueTextArray[i].setEditable(true);

            if (frainI.data < 0) {
                valueString = "M";
                precipValueTextArray[i].setText(valueString);
            } else {
                valueString = String.format("%5.2f", frainI.data);
                precipValueTextArray[i].setText(valueString.trim());
            }

            precipValueStringArray[i] = precipValueTextArray[i].getText();

            final int ii = i;

            precipValueTextArray[i].addModifyListener(new ModifyListener() {
                @Override
                public void modifyText(ModifyEvent e) {
                    precipValueStringArray[ii] = precipValueTextArray[ii]
                            .getText();
                    editVal = precipValueTextArray[ii].getText();
                }
            });
        }
    }

    private String getQualityTextFromCode(int qualityCode) {
        if ((qualityCode >= 0) && (qualityCode < qualityCodeNameArray.length)) {
            return qualityCodeNameArray[qualityCode];
        } else {
            return StringUtils.EMPTY;
        }

    }

    private void createButtonComp() {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnGpComp = new Composite(shell, SWT.NONE);
        GridLayout btnGpCompLayout = new GridLayout(3, false);
        btnGpComp.setLayout(btnGpCompLayout);
        btnGpComp.setLayoutData(gd);

        GridData bd = new GridData(110, 25);
        Button applyBtn = new Button(btnGpComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(bd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleApplyActions();
            }
        });

        Button closeBtn = new Button(btnGpComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(bd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });
        Button graphBtn = new Button(btnGpComp, SWT.PUSH);
        graphBtn.setText("Graph");
        graphBtn.setLayoutData(bd);
        graphBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                AppLauncherHandler alh = new AppLauncherHandler();
                String lid = precipStationList.get(isave).hb5;
                String dataType = precipStationList.get(isave).parm;
                final String TSL_BUNDLE_LOC = "bundles/run-TimeSeriesLite.xml";
                try {
                    statusHandler
                            .info("Launching TSL " + lid + ", " + dataType);
                    alh.execute(getShell(), TSL_BUNDLE_LOC, lid, dataType);
                } catch (ExecutionException ee) {
                    statusHandler.error("Failed to launch TSL " + lid + ", "
                            + dataType + ".", ee);
                }
            }
        });
    }

    private void handleApplyActions() {
        // check values for consistency
        checkTextfieldConsistency();
    }

    protected void resetStationQuality(Integer data) {
        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        if (data == 1) {

            for (int k = 0; k < 5; k++) {
                pdata[pcpn_day].stn[isave].frain[k].qual = pdata[pcpn_day].stn[isave].rrain[k].qual;
                pdata[pcpn_day].stn[isave].frain[k].data = pdata[pcpn_day].stn[isave].rrain[k].data;
            }

            reset_value = 1;
            new_qual = pdata[pcpn_day].stn[isave].rrain[time_pos].qual;

        } else {
            reset_value = 0;
        }

    }

    protected void changeStationQuality(Integer data) {
        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        new_qual = allowedQualityCodes[data];
        pdata[pcpn_day].stn[isave].frain[time_pos].qual = (short) new_qual;
    }

    protected void changeCustomFile() {
        String pathName = getStationListPath(DailyQcUtils.currentQcArea);
        String station_list_custom_file = pathName + "_label_position";
        int time_pos;
        float val, fdif;
        String cstr;
        Boolean bval = false;
        float rtotal;
        int m;
        Button rpbutton = QcPrecipOptionsDialog.renderGridsBtn;
        int tcmode = OtherPrecipOptions.tcmode;
        int pcp_flag = DailyQcUtils.pcp_flag;
        int grids_flag = DailyQcUtils.grids_flag;
        int points_flag = DailyQcUtils.points_flag;
        int map_flag = DailyQcUtils.map_flag;
        File custom = new File(station_list_custom_file);
        custom.setReadable(true, false);
        custom.setWritable(true, false);

        // token name: mpe_dqc_6hr_24hr_set_bad
        // token value = OFF
        // mpe_dqc_6hr_24hr_flag = 0
        // if user sets 6hr value to Bad, then 24hr value is unaffected

        // token value = ON
        // mpe_dqc_6hr_24hr_flag = 1
        // if user sets 6hr value to Bad, then 24hr value is set to Bad

        String mpe_dqc_6hr_24hr_string = AppsDefaults.getInstance()
                .getToken("mpe_dqc_6hr_24hr_set_bad", "ON");

        if ("OFF".equalsIgnoreCase(mpe_dqc_6hr_24hr_string)) {
            mpe_dqc_6hr_24hr_flag = 0;
        }

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        try (BufferedWriter out = new BufferedWriter(new FileWriter(custom))) {
            for (int i = 0; i < max_stations; i++) {
                Station station = precipStationList.get(i);
                String rec = String.format("%s %s %d %d\n", station.hb5,
                        station.parm, station.xadd, station.yadd);
                out.write(rec);
            }
        } catch (IOException e) {
            statusHandler.error(
                    "Could not open file " + station_list_custom_file, e);
            return;
        }

        if (snow) {
            bval = snotelButton.getSelection();
        }

        /* snotel path */

        if (snow && ((bval && pdata[pcpn_day].stn[isave].sflag[HOURS_24] == -1)
                || (!bval
                        && pdata[pcpn_day].stn[isave].sflag[HOURS_24] == 1))) {

            pdata[pcpn_day].stn[isave].sflag[HOURS_24] = (short) -pdata[pcpn_day].stn[isave].sflag[HOURS_24];

            if (pdata[pcpn_day].stn[isave].sflag[HOURS_24] == 1) {

                pdata[pcpn_day].stn[isave].frain[HOURS_24].data = pdata[pcpn_day].stn[isave].srain[HOURS_24].data;
                pdata[pcpn_day].stn[isave].frain[HOURS_24].qual = F_VERIFIED;
            }

            else {
                pdata[pcpn_day].stn[isave].frain[HOURS_24].data = pdata[pcpn_day].stn[isave].rrain[HOURS_24].data;
                pdata[pcpn_day].stn[isave].frain[HOURS_24].qual = F_VERIFIED;
            }

        }

        else {
            boolean value_edit_flag = false;
            val = pdata[pcpn_day].stn[isave].frain[time_pos].data;
            int p = -1;

            for (int k = 0; k < 5; k++) {
                cstr = precipValueStringArray[k];
                p = cstr.indexOf('M');
                if (p == -1) {
                    val = Float.parseFloat(cstr);
                }
                cstr = null;

                /* use manually entered data */
                // Changed for DR 17388
                fdif = Math.abs(val - pdata[pcpn_day].stn[isave].frain[k].data);

                if (fdif > .005 && p == -1 && reset_value == 0) {
                    pdata[pcpn_day].stn[isave].frain[k].data = val;
                    pdata[pcpn_day].stn[isave].frain[k].qual = F_MANUAL;
                    pdata[pcpn_day].stn[isave].sflag[k] = -1;
                    value_edit_flag = true;

                }
            }
            if (value_edit_flag && reset_value == 0) {
                rtotal = 0;

                for (m = 0; m < 4; m++) {
                    if (pdata[pcpn_day].stn[isave].frain[m].data >= 0) {
                        rtotal = rtotal
                                + pdata[pcpn_day].stn[isave].frain[m].data;
                    }
                }

                /*
                 * If setting a 24 hour value to 0, set all corresponding 6 hour
                 * values to zero and set their QC codes to "Manual" as well.
                 */

                if ((Math.abs(pdata[pcpn_day].stn[isave].frain[HOURS_24].data
                        - 0.0) < 0.001) && (time_pos == HOURS_24)) {
                    for (m = 0; m < 4; m++) {
                        pdata[pcpn_day].stn[isave].frain[m].data = 0;
                        pdata[pcpn_day].stn[isave].frain[m].qual = F_MANUAL;
                    }
                    rtotal = 0;
                }

                if (Math.abs(rtotal
                        - pdata[pcpn_day].stn[isave].frain[HOURS_24].data) > .005) {
                    read_text();
                    return;

                }

            } else {

                pdata[pcpn_day].stn[isave].frain[time_pos].qual = (short) new_qual;

                /* 24 hour data set bad/good then 6 hourly bad/good also */

                if (new_qual == F_BAD && time_pos == HOURS_24
                        && pdata[pcpn_day].stn[isave].sflag[time_pos] == 1) {

                    pdata[pcpn_day].stn[isave].frain[time_pos].data = pdata[pcpn_day].stn[isave].rrain[time_pos].data;

                    pdata[pcpn_day].stn[isave].sflag[time_pos] = -1;

                }

                if (time_pos == HOURS_24 && (new_qual == F_BAD
                        || new_qual == F_SCREENED || new_qual == F_VERIFIED
                        || new_qual == F_PARTIAL)) {

                    for (int k = 0; k < 4; k++) {
                        pdata[pcpn_day].stn[isave].frain[k].qual = (short) new_qual;
                    }

                }
                /*-------------------------------------------------------*/
                /*
                 * if 6 hr QC code set Bad by user and token value = ON, then
                 * set 24hr QC code to Bad following code also allows 24 hr
                 * partial data to be set to Bad
                 */

                if (time_pos != HOURS_24 && new_qual == F_BAD
                        && pdata[pcpn_day].stn[isave].frain[HOURS_24].qual != F_ESTIMATED
                        && pdata[pcpn_day].stn[isave].frain[HOURS_24].data >= 0) {
                    if (mpe_dqc_6hr_24hr_flag == 1) {
                        pdata[pcpn_day].stn[isave].frain[HOURS_24].qual = F_BAD;
                    }
                }

                if (pdata[pcpn_day].stn[isave].frain[HOURS_24].qual == F_BAD
                        || pdata[pcpn_day].stn[isave].frain[HOURS_24].data < 0) {
                    if (tcmode == 0) {
                        pdata[pcpn_day].stn[isave].tcons = 1;
                    } else {
                        pdata[pcpn_day].stn[isave].tcons = -1;
                    }
                }

            }

        }

        for (int k = 0; k < 5; k++) {

            if (k < 4) {
                time_pos = pcpn_day * 4 + k;
            } else {
                time_pos = 40 + pcpn_day;
            }

            if (pdata[pcpn_day].used[k] != 0) {
                pdata[pcpn_day].used[k] = 2;
            }
        }

        QcPrecipOptionsDialog.dataSet.clear();
        QcPrecipOptionsDialog.dataSet.addAll(QcPrecipOptionsDialog.dataType);
        for (int k = 1; k < 7; k++) {
            QcPrecipOptionsDialog.dataSet.remove(QcPrecipOptionsDialog.dataSet
                    .indexOf(QcPrecipOptionsDialog.dataType.get(k)));
        }
        String[] a = new String[QcPrecipOptionsDialog.dataSet.size()];
        QcPrecipOptionsDialog
                .setDataSetCombo(QcPrecipOptionsDialog.dataSet.toArray(a));

        if (pcpn_time_step == 0) {
            time_pos = pcp_flag;
        } else {
            time_pos = 40 + pcpn_day;
        }

        int k = 6;
        if (points_flag == 1 && DailyQcUtils.pcp_in_use[time_pos] == -1) {
            k = 0;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == -1) {
            k = 0;
        } else if (points_flag == -1 && grids_flag == 1 && map_flag == -1) {
            k = 1;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == 1) {
            k = 2;
        } else if (points_flag == 1 && grids_flag == 1 && map_flag == -1) {
            k = 3;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == 1) {
            k = 4;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == -1) {
            k = 5;
        } else if (points_flag == 1 && DailyQcUtils.contour_flag == 1) {
            k = 6;
        }

        QcPrecipOptionsDialog.selectDataSetVal(k);
        rpbutton.setEnabled(true);

        BadValues bv = new BadValues();
        bv.update_bad_values(pcpn_day);

        /*
         * do not estimate daily from each other if run DQC on pcpn_day=0
         */
        if (pcpn_day != 0) {
            EstDailyStations eds = new EstDailyStations();
            eds.estimate_daily_stations(pcpn_day, precipStationList,
                    max_stations);
        }

        /*
         * do not estimate partial point precipitation from each other if run
         * DQC on partial time frame and pcpn_day=0. Not considering
         * dqc.curHr12_18 since that is same as the DataOption "6 Hour".
         */
        if (pcpn_day == 0 && (dqc.curHr00_06 == 1 || dqc.curHr06_12 == 1
                || dqc.curHr18_00 == 1)) {
            // do nothing
        } else {
            EstPartStations eps = new EstPartStations();
            eps.estimate_partial_stations(pcpn_day, precipStationList,
                    max_stations);
        }

        QCStations qcs = new QCStations();
        qcs.quality_control_stations(pcpn_day, precipStationList, max_stations);

        CheckConsistency cc = new CheckConsistency();
        cc.check_consistency(pcpn_day, precipStationList, max_stations);

        bv.restore_bad_values(pcpn_day, precipStationList, max_stations);

        if (k == 1 || k == 3) {
            grids_flag = 1;
        }
        return;
    }

    private void checkTextfieldConsistency() {
        float sumof6hrprecip = 0.0f;
        int nonmissing_cnt = 0;
        boolean inconsistency_flag = false;
        float textfield24_val = -9999.0f;
        float textfield6_val = -9999.0f;
        boolean textfield24_missing_flag = false;
        boolean partial_day_flag = false;
        boolean textfield_edit_flag = false;
        String cstr;
        float val = 0;
        float fdif = 0;

        /*
         * check if the sum up of four 6hr value in the textfiles equals to the
         * 24hr value, if not equal set inconsistency_flag as true
         */

        String textfield24_str = precipValueStringArray[HOURS_24];
        if (!textfield24_str.isEmpty()) {
            if (textfield24_str.indexOf('M') != -1) {
                textfield24_missing_flag = true;
            } else {
                textfield24_val = Float.parseFloat(textfield24_str);
            }
        }

        for (int i = 0; i < 4; i++) {
            String textfield6_str = precipValueStringArray[i];
            if (!textfield6_str.isEmpty()) {
                if (textfield6_str.indexOf('M') == -1) {
                    textfield6_val = Float.parseFloat(textfield6_str);
                    sumof6hrprecip += textfield6_val;
                    nonmissing_cnt++;
                }
            }

        }

        // not all 6hr values missing
        if (nonmissing_cnt > 0) {
            if (textfield24_missing_flag) {
                inconsistency_flag = true;
            } else if (Math.abs(textfield24_val - sumof6hrprecip) > 0.005) {
                inconsistency_flag = true;
            } else {
                inconsistency_flag = false;
            }

        } else {
            // all 6hr values are missing
            if (!textfield24_missing_flag) {
                inconsistency_flag = true;
            } else {
                inconsistency_flag = false;
            }
        }

        /*
         * check if partial hydrologic day, if it is, do not popup the warning
         * message
         */
        if (pcpn_day == 0 && (dqc.curHr00_06 == 1 || dqc.curHr06_12 == 1
                || dqc.curHr18_00 == 1)) {
            partial_day_flag = true;
        } else {
            partial_day_flag = false;
        }

        /*
         * check if there is any change in the five partial values text fields
         */
        for (int k = 0; k < 5; k++) {
            Rain frainK = pdata[pcpn_day].stn[isave].frain[k];

            cstr = precipValueStringArray[k];
            int p = cstr.indexOf('M');
            if (p == -1 && frainK.data != -1) {
                val = Float.parseFloat(cstr);
                fdif = Math.abs(val - frainK.data);
            }

            if (fdif > .005) {
                textfield_edit_flag = true;
            } else if (frainK.data != -1 && p != -1) {
                textfield_edit_flag = true;
            } else if (p == -1 && frainK.data == -1) {
                textfield_edit_flag = true;
            }
            cstr = null;
        }
        if (inconsistency_flag && mpe_dqc_warningpopup_flag && !partial_day_flag
                && textfield_edit_flag) {
            String title = "Warning!";
            String msg = "";

            if (Math.abs(textfield24_val) - 0.0 < 0.0001) {
                msg = "All 6hr precip vals are set as 0.0 because 24hr precip val is 0.0, want to continue?";
            } else {
                msg = "The sum of the four 6hr precip values is not equal to the 24hr total, do you want to continue?";
            }
            boolean go = MessageDialog.openQuestion(shell, title, msg);
            if (go) {
                changeCustomFile();
            }
        } else {
            changeCustomFile();
        }

        new OtherPrecipOptions().refresh_exposure();
        // redraw this updated dialog
        this.open();
    }

    protected void read_text() {
        float val, fdif;
        int pcp_flag = DailyQcUtils.pcp_flag;
        Button rpbutton = QcPrecipOptionsDialog.renderGridsBtn;
        int grids_flag = DailyQcUtils.grids_flag;
        int points_flag = DailyQcUtils.points_flag;
        int map_flag = DailyQcUtils.map_flag;
        int contour_flag = DailyQcUtils.contour_flag;

        Rain frain24 = pdata[pcpn_day].stn[isave].frain[HOURS_24];

        for (int k = 0; k < 5; k++) {

            Rain frain = pdata[pcpn_day].stn[isave].frain[k];
            String cstr = precipValueStringArray[k];
            val = 0;
            int p = cstr.indexOf('M');
            if (p == -1) {
                val = Float.parseFloat(cstr);
            }

            fdif = Math.abs(val - frain.data);

            if (p != -1) {
                pdata[pcpn_day].stn[isave].frain[k].data = -1;
                p = -1;
            } else if (fdif > .005 && p == -1) {
                pdata[pcpn_day].stn[isave].frain[k].data = val;
                pdata[pcpn_day].stn[isave].frain[k].qual = F_MANUAL;
                pdata[pcpn_day].stn[isave].sflag[k] = -1;

            }
            cstr = null;
        }

        /*
         * special check if 24hr is set as 0.0, then all 4 6hr value are set as
         * 0.0 as well
         */

        if (Math.abs(frain24.data - 0.0) < 0.001) {
            for (int k = 0; k < 4; k++) {
                pdata[pcpn_day].stn[isave].frain[k].data = 0.0f;
            }
        }

        for (int k = 0; k < 5; k++) {

            if (k < 4) {
                time_pos = pcpn_day * 4 + k;
            } else {
                time_pos = 40 + pcpn_day;
            }

            // pcp_in_use[time_pos] = -1;

            if (pdata[pcpn_day].used[k] != 0) {
                pdata[pcpn_day].used[k] = 2;
            }
        }

        QcPrecipOptionsDialog.dataSet.clear();
        QcPrecipOptionsDialog.dataSet.addAll(QcPrecipOptionsDialog.dataType);

        if (pcpn_time_step == 0) {
            time_pos = pcp_flag;
        } else {
            time_pos = 40 + pcpn_day;
        }

        int k = 4;
        if (points_flag == 1 && DailyQcUtils.pcp_in_use[time_pos] == -1) {
            k = 0;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == -1
                && contour_flag == -1) {
            k = 0;
        } else if (points_flag == -1 && grids_flag == 1 && map_flag == -1) {
            k = 1;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == 1) {
            k = 2;
        } else if (points_flag == 1 && grids_flag == 1 && map_flag == -1) {
            k = 3;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == 1) {
            k = 4;
        } else if (points_flag == -1 && contour_flag == 1) {
            k = 5;
        } else if (points_flag == 1 && contour_flag == 1) {
            k = 6;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == -1
                && contour_flag == -1) {
            k = 7;
        }

        QcPrecipOptionsDialog.selectDataSetVal(k);

        rpbutton.setEnabled(true);

        BadValues bv = new BadValues();
        bv.update_bad_values(pcpn_day);

        /*
         * do not estimate daily and partial point precipitation from each other
         * if run DQC on partial time frame and pcpn_day=0
         */

        if (pcpn_day == 0 && (dqc.curHr00_06 == 1 || dqc.curHr06_12 == 1
                || dqc.curHr18_00 == 1)) {

        } else {
            EstDailyStations eds = new EstDailyStations();
            eds.estimate_daily_stations(pcpn_day, precipStationList,
                    max_stations);

            EstPartStations eps = new EstPartStations();
            eps.estimate_partial_stations(pcpn_day, precipStationList,
                    max_stations);
        }

        QCStations qcs = new QCStations();
        qcs.quality_control_stations(pcpn_day, precipStationList, max_stations);

        CheckConsistency cc = new CheckConsistency();
        cc.check_consistency(pcpn_day, precipStationList, max_stations);

        bv.restore_bad_values(pcpn_day, precipStationList, max_stations);

        if (k == 1 || k == 3) {
            DailyQcUtils.grids_flag = 1;
        }

        OtherPrecipOptions op = new OtherPrecipOptions();
        op.send_expose();
        return;
    }

    private String getStationListPath(String qcArea) {
        String station_dir = DailyQcUtils.mpe_station_list_dir;
        String dir;

        if (qcArea != null) {
            if (station_dir.length() > 0) {
                dir = station_dir + "/" + qcArea + "_station_list";
            } else {
                dir = qcArea;
            }
        } else {
            if (station_dir.length() > 0) {
                dir = station_dir + "/" + qcArea + "_station_list";
            } else {
                dir = qcArea;
            }
        }
        return dir;
    }
}
