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

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.GetClimateSource;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.util.BadValues;
import com.raytheon.viz.mpe.util.CheckConsistency;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Rain;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.EstDailyStations;
import com.raytheon.viz.mpe.util.EstPartStations;
import com.raytheon.viz.mpe.util.QCStations;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This is a popup dialog box to edit Precipitation Station values and quality
 * codes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2009            snaples     Initial creation
 * Jun 27, 2013 15859      wkwock      Update this dialog after click Apply button
 * Nov 26, 2014  16889     snaples     Updated to fix SNOTEL display
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class EditPrecipStationsDialog extends AbstractMPEDialog implements
        StationFilter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditPrecipStationsDialog.class);
    
    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private Font font;

    private int reset_value = 0;

    private int new_qual = 0;

    private String[] precipValueStringArray = new String[5];

    private Text[] precipValueTextArray = new Text[5];

    private Label[] precipValueLabelArray = new Label[5];

    private Label[] qualityCodeStatusLabelArray = new Label[5]; // chip

//    private String[][] timefile = DailyQcUtils.timefile;

    private int time_pos = 0;

    private int pcpn_time_step = MPEDisplayManager.pcpn_time_step;

    private int pcpn_time = dqc.pcpn_time;

    private Button applyBtn;

    private Button closeBtn;

    private Button graphBtn;

    protected Button snotelButton;

    private int retval = 0;

    public Button[] qualityCodeButtonArray;

    public Button[] locationButtonArray;

    public String editVal;

    private boolean snow = false;

    private String[] locationButtonNameArray = { "upper left", "upper right",
            "lower left", "lower right" };

    TextPositionMgr _textPosMgr = new TextPositionMgr();

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

    // prefix "R_" indicates that these quality codes are for RRAIN values.
    @SuppressWarnings("unused")
    private static final int R_SCREENED = 0;

    @SuppressWarnings("unused")
    private static final int R_FORCED = 1;

    @SuppressWarnings("unused")
    private static final int R_LUMPED = 2;

    @SuppressWarnings("unused")
    private static final int R_QUESTIONABLE = 3;

    @SuppressWarnings("unused")
    private static final int R_PARTIAL = 4;

    @SuppressWarnings("unused")
    private static final int R_ESTIMATED = 5;

    @SuppressWarnings("unused")
    private static final int R_TIME_DISTRIBUTED = 6;

    @SuppressWarnings("unused")
    private static final int R_UNKNOWN = 7;

    @SuppressWarnings("unused")
    private static final int R_VERIFIED = 8;

    /*
     * = 1 -- Forced good by user = 2 -- Lumped = 3 -- Questionable = 4 --
     * Forced good by user = 5 -- Estimated = 6 -- T = 8 -- Screened/Verified
     */

    private static final int HOURS_24 = 4; // index value

    private String[] q2bnames = { "Manual", "Reset to Original" };

    private String[] q45bnames = { "Verified", "Screened (Forced)",
            "Questionable", "Bad" };

    private int initial_qual = F_MANUAL;

    int isave = -1;

    String pClimateSource = null;

    int dcmode = OtherPrecipOptions.dcmode;

    int tcmode = OtherPrecipOptions.tcmode;

//    Ts ts[] = DailyQcUtils.ts;

    int tsmax = dqc.tsmax;

    int isom = dqc.isom;

    int gage_char[] = dqc.gage_char;

    int method = dqc.method;

    int qflag[] = dqc.qflag;

//    Pdata pdata[] = DailyQcUtils.pdata;

    int dflag[] = dqc.dflag;

    int naflag;

//    ArrayList<Station> precipStationList = DailyQcUtils.precip_stations;

//    ReadPrecipStationList rp = new ReadPrecipStationList();

    int max_stations = dqc.precip_stations.size();

    int i, m, x, y;

    float lat, lon;

    int initial_pos;

    int[] allowedQualityCodes = dqc.func;

    int pcpn_day = dqc.pcpn_day;

    Coordinate coord = new Coordinate();

    boolean mpe_dqc_warningpopup_flag = false;

    private Shell parent;

    // -----------------------------------
    // variables having to do with a redisplay
    // private boolean isDisposed = false;
    // private static EditPrecipStationsDialog previousDialogInstance = null;
    // private static Point previousLocation = null;

    private static WindowReplacementHelper windowReplacementHelper = new WindowReplacementHelper();

    // -----------------------------------------------------------------------------

    @SuppressWarnings("unused")
    private EditPrecipStationsDialog(Shell parentShell) {
        this(parentShell, null);
    }

    // -----------------------------------------------------------------------------

    public EditPrecipStationsDialog(Shell parentShell,
            ReferencedCoordinate rcoord) {
        super(parentShell);
        if (rcoord != null) {
            try {
                coord = rcoord.asLatLon();
            } catch (TransformException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (FactoryException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        // Envelope env = new Envelope(coord);

        AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (mgr != null) {
            mgr.addPerspectiveDialog(this);
        }
    }

    /**
     * Open method used to display the Group Edit Stations dialog.
     * 
     * @return Null.
     */
    public Integer open() {
        parent = this.getParent();
        Display display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Edit Precipitation Stations");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null) {
                    mgr.removePespectiveDialog(EditPrecipStationsDialog.this);
                    windowReplacementHelper.setIsDisposed(true);
                }
            }
        });

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        initializeComponents();
        if (isave == -1) {
            System.out.println("isave == -1 !!! quit!");
            shell.dispose();
            return retval;
        }

        shell.pack();

        windowReplacementHelper.manageWindows(this);

        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        font.dispose();

        removePerspectiveListener();

        return retval;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        ClosestStationFinder finder = new ClosestStationFinder(this);

        isave = finder.findClosestStation(dqc.precip_stations, coord);
        if (isave == -1) {
            return;
        }

        Rain frain = dqc.pdata[pcpn_day].stn[isave].frain[time_pos];

        reset_value = 0;
        initial_qual = frain.qual;
        new_qual = initial_qual;
        
        Rain srain = dqc.pdata[pcpn_day].stn[isave].srain[time_pos];

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
    private void loadPrecipStationText(StringBuilder stationStringBuilder,
            Station selectedStation) {

        Rain frain = dqc.pdata[pcpn_day].stn[isave].frain[time_pos];

        stationStringBuilder.append(selectedStation.hb5 + " "
                + selectedStation.parm + "\n" + selectedStation.name + "\n"
                + String.format("%d ft    ", selectedStation.elev));

        if (selectedStation.tip == 0) {
            stationStringBuilder.append("tipping");
        } else {
            stationStringBuilder.append("weighing");
        }

        stationStringBuilder.append("\n");
        if (method == 2 && selectedStation.isoh[isom] > 0) {
            GetClimateSource gc = new GetClimateSource();
            pClimateSource = gc.getClimateSource(selectedStation.cparm);

            stationStringBuilder.append(String.format(
                    "monthly normal %5.2f in. source: %s\n",
                    selectedStation.isoh[isom], pClimateSource));
        }
        if (frain.data >= 0) {
            stationStringBuilder.append(String.format(
                    "estimate %5.2f in. dev %5.2f\n", frain.estimate,
                    frain.stddev));
        }

        int frzlvl = dqc.pdata[pcpn_day].stn[isave].frzlvl[time_pos];
        if (selectedStation.tip == 0 && time_pos != HOURS_24 && frzlvl > -99) {
            stationStringBuilder.append(String.format("Freezing level %dft\n",
                    frzlvl));
        }

        short snoflag = dqc.pdata[pcpn_day].stn[isave].snoflag[time_pos];

        if (snoflag > 0) {
            stationStringBuilder.append("SNOTEL error is ");

            if (snoflag == 1) {
                stationStringBuilder.append(" SNOTEL >> PCPN\n");
            }

            else if (snoflag == 2) {
                stationStringBuilder.append(" CONTINUATION\n");
            }

            else if (snoflag == 3) {
                stationStringBuilder.append(" PCPN RESET\n");
            }

            else if (snoflag == 4) {
                stationStringBuilder.append(" PCPN >> SNOTEL\n");
            }

        }

        Rain srain = dqc.pdata[pcpn_day].stn[isave].srain[time_pos];

        if (srain.data > -98) {

            stationStringBuilder.append(String.format(
                    "Snow water change is %5.2f in.", srain.data));
            if (time_pos == HOURS_24 && srain.data >= 0) {
                snow = true;
            }

        }

        // only display in 24-hour mode
        if ((time_pos == HOURS_24) && (dqc.QPEaccum24hr != null)) {

            double accumulatedAmount = get24HourPrecipTotal(
                    dqc.QPEaccum24hr, selectedStation.hrap_x
                            - dqc.getHrap_grid().hrap_minx,
                    selectedStation.hrap_y
                            - dqc.getHrap_grid().hrap_miny);

            stationStringBuilder.append(String.format(
                    "accumulated amount %5.2f in.", accumulatedAmount));
        }
    }

    // --------------------------------------------------------

    private double get24HourPrecipTotal(double[][][] qpeAccum24hr,
            float hrap_x, float hrap_y) {
        double value = -9.0;
        int dayIndex = pcpn_day;

        if (qpeAccum24hr != null) {
            int x = (int) Math.floor(hrap_x);
            int y = (int) Math.floor(hrap_y);

            try {

                value = qpeAccum24hr[dayIndex][y][x];
            } catch (Throwable t) {
                System.out.println("Failed to get data from qpeAccum24hr["
                        + dayIndex + "][" + y + "][" + x + "]");
            }
        }

        return value;
    }

    // --------------------------------------------------------
    public boolean shouldFilterOut(int stationIndex) {
        boolean filteredOut = false;
        Station station = dqc.precip_stations.get(stationIndex);

        Rain frain = dqc.pdata[pcpn_day].stn[stationIndex].frain[time_pos];

        if ((frain.data > QcPrecipOptionsDialog.getPointFilterReverseValue())
                && (frain.data < 20.00)) {
            return true;
        }

        if (station.elev > 0
                && station.elev < dqc.elevation_filter_value) {
            return true;
        }

        if (frain.data < 0) {
            return true;
        }

        if (frain.data < QcPrecipOptionsDialog.getPointFilterValue()) {
            return true;
        }

        if (tcmode == 0 && dqc.pdata[pcpn_day].stn[stationIndex].tcons == -1) {
            return true;
        }

        if (tcmode == 1 && dqc.pdata[pcpn_day].stn[stationIndex].tcons == 1) {
            return true;
        }

        if (dcmode == 0
                && dqc.pdata[pcpn_day].stn[stationIndex].scons[time_pos] == -1) {
            return true;
        }

        if (dcmode == 1
                && dqc.pdata[pcpn_day].stn[stationIndex].scons[time_pos] == 1) {
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
            if ((kd.compareTo(dqc.ts[m].abr) == 0 && dflag[m + 1] == 1)) {
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

        StringBuilder stringBuilder = new StringBuilder();
        Station selectedStation = dqc.precip_stations.get(isave);
        loadPrecipStationText(stringBuilder, selectedStation);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);

        Label hb5Lbl = new Label(dataComp, SWT.LEFT);
        hb5Lbl.setText(stringBuilder.toString());
        hb5Lbl.setLayoutData(gd);

        if (snow == true) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            snotelButton = new Button(dataComp, SWT.CHECK);
            snotelButton.setText("Use SWD for PPD");
            snotelButton.setLayoutData(gd);

            Rain srain = dqc.pdata[pcpn_day].stn[isave].srain[time_pos];
            short sflag = dqc.pdata[pcpn_day].stn[isave].sflag[time_pos];

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

        if (initial_qual < 0
                || dqc.pdata[pcpn_day].stn[isave].frain[time_pos].data < 0) {
            naflag = 1;
        } else {
            naflag = 0;
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
            List<Integer> allowableChangeList = getAllowableChangeList(initial_qual);

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
        final Integer[] questionableArray = { F_QUESTIONABLE, F_BAD, F_SCREENED };
        final Integer[] screenedArray = { F_SCREENED, F_BAD };
        final Integer[] badArray = { F_BAD, F_VERIFIED, F_SCREENED,
                F_QUESTIONABLE };
        final Integer[] partialArray = { F_BAD };
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

        Station selectedStation = dqc.precip_stations.get(isave);
        initial_pos = _textPosMgr.getTextPosition(selectedStation);

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
                    Station station = dqc.precip_stations.get(isave);
                    int textPositionCode = (Integer) b.getData();
                    _textPosMgr
                            .changeStationLocation(textPositionCode, station);
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

        for (int i = 0; i < 5; i++) {

            String valueString;
            precipValueLabelArray[i] = new Label(stnConComp, SWT.LEFT);
            precipValueLabelArray[i].setText(dqc.timefile[2][i]);
            precipValueTextArray[i] = new Text(stnConComp, SWT.LEFT
                    | SWT.BORDER | SWT.READ_ONLY);
            
            qualityCodeStatusLabelArray[i] = new Label(stnConComp, SWT.CENTER);
            
            int qualityCode = dqc.pdata[pcpn_day].stn[isave].frain[i].qual;
            String qualityText = getQualityTextFromCode(qualityCode);
            
            qualityCodeStatusLabelArray[i].setText(qualityText);

            Rain frainI = dqc.pdata[pcpn_day].stn[isave].frain[i];
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
        } // end for

    } // end createStnConComp()

    private String getQualityTextFromCode(int qualityCode) {
        return qualityCodeNameArray[qualityCode];

    }

    private void createButtonComp() {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnGpComp = new Composite(shell, SWT.NONE);
        GridLayout btnGpCompLayout = new GridLayout(3, false);
        btnGpComp.setLayout(btnGpCompLayout);
        btnGpComp.setLayoutData(gd);

        GridData bd = new GridData(110, 25);
        applyBtn = new Button(btnGpComp, SWT.PUSH);
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
                handleApplyActions();
                // check_textfield_consistency();
            }
        });

        closeBtn = new Button(btnGpComp, SWT.PUSH);
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
                retval = 0;
                shell.dispose();
            }

        });
        graphBtn = new Button(btnGpComp, SWT.PUSH);
        graphBtn.setText("Graph");
        graphBtn.setLayoutData(bd);
        graphBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                AppLauncherHandler alh = new AppLauncherHandler();
                String lid = dqc.precip_stations.get(isave).hb5;
                String dataType = dqc.precip_stations.get(isave).parm;
                final String TSL_BUNDLE_LOC = "bundles/run-TimeSeriesLite.xml";
                try {
                    System.out
                            .println("Launching TSL " + lid + ", " + dataType);
                    alh.execute(TSL_BUNDLE_LOC, lid, dataType);
                } catch (ExecutionException ee) {
                    // TODO Auto-generated catch block
                    ee.printStackTrace();
                }
                retval = 2;
            }

        });
    }

    private void handleApplyActions() {
        // check values for consistency
        check_textfield_consistency(); // for the station consistency section
    }

    protected void resetStationQuality(Integer data) {
        int k;

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        if (data == 1) {

            for (k = 0; k < 5; k++) {

                dqc.pdata[pcpn_day].stn[isave].frain[k].qual = dqc.pdata[pcpn_day].stn[isave].rrain[k].qual;

                dqc.pdata[pcpn_day].stn[isave].frain[k].data = dqc.pdata[pcpn_day].stn[isave].rrain[k].data;

            }

            reset_value = 1;
            new_qual = dqc.pdata[pcpn_day].stn[isave].rrain[time_pos].qual;

        } else {
            reset_value = 0;
        }

    }

    protected void changeStationQuality(Integer data) {
        String header = "EditPrecipStationsDialog.changeStationQuality()";

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        new_qual = allowedQualityCodes[data];
        dqc.pdata[pcpn_day].stn[isave].frain[time_pos].qual = (short) new_qual;
        System.out.println(header + " new_qual = " + new_qual);
    }

    protected void changeCustomFile() {

        String header = "EditPrecipStationsDialog.changeCustomFile(): ";

        String pathName = getStationListPath(dqc.currentQcArea);
        String station_list_custom_file = pathName + "_label_position";
        int i;
        int time_pos;
        float val, fdif;
        String cstr;
        int k, p;
//        int[] pcp_in_use = dqc.pcp_in_use;
        Boolean bval = false;
        float rtotal;
        int m;
        Button rpbutton = QcPrecipOptionsDialog.renderGridsBtn;
        int tcmode = OtherPrecipOptions.tcmode;
        BufferedWriter out = null;
        int pcp_flag = dqc.pcp_flag;
        int grids_flag = dqc.grids_flag;
        int points_flag = dqc.points_flag;
        int map_flag = dqc.map_flag;
        File custom = new File(station_list_custom_file);
        custom.setReadable(true, false);
        custom.setWritable(true, false);

        String mpe_dqc_6hr_24hr_string = AppsDefaults.getInstance().getToken(
                "mpe_dqc_6hr_24hr_set_bad", "OFF");

        int mpe_dqc_6hr_24hr_flag = 0;

        if (mpe_dqc_6hr_24hr_string.equalsIgnoreCase("ON")) {
            mpe_dqc_6hr_24hr_flag = 1;
        }

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = HOURS_24;
        }

        try {
            out = new BufferedWriter(new FileWriter(custom));

            for (i = 0; i < max_stations; i++) {
                Station station = dqc.precip_stations.get(i);
                String rec = String.format("%s %s %d %d\n", station.hb5,
                        station.parm, station.xadd, station.yadd);
                out.write(rec);
            }
            out.close();
        } catch (IOException e) {
            System.out.println(String.format("Could not open file: %s\n",
                    station_list_custom_file));
            e.printStackTrace();
            return;
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        if (snow == true) {
            bval = snotelButton.getSelection();
        }

        /* snotel path */

        if (snow == true
                && ((bval == true && dqc.pdata[pcpn_day].stn[isave].sflag[HOURS_24] == -1) || (bval == false && dqc.pdata[pcpn_day].stn[isave].sflag[HOURS_24] == 1))) {

            dqc.pdata[pcpn_day].stn[isave].sflag[HOURS_24] = (short) -dqc.pdata[pcpn_day].stn[isave].sflag[HOURS_24];

            if (dqc.pdata[pcpn_day].stn[isave].sflag[HOURS_24] == 1) {

                dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].data = dqc.pdata[pcpn_day].stn[isave].srain[HOURS_24].data;
                dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].qual = F_VERIFIED;
            }

            else {
                dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].data = dqc.pdata[pcpn_day].stn[isave].rrain[HOURS_24].data;
                dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].qual = F_VERIFIED;
            }

        }

        else {
            boolean value_edit_flag = false;
            val = dqc.pdata[pcpn_day].stn[isave].frain[time_pos].data;
            p = -1;
            // if (editVal != null) // null unless a value was edited
            // {
            for (k = 0; k < 5; k++) {
                /* other path */
                cstr = precipValueStringArray[k];
                System.out.println(header + "cstr = " + cstr);
                val = Float.parseFloat(cstr);
                System.out.println(header + "value = " + val);
                p = cstr.indexOf('M');
                cstr = null;

                // } // end if editVal != null
                /* use manually entered data */

                /* need to ensure consistency in 6 and 24 hour data??? */

                fdif = Math.abs(val
                        - dqc.pdata[pcpn_day].stn[isave].frain[time_pos].data);

                if (fdif > .005 && p == -1 && reset_value == 0) {
                    dqc.pdata[pcpn_day].stn[isave].frain[k].data = val;
                    dqc.pdata[pcpn_day].stn[isave].frain[k].qual = F_MANUAL;
                    dqc.pdata[pcpn_day].stn[isave].sflag[k] = -1;
                    value_edit_flag = true;

                }
            }
            if (value_edit_flag == true && reset_value == 0) {
                rtotal = 0;

                for (m = 0; m < 4; m++) {
                    if (dqc.pdata[pcpn_day].stn[isave].frain[m].data >= 0) {
                        rtotal = rtotal
                                + dqc.pdata[pcpn_day].stn[isave].frain[m].data;
                    }
                }

                /*
                 * If setting a 24 hour value to 0, set all corresponding 6 hour
                 * values to zero and set their QC codes to "Manual" as well.
                 */

                if ((Math
                        .abs(dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].data - 0.0) < 0.001)
                        && (time_pos == HOURS_24)) {
                    for (m = 0; m < 4; m++) {
                        dqc.pdata[pcpn_day].stn[isave].frain[m].data = 0;
                        dqc.pdata[pcpn_day].stn[isave].frain[m].qual = F_MANUAL;
                    }
                    rtotal = 0;
                }

                if (Math.abs(rtotal
                        - dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].data) > .005) {
                    read_text();
                    return;

                }

            } // end if (frain.qual == MANUAL || tcmode == 1)

            else {

                dqc.pdata[pcpn_day].stn[isave].frain[time_pos].qual = (short) new_qual;

                /* 24 hour data set bad/good then 6 hourly bad/good also */

                if (new_qual == F_BAD && time_pos == HOURS_24
                        && dqc.pdata[pcpn_day].stn[isave].sflag[time_pos] == 1) {

                    dqc.pdata[pcpn_day].stn[isave].frain[time_pos].data = dqc.pdata[pcpn_day].stn[isave].rrain[time_pos].data;

                    dqc.pdata[pcpn_day].stn[isave].sflag[time_pos] = -1;

                }

                if (time_pos == HOURS_24
                        && (new_qual == F_BAD || new_qual == F_SCREENED
                                || new_qual == F_VERIFIED || new_qual == F_PARTIAL)) {

                    for (k = 0; k < 4; k++) {
                        /* if(pdata[pcpn_day].stn[isave].frain[k].qual!=1) */
                        dqc.pdata[pcpn_day].stn[isave].frain[k].qual = (short) new_qual;
                    }

                }
                /*-------------------------------------------------------*/
                /*
                 * if 6 hr QC code set bad by user, then some RFCs want the 24hr
                 * QC code to be set to Bad
                 */
                /* others want the 24hr QC code to remain unchanged */
                /* added token to choose behavior */
                /* following code also allows partial data to be set as bad */
                /* 6 hour data set bad set 24 hour bad too */

                if (time_pos != HOURS_24
                        && new_qual == F_BAD
                        && dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].qual != F_ESTIMATED
                        && dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].data >= 0) {
                    if (mpe_dqc_6hr_24hr_flag == 1) {
                        System.out
                                .println("6hr qual code set to Bad - 24hr qual code changed to Bad\n");
                        // frain24.qual = (short) new_qual;
                        dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24].qual = (short) new_qual;
                    }
                }

                /*
                 * --------------------------------------------------------------
                 * --
                 */

                if (dqc.pdata[pcpn_day].stn[isave].frain[4].qual == F_BAD
                        || dqc.pdata[pcpn_day].stn[isave].frain[4].data < 0) {
                    if (tcmode == 0) {
                        dqc.pdata[pcpn_day].stn[isave].tcons = 1;
                    } else {
                        dqc.pdata[pcpn_day].stn[isave].tcons = -1;
                    }
                }
            } // end else
        } // end else

        for (k = 0; k < 5; k++) {

            if (k < 4) {
                time_pos = pcpn_day * 4 + k;
            } else {
                time_pos = 40 + pcpn_day;
            }

            // pcp_in_use[time_pos] = -1;

            if (dqc.pdata[pcpn_day].used[k] != 0) {
                dqc.pdata[pcpn_day].used[k] = 2;
            }
        }

        QcPrecipOptionsDialog.dataSet.clear();
        QcPrecipOptionsDialog.dataSet.addAll(QcPrecipOptionsDialog.dataType);
        for (k = 1; k < 7; k++) {
            QcPrecipOptionsDialog.dataSet.remove(QcPrecipOptionsDialog.dataSet
                    .indexOf(QcPrecipOptionsDialog.dataType.get(k)));
        }
        String[] a = new String[QcPrecipOptionsDialog.dataSet.size()];
        QcPrecipOptionsDialog.setDataSetCombo(QcPrecipOptionsDialog.dataSet
                .toArray(a));

        if (pcpn_time_step == 0) {
            time_pos = pcp_flag;
        } else {
            time_pos = 40 + pcpn_day;
        }

        if (points_flag == 1 && dqc.pcp_in_use[time_pos] == -1) {
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
        } else if (points_flag == 1 && dqc.contour_flag == 1) {
            k = 6;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == -1) {
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

        if (pcpn_day == 0
                && (dqc.curHr00_06 == 1
                        || dqc.curHr06_12 == 1 || dqc.curHr18_00 == 1)) {
            // do nothing
        } else {
            EstDailyStations eds = new EstDailyStations();
            eds.estimate_daily_stations(pcpn_day, dqc.precip_stations,
                    max_stations);

            EstPartStations eps = new EstPartStations();
            eps.estimate_partial_stations(pcpn_day, dqc.precip_stations,
                    max_stations);
        }

        QCStations qcs = new QCStations();
        qcs.quality_control_stations(pcpn_day, dqc.precip_stations, max_stations);

        CheckConsistency cc = new CheckConsistency();
        cc.check_consistency(pcpn_day, dqc.precip_stations, max_stations);

        bv.restore_bad_values(pcpn_day, dqc.precip_stations, max_stations);

        // logMessage("new_qual again %d station qual %d\n", new_qual,
        // pdata[pcpn_day].stn[isave].frain[4].qual);

        if (k == 1 || k == 3) {
            grids_flag = 1;
        }

        OtherPrecipOptions op = new OtherPrecipOptions();
        op.send_expose();
        return;
    }

    private void check_textfield_consistency() {
        float sumof6hrprecip = 0.0f;
        int nonmissing_cnt = 0;
        boolean inconsistency_flag = false;
        String textfield24_str = "";
        String textfield6_str = "";
        float textfield24_val = -9999.0f;
        float textfield6_val = -9999.0f;
        boolean textfield24_missing_flag = false;
        boolean partial_day_flag = false;
        boolean textfield_edit_flag = false;
        String cstr;
        int k;
        int p;
        float val = 0;
        float fdif = 0;

        /*
         * check if the sum up of four 6hr value in the textfiles equals to the
         * 24hr value, if not equal set inconsistency_flag as true
         */

        textfield24_str = precipValueStringArray[HOURS_24];
        if (textfield24_str != "") {
            if (textfield24_str.indexOf('M') != -1) {
                textfield24_missing_flag = true;
            } else {
                textfield24_val = Float.parseFloat(textfield24_str);
            }
        }

        for (i = 0; i < 4; i++) {
            textfield6_str = precipValueStringArray[i];
            if (textfield6_str != "") {
                if (textfield6_str.indexOf('M') == -1) {
                    textfield6_val = Float.parseFloat(textfield6_str);
                    sumof6hrprecip += textfield6_val;
                    nonmissing_cnt++;
                }
            }

        }
        if (nonmissing_cnt > 0) // not all 6hr values missing
        {
            if (textfield24_missing_flag == true) {
                inconsistency_flag = true;
            } else if (Math.abs(textfield24_val - sumof6hrprecip) > 0.005) {
                inconsistency_flag = true;
            } else {
                inconsistency_flag = false;
            }

        } else // all 6hr values are missing
        {
            if (textfield24_missing_flag == false) {
                inconsistency_flag = true;
            } else {
                inconsistency_flag = false;
            }
        }

        /*
         * check if partial hydrologic day, if it is, do not popup the warning
         * message
         */
        if (pcpn_day == 0
                && (dqc.curHr00_06 == 1
                        || dqc.curHr06_12 == 1 || dqc.curHr18_00 == 1)) {
            partial_day_flag = true;
        } else {
            partial_day_flag = false;
        }

        /* check if there is any change in the five partial values text fields */
        for (k = 0; k < 5; k++) {
            Rain frainK = dqc.pdata[pcpn_day].stn[isave].frain[k];

            cstr = precipValueStringArray[k];
            p = cstr.indexOf('M');
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
        if (inconsistency_flag == true && mpe_dqc_warningpopup_flag == true
                && partial_day_flag == false && textfield_edit_flag == true) {
            String title = "Warning!";
            String msg = "";

            if (Math.abs(textfield24_val) - 0.0 < 0.0001) {
                msg = "All 6hr precip vals are set as 0.0 because 24hr precip val is 0.0, want to continue?";
            } else {
                msg = "The sum of the four 6hr precip values is not equal to the 24hr total, do you want to continue?";
            }
            boolean go = MessageDialog.openQuestion(shell, title, msg);
            if (go == true) {
                changeCustomFile();
                // shell.dispose();
            }
        } else {
            changeCustomFile();
            // shell.dispose();
        }

        this.open();//redraw this updated dialog
    }

    protected void read_text() {
        int k, p;
        String cstr;
        float val, fdif;
//        int[] pcp_in_use = DailyQcUtils.pcp_in_use;
        int pcp_flag = dqc.pcp_flag;
        Button rpbutton = QcPrecipOptionsDialog.renderGridsBtn;
        int grids_flag = dqc.grids_flag;
        int points_flag = dqc.points_flag;
        int map_flag = dqc.map_flag;
        int contour_flag = dqc.contour_flag;

        Rain frain24 = dqc.pdata[pcpn_day].stn[isave].frain[HOURS_24];

        for (k = 0; k < 5; k++) {

            Rain frain = dqc.pdata[pcpn_day].stn[isave].frain[k];

            cstr = precipValueStringArray[k];
            val = 0;
            p = cstr.indexOf('M');
            if (p == -1) {
                val = Float.parseFloat(cstr);
            }

            fdif = Math.abs(val - frain.data);

            if (p != -1) {
                dqc.pdata[pcpn_day].stn[isave].frain[k].data = -1;
                p = -1;
            } else if (fdif > .005 && p == -1) {

                dqc.pdata[pcpn_day].stn[isave].frain[k].data = val;
                dqc.pdata[pcpn_day].stn[isave].frain[k].qual = F_MANUAL;
                dqc.pdata[pcpn_day].stn[isave].sflag[k] = -1;

            }
            cstr = null;
        }

        /*
         * special check if 24hr is set as 0.0, then all 4 6hr value are set as
         * 0.0 as well
         */

        if (Math.abs(frain24.data - 0.0) < 0.001) {
            for (k = 0; k < 4; k++) {
                dqc.pdata[pcpn_day].stn[isave].frain[k].data = 0.0f;
            }
        }

        for (k = 0; k < 5; k++) {

            if (k < 4) {
                time_pos = pcpn_day * 4 + k;
            } else {
                time_pos = 40 + pcpn_day;
            }

            // pcp_in_use[time_pos] = -1;

            if (dqc.pdata[pcpn_day].used[k] != 0) {
                dqc.pdata[pcpn_day].used[k] = 2;
            }
        }

        QcPrecipOptionsDialog.dataSet.clear();
        QcPrecipOptionsDialog.dataSet.addAll(QcPrecipOptionsDialog.dataType);
        // for (k = 1; k < 7; k++) {
        // QcPrecipOptionsDialog.dataSet.remove(QcPrecipOptionsDialog.dataSet
        // .indexOf(QcPrecipOptionsDialog.dataType.get(k)));
        // }

        if (pcpn_time_step == 0) {
            time_pos = pcp_flag;
        } else {
            time_pos = 40 + pcpn_day;
        }

        if (points_flag == 1 && dqc.pcp_in_use[time_pos] == -1) {
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

        if (pcpn_day == 0
                && (dqc.curHr00_06 == 1
                        || dqc.curHr06_12 == 1 || dqc.curHr18_00 == 1)) {

        } else {
            EstDailyStations eds = new EstDailyStations();
            eds.estimate_daily_stations(pcpn_day, dqc.precip_stations,
                    max_stations);

            EstPartStations eps = new EstPartStations();
            eps.estimate_partial_stations(pcpn_day, dqc.precip_stations,
                    max_stations);
        }

        QCStations qcs = new QCStations();
        qcs.quality_control_stations(pcpn_day, dqc.precip_stations, max_stations);

        CheckConsistency cc = new CheckConsistency();
        cc.check_consistency(pcpn_day, dqc.precip_stations, max_stations);

        bv.restore_bad_values(pcpn_day, dqc.precip_stations, max_stations);

        if (k == 1 || k == 3) {
            dqc.grids_flag = 1;
        }

        OtherPrecipOptions op = new OtherPrecipOptions();
        op.send_expose();
        return;
    }

    private String getStationListPath(String qcArea) {
        String station_dir = dqc.mpe_station_list_dir;
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
