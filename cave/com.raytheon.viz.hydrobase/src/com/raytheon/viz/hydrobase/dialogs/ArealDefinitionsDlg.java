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
package com.raytheon.viz.hydrobase.dialogs;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.data.GeoAreaData;
import com.raytheon.viz.hydrobase.data.GeoDataManager;
import com.raytheon.viz.hydrobase.data.HrapBinList;
import com.raytheon.viz.hydrobase.data.LineSegment;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroConstants.ArealTypeSelection;
import com.raytheon.viz.hydrocommon.texteditor.TextEditorDlg;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class displays the Areal Definitions dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 09 Sep 2009  2772       mpduff      Implemented Dialog.
 * 16 Apr 2013  1790       rferrel     Made dialog non-blocking.
 * 16 Jul 2013  2088       rferrel     Changes for non-blocking TextEditorDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ArealDefinitionsDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArealDefinitionsDlg.class);

    private static final String[] GEOAREA_FILENAMES = { "zones.dat",
            "counties.dat", "basins.dat", "resvrs.dat" };

    private static final String GEOAREA_FORMAT = "%-8s %-40s %9s  %9s";

    private static final String[] IMPORT_LOGS = { "process_geoarea_ZONE.log",
            "process_geoarea_COUNTY.log", "process_geoarea_BASIN.log",
            "process_geoarea_RESRVR.log" };

    private static final int LOC_AREANAME_LEN = 40;

    /**
     * Font used with the controls.
     */
    private Font controlFont;

    /**
     * List combo box.
     */
    private Combo listCbo;

    /**
     * Area list control.
     */
    private List areaList;

    /**
     * Source data file text control.
     */
    private Text sourceDataFileTF;

    /**
     * Create import button.
     */
    private Button importBtn;

    /**
     * Review log button.
     */
    private Button reviewLogBtn;

    /**
     * Edit file button.
     */
    private Button editFileBtn;

    /**
     * Is the filename a default or user-specified file
     */
    boolean defaultMatch = false;

    /** The selected type */
    private ArealTypeSelection selectedType;

    /** The log file */
    private File logFile;

    private BufferedWriter logger = null;

    /** Log file open flag */
    private boolean logFileOpen = false;

    /** Allow single instance of editor for a given file. */
    private final Map<File, TextEditorDlg> textEditorDlgMap = new HashMap<File, TextEditorDlg>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ArealDefinitionsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Areal Definitions");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
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
        setReturnValue(false);

        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListComboControl();
        createAreaListControl();
        createImportGroup();
        createOkButton();

        try {
            loadAreaList(ArealTypeSelection.ZONES);
            loadAreaImport(ArealTypeSelection.ZONES);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error updating Geo Area List", e);
        }
    }

    /**
     * Create the List combo box control.
     */
    private void createListComboControl() {
        Composite comboComp = new Composite(shell, SWT.NONE);
        comboComp.setLayout(new GridLayout(2, false));

        Label listLbl = new Label(comboComp, SWT.NONE);
        listLbl.setText("List:");

        listCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        listCbo.add("Zones");
        listCbo.add("Counties");
        listCbo.add("Basins");
        listCbo.add("Reservoirs");
        listCbo.select(0);
        selectedType = ArealTypeSelection.ZONES;

        listCbo.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                super.widgetSelected(e);
                handleTypeSelection();
            }

        });
    }

    /**
     * Create the area list control.
     */
    private void createAreaListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite areaComp = new Composite(shell, SWT.NONE);
        areaComp.setLayout(new GridLayout(1, false));
        areaComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label topLbl = new Label(areaComp, SWT.NONE);
        topLbl.setText(getAreaListTopText());
        topLbl.setFont(controlFont);
        topLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label bottomLbl = new Label(areaComp, SWT.NONE);
        bottomLbl.setText(getAreaListBottomText());
        bottomLbl.setFont(controlFont);
        bottomLbl.setLayoutData(gd);

        gd = new GridData(600, 250);
        areaList = new List(areaComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        areaList.setLayoutData(gd);
        areaList.setFont(controlFont);
    }

    /**
     * Create the Import group and controls.
     */
    private void createImportGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group importGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 25;
        importGroup.setLayout(gl);
        importGroup.setLayoutData(gd);
        importGroup.setText(" Import Operations ");

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label sourceDataLbl = new Label(importGroup, SWT.NONE);
        sourceDataLbl.setText("Source Data File:");
        sourceDataLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        sourceDataFileTF = new Text(importGroup, SWT.BORDER);
        sourceDataFileTF.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        importBtn = new Button(importGroup, SWT.PUSH);
        importBtn.setText("Import to Database");
        importBtn.setLayoutData(gd);
        importBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleImport();
            }
        });

        gd = new GridData(180, SWT.DEFAULT);
        reviewLogBtn = new Button(importGroup, SWT.PUSH);
        reviewLogBtn.setText("Review Log");
        reviewLogBtn.setLayoutData(gd);
        reviewLogBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadGeoAreaLog();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        editFileBtn = new Button(importGroup, SWT.PUSH);
        editFileBtn.setText("Edit File");
        editFileBtn.setLayoutData(gd);
        editFileBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                File f = getAreaFilename();
                if (f != null) {
                    TextEditorDlg teDlg = textEditorDlgMap.get(f);
                    if (teDlg == null || teDlg.isDisposed()) {
                        teDlg = new TextEditorDlg(shell, false, f);
                        teDlg.setCloseCallback(new ICloseCallback() {

                            @Override
                            public void dialogClosed(Object returnValue) {
                                if (returnValue instanceof File) {
                                    File f = (File) returnValue;
                                    textEditorDlgMap.remove(f);
                                }
                            }
                        });
                        teDlg.open();
                        textEditorDlgMap.put(f, teDlg);
                    } else {
                        teDlg.bringToTop();
                    }
                }
            }
        });
    }

    /**
     * Create the OK button at the bottom of the display.
     */
    private void createOkButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get the top label text for the area list label.
     * 
     * @return Label text.
     */
    private String getAreaListTopText() {
        String format = "                                                         %S";

        String str = String.format(format, "Interior");

        return str;
    }

    /**
     * Get the bottom label text for the area list label.
     * 
     * @return Label text.
     */
    private String getAreaListBottomText() {
        String format = "%S  %S                                         %S        %S";

        String str = String.format(format, "Area Id", "Name", "Lat", "Lon");

        return str;
    }

    /**
     * Handle the type selection event
     */
    private void handleTypeSelection() {
        selectedType = ArealTypeSelection.values()[listCbo.getSelectionIndex()];

        /* reload the list */
        try {
            loadAreaList(selectedType);
            loadAreaImport(selectedType);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error updating Geo Area List", e);
        }

    }

    /**
     * Load the scrolled list for the GeoArea info.
     * 
     * @param type
     *            The ArealTypeSelection
     * @throws VizException
     */
    private void loadAreaList(ArealTypeSelection type) throws VizException {
        /* load the list of geo areas from the database */
        GeoDataManager dman = GeoDataManager.getInstance();

        ArrayList<GeoAreaData> areaDataList = dman.getGeoArea(type);

        /* Clear the list */
        areaList.removeAll();

        /* Populate the list */
        GeoUtil geoUtils = GeoUtil.getInstance();
        for (GeoAreaData row : areaDataList) {
            String lat = geoUtils.cvt_latlon_from_double(row.getInteriorLat());
            String lon = geoUtils.cvt_latlon_from_double(row.getInteriorLon());
            areaList.add(String.format(GEOAREA_FORMAT, row.getAreaId(),
                    row.getName(), lat, lon));
        }

    }

    /**
     * Load the import filename for the GeoArea info.
     * 
     * @param type
     *            The ArealTypeSelection
     */
    private void loadAreaImport(ArealTypeSelection type) {
        sourceDataFileTF.setText(GEOAREA_FILENAMES[type.ordinal()]);
    }

    private void handleImport() {
        shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        File importFile = getAreaFilename();

        /*
         * if the file was not accessible don't continue with the import
         */
        if (importFile == null) {
            shell.setCursor(null);
            return;
        }

        String msg = null;
        /* confirm import of info */
        if (defaultMatch) {
            msg = String
                    .format("Importing %s data from the default file:\n  %s\n\n"
                            + "This will DELETE all %s data before the import.\n\n",
                            HydroConstants.GEOAREA_DATANAMES[listCbo
                                    .getSelectionIndex()], importFile,
                            HydroConstants.GEOAREA_DATANAMES[listCbo
                                    .getSelectionIndex()]);
        } else {
            msg = String
                    .format("Importing %s data from a user-specified file:\n  %s\n\n"
                            + "This will DELETE all %s data before the import.\n\n",
                            HydroConstants.GEOAREA_DATANAMES[listCbo
                                    .getSelectionIndex()], importFile,
                            HydroConstants.GEOAREA_DATANAMES[listCbo
                                    .getSelectionIndex()]);

            msg.concat("(Note: Occasionally save backup file copies of the data\n"
                    + "       to user-specified file - i.e. not the default filename.)\n\n");

            msg.concat("Are you sure you wish to import the data?");

        }
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Geo Area Import");
        mb.setMessage(msg);
        int choice = mb.open();

        if (choice == SWT.OK) {
            try {
                importGeoArea();
                loadAreaList(selectedType);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error importing Geo Data for file:  "
                                + getAreaFilename().getAbsolutePath(), e);
            }
        }

        shell.setCursor(null);
    }

    /**
     * Get the file for the selected file type.
     * 
     * @return The file object, null if file not found
     */
    private File getAreaFilename() {
        File file = null;
        String fullPath = null;
        int status = 1;

        /* get the value of the filename */
        String filename = sourceDataFileTF.getText();

        /* check if the name is the default for this data set */
        if (filename.equals(GEOAREA_FILENAMES[listCbo.getSelectionIndex()])) {
            defaultMatch = true;
        }

        /*
         * now append the directory onto the filename. insist that the directory
         * be defined
         */
        String dir = AppsDefaults.getInstance().getToken("whfs_geodata_dir");
        if (dir.length() <= 0) {
            status = -1;
        } else {
            fullPath = dir + File.separator + filename;

            file = new File(fullPath);
            if (!file.exists()) {
                status = -1;
            }
        }

        /* if could not access file, then issue message */
        if (status < 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Access Warning");
            if (defaultMatch) {
                mb.setMessage("Could not access default "
                        + HydroConstants.GEOAREA_DATANAMES[listCbo
                                .getSelectionIndex()] + " file:  " + fullPath);
            } else {
                mb.setMessage("Could not access user-specified "
                        + HydroConstants.GEOAREA_DATANAMES[listCbo
                                .getSelectionIndex()] + " file:  " + fullPath);
            }
            mb.open();

            return null;
        }

        return file;
    }

    /**
     * Load the log file into the text editor for viewing.
     */
    private void loadGeoAreaLog() {
        /* Get the file and load the contents into the text editor */
        File f = getLogFilename();

        if (f.exists()) {
            TextEditorDlg ted = textEditorDlgMap.get(f);
            if (ted == null || ted.isDisposed()) {
                ted = new TextEditorDlg(shell, true, f);
                ted.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof File) {
                            File f = (File) returnValue;
                            textEditorDlgMap.remove(f);
                        }
                    }
                });
                ted.open();
                textEditorDlgMap.put(f, ted);
            } else {
                ted.bringToTop();
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Not Found");
            mb.setMessage("Could not access log file:  " + f.getAbsolutePath());
            mb.open();
        }
    }

    /**
     * Get the log filename
     * 
     * @return The File object associated with the log file
     */
    private File getLogFilename() {
        /* build the file name */
        String logfilename = null;
        String dir = AppsDefaults.getInstance().getToken("whfs_util_log_dir");
        if (dir.length() <= 0) {
            logfilename = "whfs_util_log_dir undefined.";
        } else {
            logfilename = dir + File.separator
                    + IMPORT_LOGS[selectedType.ordinal()];
        }

        File f = new File(logfilename);

        return f;
    }

    /**
     * Import the data from the file.
     * 
     * <pre>
     * &lt;id&gt; &lt;name&gt; &lt;feature rank&gt; &lt;numpoints&gt; [center lat] [center lon] 
     * &lt;lat&gt; &lt;lon&gt; 
     * &lt;lat&gt; &lt;lon&gt; 
     * &lt;lat&gt; &lt;lon&gt; 
     * &lt;lat&gt; &lt;lon&gt; 
     * &lt;lat&gt; &lt;lon&gt; 
     * ... ... 
     * &lt;lat&gt; &lt;lon&gt;
     * 
     * where id is the 1-8 character id of the geoarea or geoline 
     * name is the name of the geoarea or geoline. It may be up to 
     *      20 characters long for a geoline and up to 40 characters 
     *      long for a geoarea. 
     * feature rank is the order of the geoarea or geoline. This allows 
     *      geographic features to be displayed according to relative 
     *      importance. Lower numbers take precedence over higher numbers. 
     * numpoints is the number of latitude/longitude pairs defining the 
     *      geoarea or geoline. 
     * center lat is the centroid latitude.  This applies only to geoarea polygons. 
     * center lon is the centroid longitude. This applies only to geoarea polygons.
     * </pre>
     */
    private void importGeoArea() {
        GeoDataManager dman = GeoDataManager.getInstance();
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        openLogFile();
        log("=====================================");
        log("Starting import of "
                + HydroConstants.GEOAREA_DATANAMES[selectedType.ordinal()]
                + " on " + HydroConstants.DATE_FORMAT.format(now));
        log("Loading "
                + HydroConstants.GEOAREA_DATANAMES[selectedType.ordinal()]
                + " from file " + getAreaFilename());

        boolean saveDataBlock = true;
        int linenum = 0;
        int nPts = HydroConstants.UNASSIGNED;
        double intLat = HydroConstants.UNASSIGNED;
        double intLon = HydroConstants.UNASSIGNED;
        java.util.List<GeoAreaData> geoDataList = new ArrayList<GeoAreaData>();

        // Open the file for reading
        File f = getAreaFilename();
        try {
            BufferedReader in = new BufferedReader(new FileReader(f));
            String line = null;
            while ((line = in.readLine()) != null) {
                saveDataBlock = true;
                linenum++;

                // Process the header line

                /*
                 * extract each of the attributes from the header block for the
                 * subsequent set of points
                 */

                /* allow the line to have the interior lat-lon at the end. */
                String str = line;

                /* Remove any excess whitespace. */
                str = str.trim();
                str = str.replaceAll("\\s{2,}", " ");

                String[] parts = str.split(" ");
                int numParts = parts.length;

                /*
                 * Check the last 2 items for a decimal point. If a decimal
                 * point is found then assume this line has two lat/lon values
                 * at the end.
                 */
                if (parts[numParts - 2].contains(".")
                        && parts[numParts - 1].contains(".")) {
                    intLat = Double.parseDouble(parts[numParts - 2]);
                    intLon = Double.parseDouble(parts[numParts - 1]);

                    if ((intLat < 0) || (intLat > 90)) {
                        log("ERROR:  invalid interior " + intLat
                                + " lat in line " + linenum + ":  " + line);
                        saveDataBlock = false;
                    }

                    if ((intLon < -180) || (intLon > 180)) {
                        log("ERROR:  invalid interior " + intLon
                                + " lon in line " + linenum + ":  " + line);
                        saveDataBlock = false;
                    }
                }

                /*
                 * get the number of lat-lon pairs that follow, from the end of
                 * the line
                 */
                nPts = Integer.parseInt(parts[numParts - 3]);
                double[] lonPoints = new double[nPts];
                double[] latPoints = new double[nPts];

                /*
                 * get the stream order, which is not always specified, from the
                 * field preceding the num of lat-lon pairs
                 */
                int streamOrder = Integer.parseInt(parts[numParts - 4]);

                if ((streamOrder < -1) || (streamOrder > 50)) {
                    log("WARNING: Error reading stream order in line "
                            + linenum + ": " + line);
                }

                /* now get the identifier at the beginning of the string */
                String id = parts[0];

                /* get the identifying name */
                StringBuilder name = new StringBuilder();
                for (int j = 1; j <= numParts - 5; j++) {
                    name.append(parts[j] + " ");
                }

                name.trimToSize();

                String nameString = null;
                if (name.length() > LOC_AREANAME_LEN) {
                    log(String
                            .format("WARNING: truncated name (use 1-%d chars) in line %d: %s",
                                    LOC_AREANAME_LEN, linenum, line));

                    name.substring(0, 40);
                } else if (name.length() <= 0) {
                    log(String
                            .format("WARNING: invalid name (use 1-%d chars) in line %d: %s",
                                    LOC_AREANAME_LEN, linenum, line));

                    nameString = "UNDEFINED";
                } else {
                    nameString = name.toString();
                }

                for (int i = 0; i < nPts; i++) {
                    line = in.readLine();
                    line = line.trim();
                    if (line == null) {
                        log("ERROR: Unexpected end-of-file reached after line "
                                + linenum);
                        closeLogFile();
                        in.close();
                        return;
                    }

                    linenum++;

                    /* Extract the latitude and longitude values. */
                    String[] latlon = line.split("\\s+");
                    if (latlon.length != 2) {
                        log("ERROR finding a latitude/longitude pair");
                        log("for id " + parts[0] + " in line " + linenum + ": "
                                + line);
                        log("(line " + (i + 1) + " of block)");
                        saveDataBlock = false;
                    } else {
                        double lat = Double.parseDouble(latlon[0]);
                        double lon = Double.parseDouble(latlon[1]);
                        lon *= -1;
                        /* Test the bounds of the longitude value. */
                        if ((lon < -180) || (lon > 180)) {
                            log("ERROR reading or invalid lon for id "
                                    + parts[0] + " in line " + (i + 1) + ": "
                                    + line);
                            log("(line " + (i + 1) + " of block)");
                            saveDataBlock = false;
                        }

                        lonPoints[i] = lon;

                        /* Test the bounds of the latitude value */
                        if ((lat < 0) || (lat > 90)) {
                            log("ERROR reading or invalid lat for id "
                                    + parts[0] + " in line " + (i + 1) + ": "
                                    + line);
                            log("(line " + (i + 1) + " of block)");
                            saveDataBlock = false;
                        }

                        latPoints[i] = lat;
                    }
                }

                GeoAreaData geoData = new GeoAreaData();
                geoData.setAreaId(id);
                geoData.setName(nameString);
                geoData.setBoundaryType(HydroConstants.GEOAREA_DATANAMES[listCbo
                        .getSelectionIndex()]);
                geoData.setInteriorLat(intLat);
                geoData.setInteriorLon(intLon);
                geoData.setLon(lonPoints);
                geoData.setLat(latPoints);
                geoData.setNumberPoints(nPts);
                geoData.setSaveDataBlock(saveDataBlock);
                geoDataList.add(geoData);
            } // end while ((line = in.readLine()) != null)

            in.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        if (geoDataList.size() <= 0) {
            log("ERROR: Could not read data for geotype "
                    + HydroConstants.GEOAREA_DATANAMES[listCbo
                            .getSelectionIndex()]);
        } else {
            /*
             * good data read in from file, Delete the information from the
             * GeoArea and LineSegs table for the boundary type being processed.
             */
            if (selectedType != ArealTypeSelection.RESERVOIRS) {

                try {
                    int numSegDel = dman
                            .deleteLineSegs(HydroConstants.GEOAREA_DATANAMES[selectedType
                                    .ordinal()]);
                    log("Deleting LineSegs using query: DELETE FROM LineSegs WHERE"
                            + " area_id IN (SELECT area_id FROM GeoArea WHERE boundary_type='"
                            + HydroConstants.GEOAREA_DATANAMES[selectedType
                                    .ordinal()] + "');");

                    log("DELETE " + numSegDel);
                } catch (VizException e) {
                    e.printStackTrace();
                    log("Could not delete rows corresponding to GeoArea type");
                    log(selectedType.toString()
                            + " from the LineSegs database table.");
                    closeLogFile();
                    return;
                }
            }

            try {
                int numGeoDel = dman
                        .deleteGeoArea(HydroConstants.GEOAREA_DATANAMES[selectedType
                                .ordinal()]);
                log("Deleting GeoArea info using query: DELETE FROM GeoArea WHERE"
                        + " boundary_type='"
                        + HydroConstants.GEOAREA_DATANAMES[selectedType
                                .ordinal()] + "';");

                log("DELETE " + numGeoDel);
            } catch (VizException e) {
                e.printStackTrace();
                log("Could not delete rows from the GeoArea table");
                log("corresponding to boundary_type " + selectedType.toString());
                closeLogFile();
                return;
            }

            for (GeoAreaData data : geoDataList) {
                /* load the data into the database if good data */
                if (data.isSaveDataBlock()) {
                    try {
                        int status = dman.putGeoArea(data);
                        if (status < 0) {
                            throw new VizException();
                        }
                    } catch (VizException e) {
                        e.printStackTrace();
                        log("ERROR:  Database write failed for "
                                + data.getAreaId());
                    }

                } else {
                    log("ERROR: Discarding block of data for id "
                            + data.getAreaId() + " due to errors");
                }
            }

            // Load the linesegs table

            if (selectedType != ArealTypeSelection.RESERVOIRS) {
                for (GeoAreaData data : geoDataList) {
                    /* do the main processing */
                    HrapBinList binList = getHrapBinListForArea(data);
                    log("Processing area " + data.getAreaId() + ":"
                            + "  Writing " + binList.getNumRows() + "rows");
                    dman.putLineSegs(data.getAreaId(), binList);
                }
            }
        }

        now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        log("Import completed on " + HydroConstants.DATE_FORMAT.format(now));

        closeLogFile();
    }

    /**
     * This function uses areaId to search the database for the polygon
     * associated with the area. Then it fills the HrapBinList structure by
     * finding all the HRAP bins whose centers are inside the area.
     * 
     * @param data
     *            The GeoAreaData
     * @return The HrapBinList
     */
    private HrapBinList getHrapBinListForArea(GeoAreaData data) {
        HrapBinList binList = new HrapBinList();

        ArrayList<Coordinate> points = getPointsFromArea(data);

        java.util.List<LineSegment> segments = LineSegmentUtil
                .getSegmentsFromPoints(points);

        binList = LineSegmentUtil.getHrapBinListFromSegments(segments);

        return binList;
    }

    /**
     * Creates an array of points from the information pointed to by the GeoArea
     * pointer. Ensures that (1) the last point is the same as the first and (2)
     * that if n points in a row are the same in the database, only one point is
     * propagated to the points array
     * 
     * @param data
     *            The GeoAreaData object
     */
    private ArrayList<Coordinate> getPointsFromArea(GeoAreaData data) {
        ArrayList<Coordinate> points = new ArrayList<Coordinate>();

        /* init the first point */
        Coordinate coord = new Coordinate(data.getLon()[0], data.getLat()[0]);
        points.add(coord);
        double[] lat = data.getLat();
        double[] lon = data.getLon();

        /*
         * for each input point from the database, starting with the second
         * point
         */
        for (int i = 1; i < data.getNumberPoints(); i++) {

            /* if input points are different */
            if ((lat[i] != lat[i - 1]) || (lon[i] != lon[i - 1])) {
                coord = new Coordinate(lon[i], lat[i]);
                points.add(coord);
            }
        }

        /*
         * if the first point and the last point are not the same, add a final
         * point that is the same as the first
         */
        if (!LineSegmentUtil.pointsEqual(points.get(0),
                points.get(points.size() - 1))) {
            coord = new Coordinate(lon[0], lat[0]);
            points.add(coord);
        }

        return points;
    }

    /**
     * Open the log file
     */
    private void openLogFile() {

        logFile = getLogFilename();

        try {
            logger = new BufferedWriter(new FileWriter(logFile));
            logFileOpen = true;
        } catch (IOException e) {
            logFileOpen = false;
        }
    }

    /**
     * Close the log file
     */
    private void closeLogFile() {
        try {
            if (logFileOpen) {
                logger.flush();
                logger.close();
            }
            logFileOpen = false;
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Send a message to the log file
     * 
     * @param stmt
     *            A message to be logged
     */
    private void log(String stmt) {
        if (!logFileOpen) {
            openLogFile();
        }

        if (logFileOpen) {
            try {
                logger.write(stmt + "\n");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
