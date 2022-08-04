/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.viz.hydrobase.dialogs;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.areal.ArealDataImportRequest;
import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.hydro.areal.GeoAreaData;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.hydrocommon.datamanager.GeoDataManager;
import com.raytheon.viz.hydrocommon.texteditor.TextEditorDlg;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * This class contains common functionality to display the Areal Definitions dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Mar 11, 2020 19533      mgamazaychikov Initial creation
 *                                        Extracted common functionality from ArealDefinitionsDlg
 *
 * </pre>
 *
 * @author mgamazaychikov
 *
 */
public abstract class AbstractArealDefinitionsDlg extends CaveSWTDialog implements INotificationObserver {

    protected static final String IMPORT_NOTIFICATION_URI = "edex.areal.data.import";

    private static final String WHFS_GEODATA_DIR = "whfs_geodata_dir";

    private static final String GEOAREA_FORMAT = "%-12S %-40S %-10S %-10S";

    private static final String TOP_LABEL_FORMAT = "%65S";

    private static final String INTERIOR = "INTERIOR";

    private static final String AREA_ID = "AREA ID";

    private static final String NAME = "NAME";

    private static final String LAT = "LAT";

    private static final String LON = "LON";

    protected final IUFStatusHandler statusHandler;

    /**
     * Area list control.
     */
    protected List areaList;

    /**
     * Font used with the controls.
     */
    private Font controlFont;

    /**
     * List combo box.
     */
    protected Combo listCbo;

    /**
     * Source data file text control.
     */
    protected Text sourceDataFileTF;

    /**
     * Is the filename a default or user-specified file
     */
    protected boolean defaultMatch = false;

    /** The selected type */
    protected ArealTypeSelection selectedType;

    /** Allow single instance of editor for a given file. */
    final Map<File, TextEditorDlg> textEditorDlgMap = new HashMap<>();

    private String arealImportType;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     */
    public AbstractArealDefinitionsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.statusHandler = getStatusHandler();
        this.arealImportType = getArealImportType();
        setText("Areal Definitions Source: " + this.arealImportType);
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
        removeObserver();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        initializeComponents();
    }

    /**
     * Create the area list control.
     */
    void createAreaListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite areaComp = new Composite(shell, SWT.NONE);
        areaComp.setLayout(new GridLayout(1, false));
        areaComp.setLayoutData(gd);

        Label topLbl = new Label(areaComp, SWT.NONE);
        topLbl.setText(getAreaListTopText());
        topLbl.setFont(controlFont);

        Label bottomLbl = new Label(areaComp, SWT.NONE);
        bottomLbl.setText(getAreaListBottomText());
        bottomLbl.setFont(controlFont);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        gd.widthHint = 600;
        gd.heightHint = 250;
        areaList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        areaList.setLayoutData(gd);
        areaList.setFont(controlFont);
    }

    /**
     * Get the top label text for the area list label.
     *
     * @return Label text.
     */
    private String getAreaListTopText() {
        return String.format(TOP_LABEL_FORMAT, INTERIOR);
    }

    /**
     * Get the bottom label text for the area list label.
     *
     * @return Label text.
     */
    private String getAreaListBottomText() {
        return String.format(GEOAREA_FORMAT, AREA_ID, NAME, LAT, LON);

    }

    /**
     * Create the OK button at the bottom of the display.
     */
    void createOkButton() {
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
                removeObserver();
                shell.dispose();
            }
        });
    }

    void handleImport() {
        shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        String importFile = getAreaFilename();

        /*
         * if the file was not accessible don't continue with the import
         */
        if (importFile == null) {
            shell.setCursor(null);
            return;
        }

        String msg = null;
        /* confirm import of info */
        String dataName = selectedType.getDataName();
        if (defaultMatch) {
            msg = String.format("Importing %s data from the default file:\n  %s\n\n"
                    + "This will DELETE all %s data before the import.\n\n", dataName, importFile, dataName);
        } else {
            msg = String.format("Importing %s data from a user-specified file:\n  %s\n\n"
                    + "This will DELETE all %s data before the import.\n\n", dataName, importFile, dataName);

            msg += "(Note: Occasionally save backup file copies of the data\n"
                    + "       to user-specified file - i.e. not the default filename.)\n\n";

            msg += "Are you sure you wish to import the data?";
        }

        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
        mb.setText("Geo Area Import");
        mb.setMessage(msg);
        int choice = mb.open();

        if (choice == SWT.OK) {
            handleImportOK(importFile);
        }

        shell.setCursor(null);
    }

    /**
     * Load the log file into the text editor for viewing.
     */
    void loadGeoAreaLog() {
        /* Get the file and load the contents into the text editor */
        File f = getLogFilename();

        if (f.exists()) {
            TextEditorDlg ted = textEditorDlgMap.get(f);
            if (ted == null || ted.isDisposed()) {
                ted = new TextEditorDlg(shell, true, f);
                ted.addCloseCallback(new ICloseCallback() {

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
        SimpleDateFormat sdLog = new SimpleDateFormat("yyyyMMdd");
        Date now = TimeUtil.newGmtCalendar().getTime();
        String ext = "_" + sdLog.format(now);
        String logfilename = null;
        String dir = AppsDefaults.getInstance().getToken("whfs_util_log_dir");
        if (dir.length() <= 0) {
            logfilename = "whfs_util_log_dir undefined.";
        } else {
            logfilename = dir + File.separator + CommonHydroConstants.IMPORT_LOGS[selectedType.ordinal()] + ext;
        }

        File f = new File(logfilename);

        return f;
    }

    /**
     * Get the file for the selected file type.
     *
     * @return The file object, null if file not found
     */
    protected String getAreaFilename() {
        String fullPath = null;

        /* get the value of the filename */
        String filename = sourceDataFileTF.getText();

        defaultMatch = isDefaultMatch(filename);
        /*
         * now append the directory onto the filename. Insist that the directory
         * be defined
         */
        String dir = AppsDefaults.getInstance().getToken(WHFS_GEODATA_DIR);
        if (dir != null && !dir.isEmpty()) {
            fullPath = dir + File.separator + filename;
            File ff = new File(fullPath);
            boolean exists = ff.exists();
            if (!exists) {
                statusHandler.handle(Priority.ERROR, "Unable to find specified file: " + fullPath);
             return null;
             }
        } else {
            /* if could not access file, then issue message */
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Access Warning");
            String dataName = selectedType.getDataName();
            if (defaultMatch) {
                mb.setMessage("Could not access default " + dataName + " file. " + "Check the " + WHFS_GEODATA_DIR
                        + " token in apps_defaults.");
            } else {
                mb.setMessage("Could not access user-specified " + dataName + " file. Check the " + WHFS_GEODATA_DIR
                        + " token in apps_defaults.");
            }
            mb.open();

            return null;
        }

        return fullPath;
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
    void importGeoArea() {
        String fileName = getAreaFilename();
        ArealDataImportRequest request = new ArealDataImportRequest(fileName, arealImportType, selectedType);
        try {
            Object obj = ThriftClient.sendRequest(request);
            if (obj instanceof ServerErrorResponse) {
                MessageBox messageBox2 = new MessageBox(shell, SWT.OK);
                messageBox2.setText("Geo Area Import");
                messageBox2.setMessage("Unable to complete Geo Area Import operation: Server Error");
                messageBox2.open();
            }
        } catch (VizException e1) {
            statusHandler.handle(Priority.ERROR, "Unable to complete Geo Area Import operation.", e1);
        }
    }

    void handleTypeSelection() {
        selectedType = (ArealTypeSelection) listCbo.getData(listCbo.getText());

        /* reload the list */
        try {
            loadAreaList(selectedType);
            loadAreaImport(selectedType);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error updating Geo Area List", e);
        }

    }

    void loadAreaList(ArealTypeSelection type) throws VizException {
        /* load the list of geo areas from the database */
        System.out.println("loadAreaList called");
        GeoDataManager dman = GeoDataManager.getInstance();

        java.util.List<GeoAreaData> areaDataList = dman.getGeoArea(type);

        /* Clear the list */
        areaList.removeAll();

        /* Populate the list */
        GeoUtil geoUtils = GeoUtil.getInstance();
        for (GeoAreaData row : areaDataList) {
            String lat = geoUtils.cvt_latlon_from_double(row.getInteriorLat());
            String lon = geoUtils.cvt_latlon_from_double(row.getInteriorLon());
            areaList.add(String.format(GEOAREA_FORMAT, row.getAreaId().trim(), row.getName().trim(), lat.trim(),
                    lon.trim()));
        }

    }

    abstract protected void initializeComponents();

    abstract protected void createListComboControl();

    abstract protected void createImportGroup();

    abstract protected IUFStatusHandler getStatusHandler();

    abstract protected String getArealImportType();

    abstract protected void loadAreaImport(ArealTypeSelection type);

    abstract protected void handleImportOK(String importFile);

    abstract protected boolean isDefaultMatch(String name);

    abstract protected void addObserver();

    abstract protected void removeObserver();

}
