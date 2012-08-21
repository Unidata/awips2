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
package com.raytheon.viz.awipstools.ui.dialog;

/**
 * Creates the Put Cursor Home Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Dec 5 2007		       Eric Babin   Initial Creation
 *  10Dec2007   #598       Eric Babin   Added city, state query.
 *  20Dec2007   #656       Eric Babin   Updated to refresh location after clicking GO.
 *  15Jan2007                ebabin     Update for lat/lon put home cursor bug.
 *  10-21-09    #1049       bsteffen    Synchronize with new Home Tool Layer
 *  07-09-10    #3654      bkowal       The stationid column will now be used in the
 *                                      station query instead of the icao column.
 *  07-21-10    #3654      bkowal       Added additional logic to increase the likelihood
 *                                      that we will get enough information to fill all
 *                                      of the fields on the interface for the query by
 *                                      station id and the query by city, state.                                    

 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

// TODO: the code that performs the queries will need to be cleaned up once we have
//       time to cleanup, complete the common_obs_spatial table
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKTWriter;

public class PutHomeCursorDialog extends CaveJFACEDialog implements
        IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PutHomeCursorDialog.class);

    private static final String DIST_QRY_FMT = "SELECT icao, name, state FROM common_obs_spatial "
            + "WHERE ST_DWithin(the_geom, ST_GeomFromText('%s'), %4.1f) AND"
            + " catalogtype = %d ORDER BY ST_Distance(the_geom, ST_GeomFromText('%1$s'))";

    private static final String ICAO_QRY_FMT = "SELECT AsBinary(the_geom) FROM common_obs_spatial "
            + "WHERE catalogtype = %d and icao = '%s'";

    private static final String CITY_QRY_FMT = "SELECT AsBinary(the_geom) FROM common_obs_spatial "
            + "WHERE catalogtype = %d AND name = '%s' AND state = '%s'";

    private static final String CITY_DB_QRY_FMT = "SELECT AsBinary(the_geom) FROM mapdata.city "
            + "WHERE name = '%s' AND st = '%s'";

    private static final String AIRPORT_DB_QRY_FMT = "SELECT AsBinary(the_geom) FROM mapdata.airport "
            + "WHERE city = '%s' AND state = '%s'";

    private Composite top = null;

    private Button stationRadio;

    private Button cityRadio;

    private Button latLonRadio;

    private Text stationTextField;

    private Text cityTextField;

    private Text stateTextField;

    private Text latTextField;

    private Text lonTextField;

    private Button goButton, closeButton;

    private final AbstractResourceData resourceData;

    public PutHomeCursorDialog(Shell shell,
            AbstractResourceData abstractResourceData) {
        super(shell);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.resourceData = abstractResourceData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        // Create the main layout for the shell.
        GridLayout gridLayout = new GridLayout(1, true);

        top.setLayout(gridLayout);

        // Initialize all of the menus, controls, and layouts
        initializeComponents();

        return top;
    }

    /**
     * Initializes the components.
     */
    private void initializeComponents() {
        createSelectionChoices();
        createStationChoice();
        createCityChoice();
        createLatLonChoice();
        try {
            updateStationInfo(PointsDataManager.getInstance().getHome());
        } catch (Exception e) {

        }
        // @TODO Not currently no data for city, state. So disabled for now.
        stationRadio.setSelection(true);
        cityTextField.setEnabled(false);
        stateTextField.setEnabled(false);
        latTextField.setEnabled(false);
        lonTextField.setEnabled(false);
        stationTextField.setEnabled(true);
    }

    /**
     * Creates the selection composite.
     */
    private void createSelectionChoices() {
        Group selectionGroup = new Group(top, SWT.NONE | SWT.FILL);

        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.grabExcessHorizontalSpace = true;
        selectionGroup.setLayoutData(gridData);

        RowLayout rowLayout = new RowLayout();
        rowLayout.type = SWT.VERTICAL;
        selectionGroup.setLayout(rowLayout);

        Label label = new Label(selectionGroup, SWT.NONE | SWT.CENTER);
        label.setText("Location selection via:");

        Composite locationComp = new Composite(selectionGroup, SWT.None);
        locationComp.setLayout(new RowLayout());
        stationRadio = new Button(locationComp, SWT.RADIO);
        stationRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cityTextField.setEnabled(false);
                stateTextField.setEnabled(false);
                latTextField.setEnabled(false);
                lonTextField.setEnabled(false);
                stationTextField.setEnabled(true);
            }
        });

        stationRadio.setText("Station");
        cityRadio = new Button(locationComp, SWT.RADIO);
        cityRadio.setText("City/State");
        cityRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cityTextField.setEnabled(true);
                stateTextField.setEnabled(true);
                latTextField.setEnabled(false);
                lonTextField.setEnabled(false);
                stationTextField.setEnabled(false);
            }
        });

        latLonRadio = new Button(locationComp, SWT.RADIO);
        latLonRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cityTextField.setEnabled(false);
                stateTextField.setEnabled(false);
                latTextField.setEnabled(true);
                lonTextField.setEnabled(true);
                stationTextField.setEnabled(false);
            }
        });

        latLonRadio.setText("Lat/Lon");
    }

    /**
     * Creates station choice composite.
     */
    private void createStationChoice() {
        Group comp = new Group(top, SWT.None);
        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;

        comp.setLayoutData(gridData);
        comp.setLayout(new GridLayout(3, true));
        Label label = new Label(comp, SWT.NONE);
        label.setText("Station:");
        stationTextField = new Text(comp, SWT.BORDER);
        GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL);
        data.horizontalSpan = 2;
        stationTextField.setLayoutData(data);
    }

    /**
     * Creates city choice composite.
     */
    private void createCityChoice() {
        Group comp = new Group(top, SWT.None);
        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.grabExcessHorizontalSpace = true;
        comp.setLayoutData(gridData);
        comp.setLayout(new GridLayout(3, true));

        Label citylabel = new Label(comp, SWT.NONE);
        citylabel.setText("City:");
        cityTextField = new Text(comp, SWT.BORDER);
        GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL);
        data.horizontalSpan = 2;
        cityTextField.setLayoutData(data);

        Label stateLabel = new Label(comp, SWT.NONE);
        stateLabel.setText("State:");
        stateLabel.setToolTipText("Two Letter Abbreviation");

        stateTextField = new Text(comp, SWT.BORDER);
        stateTextField.setTextLimit(2);

        stateTextField.setEnabled(false);
        cityTextField.setEnabled(false);
    }

    /**
     * Creates lat/lon choice composite.
     */
    private void createLatLonChoice() {
        Group comp = new Group(top, SWT.None);
        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.grabExcessHorizontalSpace = true;
        comp.setLayoutData(gridData);
        comp.setLayout(new GridLayout(2, true));
        Label latLabel = new Label(comp, SWT.NONE);
        latLabel.setText("Latitude:");
        latTextField = new Text(comp, SWT.BORDER);
        latTextField.setLayoutData(new GridData(80, SWT.DEFAULT));
        Label longLabel = new Label(comp, SWT.NONE);
        longLabel.setText("Longitude:");
        lonTextField = new Text(comp, SWT.BORDER);
        lonTextField.setLayoutData(new GridData(80, SWT.DEFAULT));
        Coordinate point = PointsDataManager.getInstance().getHome();
        lonTextField.setText(String.valueOf(point.x));
        latTextField.setText(String.valueOf(point.y));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        resourceData.addChangeListener(this);
        shell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                resourceData.removeChangeListener(PutHomeCursorDialog.this);
            }
        });
        shell.setText("Put Home Cursor");

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected Point getInitialSize() {
        return new Point(300, 360);
    }

    /**
     * Exits the dialog.
     */
    private void exitDialog() {
        this.resourceData.removeChangeListener(this);
        this.close();
    }

    /**
     * Updates the station.
     */
    private void updateStation() {

        if (stationRadio.getSelection()) {
            if (stationTextField.getText().length() > 0) {
                String station = stationTextField.getText().toUpperCase()
                        .trim();
                Coordinate c = runStationQuery(station);
                if (c == null) {
                    MessageDialog.openError(getParentShell(),
                            "Put Home Cursor Error",
                            "Could not find that Metar station");
                    stationTextField.setFocus();
                } else {
                    PointsDataManager.getInstance().setHome(c);
                    resourceData.fireChangeListeners(ChangeType.DATA_UPDATE,
                            null);
                    stationTextField.setText(station);
                }
            } else {
                MessageDialog.openError(getParentShell(),
                        "Put Home Cursor Error",
                        "The input for the Station is empty.  Please correct.");
                stationTextField.setFocus();
            }
        } else if (cityRadio.getSelection()) {
            if (cityTextField.getText().isEmpty()) {
                MessageDialog.openError(getParentShell(),
                        "Put Home Cursor Error",
                        "The input for the City is empty.  Please correct.");
                cityTextField.setFocus();
                return;
            }
            if (stateTextField.getText().length() != 2) {
                MessageDialog.openError(getParentShell(),
                        "Put Home Cursor Error",
                        "The input for the State is invalid.  Please correct.");
                stateTextField.setFocus();
                return;
            }
            String city = cityTextField.getText().toUpperCase().trim();
            String state = stateTextField.getText().toUpperCase().trim();
            Coordinate c = runCityQuery(city, state);
            if (c == null) {
                MessageDialog.openError(getParentShell(),
                        "Put Home Cursor Error", "Could not find that city");
                cityTextField.setFocus();
            } else {
                PointsDataManager.getInstance().setHome(c);
                resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, null);
                cityTextField.setText(city);
                stateTextField.setText(state);
            }
        } else if (latLonRadio.getSelection()) {
            Coordinate c = new Coordinate();
            try {
                c.x = Float.parseFloat(lonTextField.getText());
            } catch (NumberFormatException nfe) {
                MessageDialog
                        .openError(getParentShell(), "Put Home Cursor Error",
                                "The input for the Longitude not a number.  Please correct.");
                lonTextField.setFocus();
                return;
            }
            try {
                c.y = Float.parseFloat(latTextField.getText());
                if ((c.y < -180) || c.y > 180) {
                    MessageDialog
                            .openError(new Shell(), "Put Home Cursor Error",
                                    "Latitude must be between -90 and 90.  Please correct.");

                    latTextField.setFocus();
                    return;
                }
            } catch (NumberFormatException nfe) {
                MessageDialog
                        .openError(new Shell(), "Put Home Cursor Error",
                                "The input for the Latitude not a number.  Please correct.");
                latTextField.setFocus();
                return;
            }
            PointsDataManager.getInstance().setHome(c);
            resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, null);
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        // override, so can add calculate as default button.
        goButton = createButton(parent, SWT.OK, "Go", true);
        closeButton = createButton(parent, SWT.CLOSE, "Close", false);
        goButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                updateStation();
            }
        });
        closeButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                exitDialog();
            }

        });

    }

    /**
     * a generic function that will execute a provided sql query and return the
     * results.
     * 
     * @param sql
     *            the sql statement to execute
     * @param database
     *            the name of the database to use
     * @return the records that were retrieved if successful or NULL if the
     *         query failed
     */
    private List<Object[]> executeSQLQuery(String sql, String database) {
        try {
            return DirectDbQuery.executeQuery(sql, database,
                    DirectDbQuery.QueryLanguage.SQL);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "", e);
            return null;
        }
    }

    /**
     * takes the result of a query for a geometry and parses out the coordinate.
     * 
     * @param data
     * @return
     */
    private Coordinate getCoordinate(List<Object[]> data) {
        if (data == null || data.isEmpty()) {
            return null;
        }
        Object[] dataArray = data.get(0);
        if (dataArray == null || dataArray.length != 1) {
            return null;
        }
        byte[] bytes = (byte[]) dataArray[0];
        WKBReader reader = new WKBReader();
        Geometry geo = null;
        try {
            geo = reader.read(bytes);
            return geo.getCoordinate();
        } catch (ParseException e) {
            statusHandler.handle(Priority.PROBLEM, "", e);
            return null;
        }
    }

    /**
     * Calls a script for retrieving a metar station.
     * 
     * @return
     * @throws ParseException
     */
    private Coordinate runStationQuery(String stationId) {
        String sql = String.format(ICAO_QRY_FMT, ObStation.CAT_TYPE_ICAO,
                stationId);
        return getCoordinate(executeSQLQuery(sql, "metadata"));
    }

    /**
     * Calls a query for finding a city, states lat/long
     * 
     * @return A coordinate lat/lon of the city, or null if not found.
     */
    private Coordinate runCityQuery(String city, String state) {
        // First check the obs database
        String sql = String.format(CITY_QRY_FMT, ObStation.CAT_TYPE_ICAO, city,
                state);
        Coordinate c = getCoordinate(executeSQLQuery(sql, "metadata"));
        if (c != null) {
            return c;
        }
        // now try the city database
        sql = String.format(CITY_DB_QRY_FMT, city, state);
        c = getCoordinate(executeSQLQuery(sql, "maps"));
        if (c != null) {
            return c;
        }
        // now try the airport database
        sql = String.format(AIRPORT_DB_QRY_FMT, city, state);
        c = getCoordinate(executeSQLQuery(sql, "maps"));
        if (c != null) {
            return c;
        }
        return null;
    }

    /**
     * Updates the station identifier, name, and state information on the dialog
     * using the closest station from a given coordinate.
     * 
     * @param c
     *            the coordinates associated with the city and state that the
     *            user entered
     * @return the station id if found; otherwise NULL
     */
    private void updateStationInfo(Coordinate c) {
        if (c != null) {
            List<Object[]> data = null;

            WKTWriter wktWriter = new WKTWriter();
            GeometryFactory factory = new GeometryFactory();
            String coordAsString = wktWriter.writeFormatted(factory
                    .createPoint(c));
            double distance = 0; // distance in degrees
            final int MAX_DISTANCE = 20;
            boolean found = false;
            while (!found && distance < MAX_DISTANCE) {
                distance += 0.5; // move out at one half degree increments.
                String sql = String.format(DIST_QRY_FMT, coordAsString,
                        distance, ObStation.CAT_TYPE_ICAO);
                data = executeSQLQuery(sql, "metadata");
                found = (data != null && data.size() >= 1);

            } // while
            if (found) {
                String s = (String) data.get(0)[0];
                stationTextField.setText(s.toUpperCase());
                s = (String) data.get(0)[1];
                cityTextField.setText((s != null) ? s.toUpperCase() : "");
                s = (String) data.get(0)[2];
                stateTextField.setText((s != null) ? s.toUpperCase() : "");
            }
        }
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        Coordinate point = PointsDataManager.getInstance().getHome();
        lonTextField.setText(String.valueOf(point.x));
        latTextField.setText(String.valueOf(point.y));
        // find the nearest station and update the fields.
        updateStationInfo(point);
    }

}
