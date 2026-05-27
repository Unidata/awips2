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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKBReader;
import org.locationtech.jts.io.WKTWriter;

/**
 * Creates the Put Cursor Home Dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 05, 2007           Eric Babin  Initial Creation
 * Dec 10, 2007  598      Eric Babin  Added city, state query.
 * Dec 20, 2007  656      Eric Babin  Updated to refresh location after
 *                                    clicking GO.
 * Jan 15, 2007           ebabin      Update for lat/lon put home cursor bug.
 * Oct 21, 2009  1049     bsteffen    Synchronize with new Home Tool Layer
 * Jul 09, 2010  3654     bkowal      The stationid column will now be used in
 *                                    the station query instead of the icao
 *                                    column.
 * Jul 21, 2010  3654     bkowal      Added additional logic to increase the
 *                                    likelihood that we will get enough
 *                                    information to fill all of the fields on
 *                                    the interface for the query by station id
 *                                    and the query by city, state.
 * Sep 03, 2013  2310     bsteffen    Extend IPointChangedListener instead of
 *                                    IResourceDataChanged.
 * Apr 21, 2014  3041     lvenable    Added dispose check to runAsync call and cleaned up
 *                                    code.  Wrote ticket #3047 for common_obs_spatial
 *                                    for the city/state issues.
 * Nov 11, 2014  3401     rferrel     Add Enter key events.
 * Jan 15, 2015  5054     randerso    Remove unnecessary new Shell
 * Jan 16, 2016  DR 11474 A. Rickert  Parsing Lat/Lon with parseDouble for better accuracy
 * Jan 05, 2018  6735     tgurney     Rewrite updateStationInfo and db queries
 *
 * </pre>
 *
 * @author ebabin
 */
public class PutHomeCursorDialog extends CaveSWTDialog
        implements IPointChangedListener {

    /**
     * City, METAR station or airport and its distance (in degrees) away from
     * the home cursor
     */
    private static class NearbyFeature {
        public String city = "";

        public String state = "";

        public String icao = "";

        /** Distance away from specified coordinate. In degrees */
        public double distance = Double.MAX_VALUE;
    }

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String NEAREST_CITY_SQL = "select distance, name, state from ("
            + " select name, state,"
            + " ST_Distance(the_geom, ST_GeomFromText(:coordinate, 4326)) distance"
            + " from mapdata.city) a where distance < 20 "
            + " order by distance asc limit 1";

    private static final String NEAREST_OBSTATION_SQL = "select distance, name, state, icao from ("
            + " select icao, name, state,"
            + " ST_Distance(the_geom, ST_GeomFromText(:coordinate)) distance"
            + " from common_obs_spatial where catalogtype = "
            + ObStation.CAT_TYPE_ICAO + ") a where distance < 20 "
            + " order by distance asc limit 1";

    private static final String NEAREST_AIRPORT_SQL = "select distance, city, state from ("
            + " select city, state,"
            + " ST_Distance(the_geom, ST_GeomFromText(:coordinate, 4326)) distance"
            + " from mapdata.airport) a where distance < 20 "
            + " order by distance asc limit 1";

    private static final String STATION_GEOM_SQL = "select ST_AsBinary(the_geom) "
            + " from common_obs_spatial " + " where catalogtype = "
            + ObStation.CAT_TYPE_ICAO + " and icao = :stationId";

    private static final String CITY_STATION_GEOM_SQL = "select ST_AsBinary(the_geom) "
            + " from common_obs_spatial " + " where catalogtype = "
            + ObStation.CAT_TYPE_ICAO
            + " and upper(name) = :city and state = :state";

    private static final String CITY_MAPS_GEOM_SQL = "select ST_AsBinary(the_geom) "
            + " from mapdata.city "
            + " where upper(name) = :city and st = :state";

    private static final String AIRPORT_GEOM_SQL = "select ST_AsBinary(the_geom) "
            + " from mapdata.airport "
            + " where upper(city) = :city and upper(state) = :state";

    private Button stationRadio;

    private Button cityRadio;

    private Button latLonRadio;

    private Label stationLabel;

    private Text stationTextField;

    private Label citylabel;

    private Text cityTextField;

    private Label stateLabel;

    private Text stateTextField;

    private Label latLabel;

    private Text latTextField;

    private Label lonLabel;

    private Text lonTextField;

    /** verify listener force entry to upper case. */
    private VerifyListener verifyToUpperCase = new VerifyListener() {

        @Override
        public void verifyText(VerifyEvent e) {
            e.text = e.text.toUpperCase();
        }
    };

    /** Performs update when Enter key is pressed. */
    private KeyListener keyListenerUpdate = new KeyAdapter() {
        @Override
        public void keyPressed(KeyEvent e) {
            if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                updateStation();
            }
        }
    };

    public PutHomeCursorDialog(Shell shell) {
        super(shell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);

        setText("Put Home Cursor");
        PointsDataManager.getInstance().addHomeChangedListener(this);
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout gridLayout = new GridLayout(1, false);
        return gridLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return super.constructShellLayoutData();
    }

    @Override
    protected void initializeComponents(Shell shell) {

        Group locSelectionGroup = new Group(shell, SWT.NONE);

        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        locSelectionGroup.setLayoutData(gridData);
        locSelectionGroup.setText("Location selection via:");

        GridLayout rowLayout = new GridLayout(1, false);
        locSelectionGroup.setLayout(rowLayout);

        createSelectionChoices(locSelectionGroup);

        Composite controlsComp = new Composite(locSelectionGroup, SWT.NONE);
        controlsComp.setLayout(new GridLayout(2, false));
        controlsComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        createStationChoice(controlsComp);
        createCityChoice(controlsComp);
        createLatLonChoice(controlsComp);

        createActionButtons();

        Coordinate home = PointsDataManager.getInstance().getHome();
        if (home != null) {
            updateStationInfo(home);
        }

        enableDisableControls();
    }

    @Override
    protected void disposed() {
        PointsDataManager.getInstance()
                .removeHomeChangedListener(PutHomeCursorDialog.this);
    }

    /**
     * Creates the selection composite.
     *
     * @param selectionGroup
     *            Group container.
     */
    private void createSelectionChoices(Group selectionGroup) {

        /*
         * Composite for the location radio buttons.
         */
        Composite locationComp = new Composite(selectionGroup, SWT.BORDER);
        locationComp.setLayout(new GridLayout(3, true));

        stationRadio = new Button(locationComp, SWT.RADIO);
        stationRadio.setText("Station");
        stationRadio.setSelection(true);
        stationRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!stationRadio.getSelection()) {
                    return;
                }
                enableDisableControls();
            }
        });

        cityRadio = new Button(locationComp, SWT.RADIO);
        cityRadio.setText("City/State");
        cityRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!cityRadio.getSelection()) {
                    return;
                }
                enableDisableControls();
            }
        });

        latLonRadio = new Button(locationComp, SWT.RADIO);
        latLonRadio.setText("Lat/Lon");
        latLonRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!latLonRadio.getSelection()) {
                    return;
                }
                enableDisableControls();
            }
        });
    }

    /**
     * Creates station choice controls.
     *
     * @param controlsComp
     *            Composite containing the controls.
     */
    private void createStationChoice(Composite controlsComp) {

        stationLabel = new Label(controlsComp, SWT.NONE);
        stationLabel.setText("Station:");

        stationTextField = new Text(controlsComp, SWT.BORDER);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stationTextField.setLayoutData(data);
        stationTextField.addKeyListener(keyListenerUpdate);
        stationTextField.addVerifyListener(verifyToUpperCase);

        // Add a separator line.
        addSeparator(controlsComp);
    }

    /**
     * Creates city choice controls.
     *
     * @param controlsComp
     *            Composite containing the controls.
     */
    private void createCityChoice(Composite controlsComp) {

        citylabel = new Label(controlsComp, SWT.NONE);
        citylabel.setText("City:");

        cityTextField = new Text(controlsComp, SWT.BORDER);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        cityTextField.setLayoutData(gd);
        cityTextField.addKeyListener(keyListenerUpdate);
        cityTextField.addVerifyListener(verifyToUpperCase);

        stateLabel = new Label(controlsComp, SWT.NONE);
        stateLabel.setText("State:");
        stateLabel.setToolTipText("Two Letter Abbreviation");

        gd = new GridData(50, SWT.DEFAULT);
        stateTextField = new Text(controlsComp, SWT.BORDER);
        stateTextField.setTextLimit(2);
        stateTextField.setLayoutData(gd);
        stateTextField.addKeyListener(keyListenerUpdate);
        stateTextField.addVerifyListener(verifyToUpperCase);

        // Add a separator line.
        addSeparator(controlsComp);
    }

    /**
     * Creates lat/lon choice controls.
     *
     * @param controlsComp
     *            Composite containing the controls.
     */
    private void createLatLonChoice(Composite controlsComp) {

        latLabel = new Label(controlsComp, SWT.NONE);
        latLabel.setText("Latitude:");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        latTextField = new Text(controlsComp, SWT.BORDER);
        latTextField.setLayoutData(new GridData(80, SWT.DEFAULT));
        latTextField.setLayoutData(gd);

        lonLabel = new Label(controlsComp, SWT.NONE);
        lonLabel.setText("Longitude:");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        lonTextField = new Text(controlsComp, SWT.BORDER);
        lonTextField.setLayoutData(new GridData(80, SWT.DEFAULT));
        lonTextField.setLayoutData(gd);
        lonTextField.addKeyListener(keyListenerUpdate);

        Coordinate point = PointsDataManager.getInstance().getHome();
        lonTextField.setText(String.valueOf(point.x));
        latTextField.setText(String.valueOf(point.y));
        latTextField.addKeyListener(keyListenerUpdate);

        // Add a separator line.
        addSeparator(controlsComp);
    }

    /**
     * Add a line separator to the given composite.
     *
     * @param parentComp
     *            Parent composite.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Enable or disable controls based on the radio button states.
     */
    private void enableDisableControls() {
        stationLabel.setEnabled(stationRadio.getSelection());
        stationTextField.setEnabled(stationRadio.getSelection());

        citylabel.setEnabled(cityRadio.getSelection());
        cityTextField.setEnabled(cityRadio.getSelection());
        stateLabel.setEnabled(cityRadio.getSelection());
        stateTextField.setEnabled(cityRadio.getSelection());

        latLabel.setEnabled(latLonRadio.getSelection());
        latTextField.setEnabled(latLonRadio.getSelection());
        lonLabel.setEnabled(latLonRadio.getSelection());
        lonTextField.setEnabled(latLonRadio.getSelection());
    }

    private void updateStation() {

        if (stationRadio.getSelection()) {
            if (stationTextField.getText().length() > 0) {
                String station = stationTextField.getText().toUpperCase()
                        .trim();
                Coordinate c = runStationQuery(station);
                if (c == null) {
                    MessageDialog.openError(shell, "Put Home Cursor Error",
                            "Could not find that METAR station");
                    stationTextField.setFocus();
                } else {
                    PointsDataManager.getInstance().setHome(c);
                    stationTextField.setText(station);
                }
            } else {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "Station cannot be empty.");
                stationTextField.setFocus();
            }
        } else if (cityRadio.getSelection()) {
            if (cityTextField.getText().isEmpty()) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "City cannot be empty.");
                cityTextField.setFocus();
                return;
            }
            if (stateTextField.getText().length() != 2) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "State is invalid (must be two letters).");
                stateTextField.setFocus();
                return;
            }
            String city = cityTextField.getText().toUpperCase().trim();
            String state = stateTextField.getText().toUpperCase().trim();
            Coordinate c = lookupCityLatLon(city, state);
            if (c == null) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "Could not find that city.");
                cityTextField.setFocus();
            } else {
                PointsDataManager.getInstance().setHome(c);
                cityTextField.setText(city);
                stateTextField.setText(state);
            }
        } else if (latLonRadio.getSelection()) {
            Coordinate c = new Coordinate();
            try {
                c.x = Double.parseDouble(lonTextField.getText());
                if (c.x < -180 || c.x > 180.0) {
                    MessageDialog.openError(shell, "Put Home Cursor Error",
                            "Longitude must be a number between -180 and 180.");

                    lonTextField.setFocus();
                    return;
                }
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "Longitude must be a number between -180 and 180.");
                lonTextField.setFocus();
                return;
            }
            try {
                c.y = Double.parseDouble(latTextField.getText());
                /*
                 * Positioning the home cursor exactly on the pole can cause a
                 * projection error. So it's not allowed
                 */
                if (c.y <= -90.0 || c.y >= 90.0) {
                    MessageDialog.openError(shell, "Put Home Cursor Error",
                            "Latitude must be a number greater than -90 and less than 90.");

                    latTextField.setFocus();
                    return;
                }
            } catch (NumberFormatException nfe) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "Latitude must be a number greater than -90 and less than 90.");
                latTextField.setFocus();
                return;
            }
            PointsDataManager.getInstance().setHome(c);
        }
    }

    /**
     * Create the action buttons at the bottom of the dialog.
     */
    private void createActionButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 75;

        // override, so can add calculate as default button.
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button goBtn = new Button(buttonComp, SWT.PUSH);
        goBtn.setText("Go");
        goBtn.setLayoutData(gd);
        goBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateStation();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
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
    private List<Object[]> executeSQLQuery(String sql, String database,
            Map<String, Object> paramMap) {
        try {
            return DirectDbQuery.executeQuery(sql, database,
                    DirectDbQuery.QueryLanguage.SQL, paramMap);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to execute SQL query", e);
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
            statusHandler.handle(Priority.PROBLEM, "Failed to parse geometry",
                    e);
            return null;
        }
    }

    /** Lookup lat/lon for ICAO station ID */
    private Coordinate runStationQuery(String stationId) {
        Map<String, Object> paramMap = new HashMap<>();
        paramMap.put("stationId", stationId);
        return getCoordinate(
                executeSQLQuery(STATION_GEOM_SQL, "metadata", paramMap));
    }

    /**
     * Calls a query for finding a city, states lat/long
     *
     * @return A coordinate lat/lon of the city, or null if not found.
     */
    private Coordinate lookupCityLatLon(String city, String state) {
        Map<String, Object> paramMap = new HashMap<>();
        paramMap.put("city", city);
        paramMap.put("state", state);
        Coordinate c = getCoordinate(
                executeSQLQuery(CITY_STATION_GEOM_SQL, "metadata", paramMap));
        if (c != null) {
            return c;
        }
        c = getCoordinate(
                executeSQLQuery(CITY_MAPS_GEOM_SQL, "maps", paramMap));
        if (c != null) {
            return c;
        }
        c = getCoordinate(executeSQLQuery(AIRPORT_GEOM_SQL, "maps", paramMap));
        if (c != null) {
            return c;
        }
        return null;
    }

    /** @return two-letter state code, or null if not found */
    private String lookupStateCode(String stateName) {
        Map<String, Object> paramMap = new HashMap<>();
        paramMap.put("stateName", stateName.toUpperCase());
        List<Object[]> result = executeSQLQuery(
                "select state from mapdata.states where upper(name) = :stateName",
                "maps", paramMap);
        if (result != null && !result.isEmpty()) {
            return result.get(0)[0].toString();
        }
        return null;
    }

    private NearbyFeature lookupNearestFeature(String sql, String database,
            Map<String, Object> paramMap) {
        NearbyFeature rval = null;
        List<Object[]> result = executeSQLQuery(sql, database, paramMap);
        if (result != null && !result.isEmpty()) {
            Object[] row = result.get(0);
            NearbyFeature nearbyFeature = new NearbyFeature();
            nearbyFeature.distance = Double.parseDouble(row[0].toString());
            if (row[1] != null) {
                nearbyFeature.city = row[1].toString().toUpperCase();
            }
            if (row[2] != null) {
                nearbyFeature.state = row[2].toString().toUpperCase();
            }
            if (row.length == 4 && row[3] != null) {
                nearbyFeature.icao = row[3].toString().toUpperCase();
            }
            if (!nearbyFeature.state.isEmpty()
                    && nearbyFeature.state.length() != 2) {
                String stateCode = lookupStateCode(nearbyFeature.state);
                if (stateCode != null) {
                    nearbyFeature.state = stateCode;
                }
            }
            rval = nearbyFeature;
        }
        return rval;
    }

    /**
     * Updates the station identifier, name, and state information on the dialog
     * using the closest station from a given coordinate.
     *
     * @param c
     *            the coordinates associated with the city and state that the
     *            user entered
     */
    private void updateStationInfo(Coordinate c) {
        WKTWriter wktWriter = new WKTWriter();
        GeometryFactory factory = new GeometryFactory();
        String coordAsString = wktWriter.writeFormatted(factory.createPoint(c));
        Map<String, Object> paramMap = new HashMap<>();
        paramMap.put("coordinate", coordAsString);
        List<NearbyFeature> nearbyLocations = new ArrayList<>();
        String icao = "";
        NearbyFeature icaoResult = lookupNearestFeature(NEAREST_OBSTATION_SQL,
                "metadata", paramMap);
        if (icaoResult != null) {
            if (icaoResult.icao != null) {
                icao = icaoResult.icao;
            }
            nearbyLocations.add(icaoResult);
        }
        nearbyLocations
                .add(lookupNearestFeature(NEAREST_CITY_SQL, "maps", paramMap));
        nearbyLocations.add(
                lookupNearestFeature(NEAREST_AIRPORT_SQL, "maps", paramMap));
        NearbyFeature closest = nearbyLocations.stream()
                .filter(e -> e != null && e.city != null && !e.city.isEmpty())
                .min((e1, e2) -> Double.compare(e1.distance, e2.distance))
                .orElseGet(NearbyFeature::new);
        stationTextField.setText(icao);
        cityTextField.setText(closest.city);
        stateTextField.setText(closest.state);

    }

    @Override
    public void pointChanged() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (isDisposed()) {
                    return;
                }

                Coordinate point = PointsDataManager.getInstance().getHome();
                lonTextField.setText(String.valueOf(point.x));
                latTextField.setText(String.valueOf(point.y));
                // find the nearest station and update the fields.
                updateStationInfo(point);
            }
        });

    }
}
