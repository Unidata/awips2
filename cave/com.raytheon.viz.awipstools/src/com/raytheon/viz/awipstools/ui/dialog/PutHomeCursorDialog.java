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

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
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
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKTWriter;

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
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */
public class PutHomeCursorDialog extends CaveSWTDialog implements
        IPointChangedListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PutHomeCursorDialog.class);

    private final String DIST_QRY_FMT = "SELECT icao, name, state FROM common_obs_spatial "
            + "WHERE ST_DWithin(the_geom, ST_GeomFromText('%s'), %4.1f) AND"
            + " catalogtype = %d ORDER BY ST_Distance(the_geom, ST_GeomFromText('%1$s'))";

    private final String ICAO_QRY_FMT = "SELECT AsBinary(the_geom) FROM common_obs_spatial "
            + "WHERE catalogtype = %d and icao = '%s'";

    private final String CITY_QRY_FMT = "SELECT AsBinary(the_geom) FROM common_obs_spatial "
            + "WHERE catalogtype = %d AND name = '%s' AND state = '%s'";

    private final String CITY_DB_QRY_FMT = "SELECT AsBinary(the_geom) FROM mapdata.city "
            + "WHERE name = '%s' AND st = '%s'";

    private final String AIRPORT_DB_QRY_FMT = "SELECT AsBinary(the_geom) FROM mapdata.airport "
            + "WHERE city = '%s' AND state = '%s'";

    /** Station radio button. */
    private Button stationRadio;

    /** City radio button. */
    private Button cityRadio;

    /** Lat/Lon radio button. */
    private Button latLonRadio;

    /** Station label. */
    private Label stationLabel;

    /** Station text field. */
    private Text stationTextField;

    /** City label. */
    private Label citylabel;

    /** City text field. */
    private Text cityTextField;

    /** State label. */
    private Label stateLabel;

    /** State text field. */
    private Text stateTextField;

    /** Latitude label. */
    private Label latLabel;

    /** Latitude text field */
    private Text latTextField;

    /** Longitude label. */
    private Label lonLabel;

    /** Longitude text field. */
    private Text lonTextField;

    /** Go button. */
    private Button goBtn;

    /** Close button. */
    private Button closeBtn;

    /**
     * Constructor.
     * 
     * @param shell
     *            Parent shell.
     */
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
        // TODO Auto-generated method stub
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
        controlsComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        createStationChoice(controlsComp);
        createCityChoice(controlsComp);
        createLatLonChoice(controlsComp);

        createActionButtons();

        try {
            updateStationInfo(PointsDataManager.getInstance().getHome());
        } catch (Exception e) {

        }

        enableDisableControls();
    }

    @Override
    protected void disposed() {
        PointsDataManager.getInstance().removeHomeChangedListener(
                PutHomeCursorDialog.this);
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

        /*
         * Station radio button selection
         */
        stationRadio = new Button(locationComp, SWT.RADIO);
        stationRadio.setText("Station");
        stationRadio.setSelection(true);
        stationRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (stationRadio.getSelection() == false) {
                    return;
                }
                enableDisableControls();
            }
        });

        /*
         * City radio button selection
         */
        cityRadio = new Button(locationComp, SWT.RADIO);
        cityRadio.setText("City/State");
        cityRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (cityRadio.getSelection() == false) {
                    return;
                }
                enableDisableControls();
            }
        });

        /*
         * Lat/Lon radio button selection
         */
        latLonRadio = new Button(locationComp, SWT.RADIO);
        latLonRadio.setText("Lat/Lon");
        latLonRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (latLonRadio.getSelection() == false) {
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

        stateLabel = new Label(controlsComp, SWT.NONE);
        stateLabel.setText("State:");
        stateLabel.setToolTipText("Two Letter Abbreviation");

        gd = new GridData(50, SWT.DEFAULT);
        stateTextField = new Text(controlsComp, SWT.BORDER);
        stateTextField.setTextLimit(2);
        stateTextField.setLayoutData(gd);

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

        Coordinate point = PointsDataManager.getInstance().getHome();
        lonTextField.setText(String.valueOf(point.x));
        latTextField.setText(String.valueOf(point.y));

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
                    MessageDialog.openError(shell, "Put Home Cursor Error",
                            "Could not find that Metar station");
                    stationTextField.setFocus();
                } else {
                    PointsDataManager.getInstance().setHome(c);
                    stationTextField.setText(station);
                }
            } else {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "The input for the Station is empty.  Please correct.");
                stationTextField.setFocus();
            }
        } else if (cityRadio.getSelection()) {
            if (cityTextField.getText().isEmpty()) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "The input for the City is empty.  Please correct.");
                cityTextField.setFocus();
                return;
            }
            if (stateTextField.getText().length() != 2) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "The input for the State is invalid.  Please correct.");
                stateTextField.setFocus();
                return;
            }
            String city = cityTextField.getText().toUpperCase().trim();
            String state = stateTextField.getText().toUpperCase().trim();
            Coordinate c = runCityQuery(city, state);
            if (c == null) {
                MessageDialog.openError(shell, "Put Home Cursor Error",
                        "Could not find that city");
                cityTextField.setFocus();
            } else {
                PointsDataManager.getInstance().setHome(c);
                cityTextField.setText(city);
                stateTextField.setText(state);
            }
        } else if (latLonRadio.getSelection()) {
            Coordinate c = new Coordinate();
            try {
                c.x = Float.parseFloat(lonTextField.getText());
            } catch (NumberFormatException nfe) {
                MessageDialog
                        .openError(shell, "Put Home Cursor Error",
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
        goBtn = new Button(buttonComp, SWT.PUSH);
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
        closeBtn = new Button(buttonComp, SWT.PUSH);
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
