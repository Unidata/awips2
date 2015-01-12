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
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.geotools.measure.Latitude;
import org.geotools.measure.Longitude;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.RangeRing;
import com.raytheon.viz.awipstools.common.RangeRing.RangeRingType;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.MenuButton;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Dialog for managing the displaying of points in the D2D's Range Rings view.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  10-21-09     #732       bsteffen   Fixed Many issues
 *  07-11-12     #875       rferrel    Bug fix for check box on
 *                                      unapplied points.
 *  07-31-12     #875       rferrel    Use MenuButton to show points in groups.
 *  11-05-12     #1304      rferrel    Added Point Change Listener.
 *  11-29-12     #1365      rferrel    Properly close dialog when not on UI thread.
 *  07-02-2013   #2145      rferrel    populatePointsMenuButton no longer modifies movableRings.
 *                                      Indicate selected movable ring and only delete selected ring.
 *                                      Verify listeners for radius, latitude and longitude fields.
 *  08-20-12     #3467      mapeters   Added addChangeListenerToResourceData  and 
 *                                     removeChangeListenerFromResourceData functions.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */
public class RangeRingDialog extends CaveJFACEDialog implements
        IResourceDataChanged, IPointChangedListener {

    /** Fixed Ring labels' combo. */
    private final String FIXED_LABELS[] = { "None", "1", "2", "3", "12", "13",
            "23", "123", "C", "C1", "C2", "C3", "C12", "C13", "C23", "C123" };

    /** Movable Ring labels' combo. */
    private final String MOVABLE_LABELS[] = { "None", "1", "C", "C1" };

    /** One of the new movable ring selections. */
    private final String LATLON = "Lat/Lon";

    /**
     * A listener to mark a given movable ring's row as selected.
     */
    private final FocusListener lastActiveListener = new FocusAdapter() {
        @Override
        public void focusGained(FocusEvent e) {
            Widget w = (Widget) e.getSource();
            MovableRingRow row = (MovableRingRow) w.getData();
            clearMovableRingSelection();
            row.selectRow(true);
        }
    };

    /** Key for determining if Widget is for latitude or longitude. */
    private final String isLatKey = "isLat";

    /** Format for displaying latitude and longitude. */
    private final String formatLatLon = "%3.6f";

    /**
     * A listener to verify a movable ring's latitude or longitude value.
     */
    private final VerifyListener verifyLatLon = new VerifyListener() {

        @Override
        public void verifyText(VerifyEvent e) {
            Text text = (Text) e.getSource();
            MovableRingRow row = (MovableRingRow) text.getData();
            RangeRing ring = row.ring;
            boolean isLat = (Boolean) text.getData(isLatKey);

            if (ring == null) {
                return;
            }

            // Setup the purposed editing changes to the text.
            StringBuilder sb = new StringBuilder(text.getText());
            sb.replace(e.start, e.end, e.text);
            if (sb.length() == 0) {
                // Empty string value.
                if (isLat) {
                    row.latitude = 0.0;
                } else {
                    row.longitude = 0.0;
                }
            } else {
                try {
                    double value = Double.parseDouble(sb.toString());
                    // Valid double keep for updates.
                    if (isLat) {
                        row.latitude = value;
                    } else {
                        row.longitude = value;
                    }
                } catch (NumberFormatException ex) {
                    // Allow parse error when it may be the start of a
                    // valid double.
                    if (sb.length() == 1) {
                        // Single character is '+', '-' or '.'.
                        e.doit = "+-.".contains(sb);
                    } else if (sb.length() == 2) {
                        e.doit = (sb.indexOf("+.") == 0)
                                || (sb.indexOf("-.") == 0);
                    } else {
                        e.doit = false;
                    }

                    // Treat like an empty string.
                    if (e.doit) {
                        if (isLat) {
                            row.latitude = 0.0;
                        } else {
                            row.longitude = 0.0;
                        }
                    }
                }
            }
        }
    };

    /**
     * Verify radius values for movable and fix rings.
     */
    private final VerifyListener verifyRadius = new VerifyListener() {

        @Override
        public void verifyText(VerifyEvent e) {
            Text text = (Text) e.getSource();
            StringBuilder sb = new StringBuilder(text.getText());
            sb.replace(e.start, e.end, e.text);
            if (sb.length() > 0) {
                try {
                    int value = Integer.parseInt(sb.toString());
                    e.doit = value >= 0;
                } catch (NumberFormatException ex) {
                    e.doit = false;
                }
            }
        }
    };

    /**
     * Groups the display fields for a fixed ring.
     */
    private class FixedRingRow {

        /** Indicate the view state of the fixed ring. */
        public Button enabled;

        /** The name of the fixed ring. */
        public Label id;

        /** The three radii associated with the fixed ring. */
        public Text[] radii = new Text[3];

        /** Combo box with the labels to display. */
        public Combo label;

        /** The fix ring being displayed. */
        public RangeRing ring;

        /**
         * Dispose all widgets so the fixed ring's row is removed from the
         * display.
         */
        public void dispose() {
            enabled.dispose();
            id.dispose();
            for (Text text : radii) {
                text.dispose();
            }
            label.dispose();
        }
    }

    /**
     * This ties the display fields for a movable ring together. Intended to be
     * displayed in a single row.
     */
    private class MovableRingRow {
        /** The ring's display state. */
        public Button enabled;

        /** The ring's name. */
        public Text id;

        /** The ring center's latitude. */
        public Text lat;

        /** The ring centers's longitude. */
        public Text lon;

        /** The ring's radius from the center. */
        public Text radius;

        /** The various labels to display for the ring. */
        public Combo label;

        /** The ring being displayed. */
        public RangeRing ring;

        /** Indicates the ring's row selection state. */
        private boolean selected = false;

        /**
         * Where to place the display widgets. Placed directly on the parent so
         * it can handle sizing an alignment.
         */
        private Composite parent;

        // WARNING Use these to validate the coordinate instead of modifying the
        // row ring's center coordinate. Modifying the center coordinate makes
        // changes that impact the perspective the next time it is refreshed.
        // Thus negating the cancel button. Cloning the ring will break the
        // connection with the perspective. So moving a ring on the perspective
        // will not update the lat/lon on the dialog.

        /** Current double value in the lat field. */
        public double latitude;

        /** Current double value in the lon field. */
        public double longitude;

        /**
         * Constructor.
         * 
         * @param parent
         */
        public MovableRingRow(Composite parent) {
            this.parent = parent;
            init();
        }

        /**
         * Change to ring used by this row and update the display to the ring's
         * values.
         * 
         * @param ring
         */
        public void setRing(RangeRing ring) {
            this.ring = ring;
            populate();
        }

        /**
         * Populates the ring's values for display.
         */
        private void populate() {
            enabled.setSelection(ring.isVisible());
            id.setText(ring.getId());
            Coordinate center = ring.getCenterCoordinate();
            lon.setText(String.format(formatLatLon, center.x));
            longitude = center.x;
            lat.setText(String.format(formatLatLon, center.y));
            latitude = center.y;
            radius.setText(String.valueOf(ring.getRadius()));
            for (int i = 0; i < MOVABLE_LABELS.length; i++) {
                if (MOVABLE_LABELS[i].equals(ring.getLabel())) {
                    label.select(i);
                    break;
                }
            }
        }

        /**
         * Create the display.
         */
        private void init() {
            int width6 = convertWidthInCharsToPixels(6);
            int height = convertHeightInCharsToPixels(1);
            enabled = new Button(parent, SWT.CHECK);
            enabled.setData(this);
            enabled.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Widget w = (Widget) e.getSource();
                    MovableRingRow row = (MovableRingRow) w.getData();
                    if (row.ring != null) {
                        row.ring.setVisible(row.enabled.getSelection());
                    }
                }
            });

            GridData gd = null;
            id = new Text(parent, SWT.SINGLE | SWT.BORDER);
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
            gd.minimumHeight = height;
            gd.minimumWidth = rowIdWidth;
            id.setLayoutData(gd);
            id.setData(this);
            lat = new Text(parent, SWT.SINGLE | SWT.BORDER);
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
            gd.minimumHeight = height;
            lat.setLayoutData(gd);
            lat.setText("-999.000000");
            int width = lat.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
            lat.setText("");
            lat.setData(this);
            lat.setData(isLatKey, true);
            lat.addVerifyListener(verifyLatLon);
            gd.minimumWidth = width;
            lat.setLayoutData(gd);
            lon = new Text(parent, SWT.SINGLE | SWT.BORDER);
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
            gd.minimumHeight = height;
            gd.minimumWidth = width;
            lon.setLayoutData(gd);
            lon.setData(this);
            lon.setData(isLatKey, false);
            lon.addVerifyListener(verifyLatLon);
            radius = new Text(parent, SWT.SINGLE | SWT.BORDER);
            radius.setLayoutData(new GridData(width6, height));
            radius.setData(this);
            radius.addVerifyListener(verifyRadius);
            label = new Combo(parent, SWT.DROP_DOWN | SWT.READ_ONLY);
            label.setItems(MOVABLE_LABELS);
            label.setData(this);
            label.select(0);

            enabled.addFocusListener(lastActiveListener);
            id.addFocusListener(lastActiveListener);
            lat.addFocusListener(lastActiveListener);
            lon.addFocusListener(lastActiveListener);
            radius.addFocusListener(lastActiveListener);
            label.addFocusListener(lastActiveListener);
        }

        /**
         * set the rows select state and update the display.
         * 
         * @param state
         */
        protected void selectRow(boolean state) {
            Color bgColor = null;
            if (state) {
                bgColor = RangeRingDialog.this.getShell().getDisplay()
                        .getSystemColor(SWT.COLOR_LIST_SELECTION_TEXT);
            }
            enabled.setBackground(bgColor);
            selected = state;
        }

        /**
         * Get the row's selected state.
         * 
         * @return state
         */
        protected boolean isSelected() {
            return selected;
        }

        /**
         * dispose widgets so ring's row is removed from the display.
         */
        public void dispose() {
            enabled.dispose();
            id.dispose();
            lat.dispose();
            lon.dispose();
            radius.dispose();
            label.dispose();
        }
    }

    /** Place for the movable ring rows. */
    private Composite movableRingsComposite;

    /** Place for fixed ring rows. */
    private Composite fixedRingsComposite;

    /** resource to inform of any changes. */
    private final AbstractResourceData resourceData;

    /** Data tool manager. */
    private ToolsDataManager toolsDataManager = ToolsDataManager.getInstance();

    /** Point manger used to get tools for selecting points. */
    private PointsDataManager pointsDataManager = PointsDataManager
            .getInstance();

    /** List of Fixed rings in the display. */
    private Collection<FixedRingRow> fixedRings = new ArrayList<FixedRingRow>();

    /** List of movable rings in the display. */
    private Collection<MovableRingRow> movableRings = new ArrayList<MovableRingRow>();

    /** The minimum width for movable ring rows. */
    private int rowIdWidth = SWT.DEFAULT;

    /** Point's selection menu. */
    private MenuButton pointsMenuButton;

    /**
     * The Constructor.
     * 
     * @param parShell
     * @param abstractResourceData
     * @throws VizException
     */
    public RangeRingDialog(Shell parShell,
            AbstractResourceData abstractResourceData) throws VizException {
        super(parShell);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.resourceData = abstractResourceData;
        resourceData.addChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    public Control createDialogArea(Composite parent) {
        Composite topComposite = (Composite) super.createDialogArea(parent);
        getShell().addShellListener(new ShellAdapter() {
            public void shellClosed(ShellEvent e) {
                resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, false);
            }
        });
        // Initialize all of the menus, controls, and layouts
        topComposite.getShell().setText("rangeRing");
        initializeComponents(topComposite);
        load();
        return topComposite;
    }

    /**
     * Initializes the components.
     */
    private void initializeComponents(Composite topComposite) {
        createFixedRingsComposite(topComposite);
        createMovableRingsComposite(topComposite);
    }

    /**
     * Create display area for fixed rings.
     * 
     * @param topComposite
     */
    private void createFixedRingsComposite(Composite topComposite) {
        GridData gd = null;
        new Label(topComposite, SWT.NONE).setText("Fixed Rings");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        topComposite.setLayoutData(gd);
        fixedRingsComposite = new Composite(topComposite, SWT.BORDER);
        fixedRingsComposite.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        fixedRingsComposite.setLayout(new GridLayout(6, false));
        // Make all the labels
        new Label(fixedRingsComposite, SWT.NONE).setText("Show");
        new Label(fixedRingsComposite, SWT.NONE).setText("ID");
        new Label(fixedRingsComposite, SWT.NONE).setText("Radii");
        new Label(fixedRingsComposite, SWT.NONE).setText("(NM)");
        new Label(fixedRingsComposite, SWT.NONE);
        new Label(fixedRingsComposite, SWT.NONE).setText("Labels");
    }

    /**
     * Create display area for movable rings.
     * 
     * @param topComposite
     */
    private void createMovableRingsComposite(Composite topComposite) {
        new Label(topComposite, SWT.NONE).setText("Movable Rings");
        Composite borderComposite = new Composite(topComposite, SWT.BORDER);
        borderComposite.setLayout(new GridLayout(1, false));
        borderComposite.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        movableRingsComposite = new Composite(borderComposite, SWT.NONE);
        movableRingsComposite.setLayoutData(new GridData(SWT.CENTER,
                SWT.CENTER, true, true, 1, 1));

        movableRingsComposite.setLayout(new GridLayout(6, false));
        // Make all the labels
        new Label(movableRingsComposite, SWT.NONE).setText("Show");
        new Label(movableRingsComposite, SWT.NONE).setText("ID");
        new Label(movableRingsComposite, SWT.NONE).setText("Lat");
        new Label(movableRingsComposite, SWT.NONE).setText("Lon");
        new Label(movableRingsComposite, SWT.NONE).setText("Radius");
        new Label(movableRingsComposite, SWT.NONE).setText("Labels");
        Composite createComposite = new Composite(borderComposite, SWT.NONE);
        createComposite.setLayout(new GridLayout(3, false));
        createComposite.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER,
                true, true, 1, 1));
        new Label(createComposite, SWT.NONE).setText("New at: ");

        pointsMenuButton = new MenuButton(createComposite);
        populatePointsMenuButton();

        pointsDataManager.addPointsChangedListener(this);

        pointsMenuButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                MenuButton menuButton = (MenuButton) e.widget;
                MenuItem mi = menuButton.getSelectedItem();
                addMovableRing(mi.getText());
                menuButton.setSelectedItem("Select One");
            }
        });

        Button delete = new Button(createComposite, SWT.PUSH);
        delete.setText("Delete");
        delete.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                deleteMovableRing();
            }
        });
    }

    /**
     * Populate the Point menus button with all the desired points.
     */
    private void populatePointsMenuButton() {
        Menu menu = new Menu(pointsMenuButton);
        MenuItem mi0 = new MenuItem(menu, SWT.PUSH);
        mi0.setText("Select One");

        ArrayList<String> items = new ArrayList<String>();

        // Determine rings to place at the top of the menu.
        Collection<RangeRing> rangeRings = toolsDataManager.getRangeRings();
        for (RangeRing ring : rangeRings) {
            if (ring.getType() == RangeRingType.FIXED) {
                items.add(ring.getId());
            }
        }
        items.add(LATLON);
        String[] itemsArray = items.toArray(new String[items.size()]);
        Arrays.sort(itemsArray);
        MenuItem mi = null;
        for (String item : itemsArray) {
            mi = new MenuItem(menu, SWT.PUSH);
            mi.setText(item);
        }
        mi = new MenuItem(menu, SWT.SEPARATOR);

        // Now add in the points organized in groups.
        pointsMenuButton.setMinimumSize(SWT.DEFAULT, SWT.DEFAULT);
        populatePoints(menu, null);

        pointsMenuButton.setMenu(menu);
        pointsMenuButton.setSelectedItem(mi0);

        // Determine the maximum row.id's width
        rowIdWidth = pointsMenuButton.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
    }

    /**
     * Recursive method for populating points keeping the grouping structure.
     * 
     * @param menu
     * @param root
     */
    private void populatePoints(Menu menu, IPointNode root) {
        for (IPointNode node : pointsDataManager.getChildren(root)) {
            if (!node.isGroup()) {
                MenuItem mi = new MenuItem(menu, SWT.PUSH);
                mi.setText("Point " + node.getName());
                mi.setData(node);
            } else {
                if (pointsDataManager.getChildren(node).size() > 0) {
                    MenuItem mi = new MenuItem(menu, SWT.CASCADE);
                    mi.setText(node.getName());
                    mi.setMenu(new Menu(menu));
                    populatePoints(mi.getMenu(), node);
                }
            }
        }
    }

    /**
     * Add a movable ring row for the desired selection.
     * 
     * @param selection
     */
    private void addMovableRing(String selection) {
        String id = null;
        Coordinate loc = null;
        int radius = 5;

        MovableRingRow row = getNewMovableRow();
        row.enabled.setSelection(true);

        if (LATLON.equals(selection)) {
            for (int i = movableRings.size(); id == null; i++) {
                for (MovableRingRow existingRow : movableRings) {
                    id = "R" + i;
                    if (existingRow.id.getText().equals(id)) {
                        id = null;
                        break;
                    }
                }
            }
        } else {
            if (selection.startsWith("Point ")) {
                String selectedPoint = selection.substring("Point ".length());
                loc = pointsDataManager.getCoordinate(selectedPoint);
            }

            if (loc != null) {
                id = selection;
            } else {
                for (FixedRingRow fixedRow : fixedRings) {
                    if (fixedRow.ring.getId().equals(selection)) {
                        id = selection;
                        loc = fixedRow.ring.getCenterCoordinate();
                        break;
                    }
                }
            }
        }

        if (loc == null) {
            loc = new Coordinate();
        }

        RangeRing ring = new RangeRing(id, loc, radius, "None", true);
        row.setRing(ring);

        movableRingsComposite.layout(true);
        getShell().pack();
        row.lat.forceFocus();
    }

    /**
     * Remove the ring of the selected movable ring row.
     */
    private void deleteMovableRing() {
        for (MovableRingRow row : movableRings) {
            if (row.isSelected()) {
                row.dispose();
                movableRings.remove(row);
                movableRingsComposite.layout(true);
                getShell().pack();
                if (!movableRings.isEmpty()) {
                    movableRings.iterator().next().selectRow(true);
                }
                break;
            }
        }
    }

    /**
     * Clear all selection's of removable ring rows.
     */
    private void clearMovableRingSelection() {
        for (MovableRingRow row : movableRings) {
            row.selectRow(false);
        }
    }

    /**
     * Insert a row for a ring in the Movable Ring composite.
     * 
     * @return row
     */
    private MovableRingRow getNewMovableRow() {
        MovableRingRow row = new MovableRingRow(movableRingsComposite);
        movableRings.add(row);
        return row;
    }

    /**
     * Insert a row for a ring in the Fixed Ring composite.
     * 
     * @return row
     */
    private FixedRingRow getNewFixedRow() {
        int width = convertWidthInCharsToPixels(6);
        int height = convertHeightInCharsToPixels(1);
        final FixedRingRow row = new FixedRingRow();
        row.enabled = new Button(fixedRingsComposite, SWT.CHECK);
        row.enabled.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                row.ring.setVisible(row.enabled.getSelection());
            }
        });

        row.id = new Label(fixedRingsComposite, SWT.NONE);
        for (int i = 0; i < row.radii.length; ++i) {
            Text text = new Text(fixedRingsComposite, SWT.SINGLE | SWT.BORDER);
            text.setLayoutData(new GridData(width, height));
            text.addVerifyListener(verifyRadius);
            row.radii[i] = text;
        }
        row.label = new Combo(fixedRingsComposite, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        row.label.setItems(FIXED_LABELS);
        row.label.select(0);
        fixedRings.add(row);
        return row;
    }

    /**
     * Populate the fields of a fixed ring row.
     * 
     * @param row
     * @param ring
     */
    private void fillFixedRow(FixedRingRow row, RangeRing ring) {
        row.enabled.setSelection(ring.isVisible());
        row.id.setText(ring.getId());
        int[] radii = ring.getRadii();
        int length = Math.min(radii.length, row.radii.length);

        // Guard against to many or to few entries added manually to the
        // localized file.
        for (int i = 0; i < length; ++i) {
            row.radii[i].setText(String.valueOf(radii[i]));
        }
        for (int i = length; i < row.radii.length; ++i) {
            row.radii[i].setText("0");
        }

        for (int i = 0; i < FIXED_LABELS.length; i++) {
            if (FIXED_LABELS[i].equals(ring.getLabel())) {
                row.label.select(i);
                break;
            }
        }
        row.ring = ring;
    }

    /**
     * Clear display and populate from range rings obtained from the manager.
     */
    private void load() {
        for (FixedRingRow row : fixedRings) {
            row.dispose();
        }
        for (MovableRingRow row : movableRings) {
            row.dispose();
        }
        fixedRings.clear();
        movableRings.clear();
        Collection<RangeRing> rangeRings = toolsDataManager.getRangeRings();

        for (RangeRing ring : rangeRings) {
            if (ring.getType() == RangeRingType.FIXED) {
                FixedRingRow row = getNewFixedRow();
                fillFixedRow(row, ring);
            } else {
                MovableRingRow row = getNewMovableRow();
                row.setRing(ring);
            }
        }

        if (!movableRings.isEmpty()) {
            movableRings.iterator().next().selectRow(true);
        }

        getShell().layout(true, true);
        getShell().pack();
    }

    /**
     * Validate the coordinates for the movable rings.
     * 
     * @return true when all coordinates are valid
     */
    private boolean verifyRings() {
        String errMsg = null;
        for (MovableRingRow row : movableRings) {
            if (row.latitude < Latitude.MIN_VALUE
                    || row.latitude > Latitude.MAX_VALUE) {
                errMsg = String.format("Latitude %s is out of range (±90°).",
                        new Latitude(row.latitude));
                row.lat.selectAll();
                row.lat.forceFocus();
                break;
            }

            if (row.longitude > 180.0 && row.longitude <= 360.0) {
                // Adjust value to range (±180°).
                row.longitude = MapUtil.correctLon(row.longitude);
                row.lon.setText(String.format(formatLatLon, row.longitude));
            } else if (row.longitude < Longitude.MIN_VALUE
                    || row.longitude > Longitude.MAX_VALUE) {
                // To far outside of range to adjust let the user handle it.
                errMsg = String.format("Longitude %s is out of range (±180°).",
                        new Longitude(row.longitude));
                row.lon.selectAll();
                row.lon.forceFocus();
                break;
            }
        }

        if (errMsg != null) {
            final String message = errMsg;
            // Finish up the listener that called this before opening the
            // dialog.
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    MessageDialog.openError(getShell(), "Error", message);
                }
            });
            return false;
        }
        return true;
    }

    /**
     * Save users range rings to the manager and notify others of the change.
     */
    private void saveChanges() {
        Collection<RangeRing> rangeRings = new ArrayList<RangeRing>();
        for (FixedRingRow row : fixedRings) {
            boolean enabled = row.enabled.getSelection();
            String id = row.id.getText();
            int[] radii = new int[3];
            radii[0] = checkedParseInt(row.radii[0].getText());
            radii[1] = checkedParseInt(row.radii[1].getText());
            radii[2] = checkedParseInt(row.radii[2].getText());
            Coordinate center = row.ring.getCenterCoordinate();
            String label = row.label.getText();
            rangeRings.add(new RangeRing(id, center, radii, label, enabled));
        }
        for (MovableRingRow row : movableRings) {
            boolean enabled = row.enabled.getSelection();
            String id = row.id.getText();
            int radius = checkedParseInt(row.radius.getText());
            Coordinate center = new Coordinate(row.longitude, row.latitude);
            String label = row.label.getText();
            rangeRings.add(new RangeRing(id, center, radius, label, enabled));
        }
        toolsDataManager.setRangeRings(rangeRings);
        load();
    }

    /**
     * Parse string for integer value and return 0 if error in the parsing.
     * 
     * @param string
     * @return value
     */
    private int checkedParseInt(String string) {
        try {
            return Integer.parseInt(string);
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        // override, so can add calculate as default button.
        Button cancelButton = createButton(parent, SWT.CANCEL, "Cancel", false);
        Button applyButton = createButton(parent, 1234, "Apply", false);
        Button okButton = createButton(parent, SWT.OK, "OK", false);

        cancelButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                close();
            }
        });
        applyButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                if (verifyRings()) {
                    saveChanges();
                    resourceData.fireChangeListeners(ChangeType.DATA_UPDATE,
                            null);
                }
            }
        });
        okButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                if (verifyRings()) {
                    saveChanges();
                    close();
                } else {
                    event.doit = false;
                }
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (object instanceof Boolean) {
            boolean b = (Boolean) object;
            if (b == false) {
                this.resourceData.removeChangeListener(this);
                performClose();
            }
            return;
        } else if (object instanceof RangeRing[]) {
            RangeRing[] update = (RangeRing[]) object;
            RangeRing oldRing = update[0];
            RangeRing newRing = update[1];
            for (FixedRingRow row : fixedRings) {
                if (row.ring.equals(oldRing)) {
                    fillFixedRow(row, newRing);
                }
            }
            for (MovableRingRow row : movableRings) {
                if (row.ring.equals(oldRing)) {
                    row.setRing(newRing);
                }
            }
            return;
        } else if (object instanceof EditableCapability) {
            if (!((EditableCapability) object).isEditable()) {
                this.resourceData.removeChangeListener(this);
                performClose();
            }
        }
    }

    /**
     * Close dialog when not on the UI thread.
     */
    private void performClose() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                close();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close() {
        // passing true tells the resource that editable should be turned off
        this.resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, true);
        pointsDataManager.removePointsChangedListener(this);
        return super.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.IPointChangedListener#pointChanged()
     */
    @Override
    public void pointChanged() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                populatePointsMenuButton();
            }
        });
    }

    public void addChangeListenerToResourceData(IResourceDataChanged resource) {
        this.resourceData.addChangeListener(resource);
    }

    public void removeChangeListenerFromResourceData(
            IResourceDataChanged resource) {
        this.resourceData.removeChangeListener(resource);
    }
}
