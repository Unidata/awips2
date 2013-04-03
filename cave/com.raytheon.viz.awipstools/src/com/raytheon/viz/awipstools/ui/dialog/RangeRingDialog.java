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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
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
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */
public class RangeRingDialog extends CaveJFACEDialog implements
        IResourceDataChanged, IPointChangedListener {

    private final String FIXED_LABELS[] = { "None", "1", "2", "3", "12", "13",
            "23", "123", "C", "C1", "C2", "C3", "C12", "C13", "C23", "C123" };

    private final String MOVABLE_LABELS[] = { "None", "1", "C", "C1" };

    private final String LATLON = "Lat/Lon";

    private class FixedRingRow {

        public Button enabled;

        public Label id;

        public Text[] radii = new Text[3];

        public Combo label;

        public RangeRing ring;

        public void dispose() {
            enabled.dispose();
            id.dispose();
            radii[0].dispose();
            radii[1].dispose();
            radii[2].dispose();
            label.dispose();
        }
    }

    private class MovableRingRow {
        public Button enabled;

        public Text id;

        public Text lat;

        public Text lon;

        public Text radius;

        public Combo label;

        public RangeRing ring;

        public void dispose() {
            enabled.dispose();
            id.dispose();
            lat.dispose();
            lon.dispose();
            radius.dispose();
            label.dispose();
        }
    }

    private Composite movableRingsComposite;

    private Composite fixedRingsComposite;

    private final AbstractResourceData resourceData;

    private ToolsDataManager toolsDataManager = ToolsDataManager.getInstance();

    private PointsDataManager pointsDataManager = PointsDataManager
            .getInstance();

    private Collection<FixedRingRow> fixedRings = new ArrayList<FixedRingRow>();

    private Collection<MovableRingRow> movableRings = new ArrayList<MovableRingRow>();

    private int rowIdWidth = SWT.DEFAULT;

    private MenuButton pointsMenuButton;

    public Widget lastActiveWidget;

    private FocusListener lastActiveListener = new FocusListener() {

        @Override
        public void focusGained(FocusEvent e) {
            lastActiveWidget = e.widget;
        }

        @Override
        public void focusLost(FocusEvent e) {
            ;// who cares
        }

    };

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
        new Label(fixedRingsComposite, SWT.NONE);
        new Label(fixedRingsComposite, SWT.NONE).setText("ID");
        new Label(fixedRingsComposite, SWT.NONE).setText("Radii");
        new Label(fixedRingsComposite, SWT.NONE).setText("(NM)");
        new Label(fixedRingsComposite, SWT.NONE);
        new Label(fixedRingsComposite, SWT.NONE).setText("Labels");
    }

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
        new Label(movableRingsComposite, SWT.NONE);
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

        for (RangeRing ring : rangeRings) {
            if (ring.getType() != RangeRingType.FIXED) {
                MovableRingRow row = getNewMovableRow();
                fillMovableRow(row, ring);
            }
        }
    }

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

    private void addMovableRing(String selection) {
        MovableRingRow row = getNewMovableRow();
        row.enabled.setSelection(true);
        row.radius.setText("5");
        if (LATLON.equals(selection)) {
            String id = null;
            for (int i = movableRings.size(); id == null; i++) {
                for (MovableRingRow existingRow : movableRings) {
                    id = "R" + i;
                    if (existingRow.id.getText().equals(id)) {
                        id = null;
                        break;
                    }
                }
            }
            row.id.setText(id);
        } else {
            Coordinate loc = null;
            if (selection.startsWith("Point ")) {
                String selectedPoint = selection.substring("Point ".length());
                loc = pointsDataManager.getCoordinate(selectedPoint);
            }

            if (loc != null) {
                row.id.setText(selection);
                row.lon.setText(String.valueOf(loc.x));
                row.lat.setText(String.valueOf(loc.y));
            } else {
                for (FixedRingRow fixedRow : fixedRings) {
                    if (fixedRow.ring.getId().equals(selection)) {
                        row.id.setText(selection);
                        loc = fixedRow.ring.getCenterCoordinate();
                        row.lon.setText(String.valueOf(loc.x));
                        row.lat.setText(String.valueOf(loc.y));
                        break;
                    }
                }
            }
        }
        movableRingsComposite.layout(true);
        getShell().pack();
        row.lat.forceFocus();
    }

    private void deleteMovableRing() {
        MovableRingRow deletedRow = null;
        if (lastActiveWidget != null && !lastActiveWidget.isDisposed()) {
            for (MovableRingRow row : movableRings) {
                if (row.enabled == lastActiveWidget
                        || row.id == lastActiveWidget
                        || row.lat == lastActiveWidget
                        || row.lon == lastActiveWidget
                        || row.radius == lastActiveWidget
                        || row.label == lastActiveWidget) {
                    deletedRow = row;
                    break;
                }
            }
        } else if (movableRings.size() > 0) {
            deletedRow = movableRings.iterator().next();
        }
        if (deletedRow != null) {
            deletedRow.dispose();
            deletedRow.label.dispose();
            lastActiveWidget = null;
            movableRings.remove(deletedRow);
            movableRingsComposite.layout(true);
            getShell().pack();
        }
    }

    private MovableRingRow getNewMovableRow() {
        final MovableRingRow row = new MovableRingRow();
        int width6 = convertWidthInCharsToPixels(6);
        int height = convertHeightInCharsToPixels(1);
        row.enabled = new Button(movableRingsComposite, SWT.CHECK);
        row.enabled.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (row.ring != null) {
                    row.ring.setVisible(row.enabled.getSelection());
                }
            }
        });

        GridData gd = null;
        row.id = new Text(movableRingsComposite, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.minimumHeight = height;
        gd.minimumWidth = rowIdWidth;
        row.id.setLayoutData(gd);
        row.lat = new Text(movableRingsComposite, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.minimumHeight = height;
        row.lat.setLayoutData(gd);
        row.lat.setText("-999.000000");
        int width = row.lat.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
        row.lat.setText("");
        gd.minimumWidth = width;
        row.lat.setLayoutData(gd);
        row.lon = new Text(movableRingsComposite, SWT.SINGLE | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.minimumHeight = height;
        gd.minimumWidth = width;
        row.lon.setLayoutData(gd);
        row.radius = new Text(movableRingsComposite, SWT.SINGLE | SWT.BORDER);
        row.radius.setLayoutData(new GridData(width6, height));
        row.label = new Combo(movableRingsComposite, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        row.label.setItems(MOVABLE_LABELS);
        row.label.select(0);
        row.enabled.addFocusListener(lastActiveListener);
        row.id.addFocusListener(lastActiveListener);
        row.lat.addFocusListener(lastActiveListener);
        row.lon.addFocusListener(lastActiveListener);
        row.radius.addFocusListener(lastActiveListener);
        row.label.addFocusListener(lastActiveListener);

        movableRings.add(row);
        return row;
    }

    private void fillMovableRow(MovableRingRow row, RangeRing ring) {
        row.enabled.setSelection(ring.isVisible());
        row.id.setText(ring.getId());
        Coordinate center = ring.getCenterCoordinate();
        row.lon.setText(String.valueOf(center.x));
        row.lat.setText(String.valueOf(center.y));
        row.radius.setText(String.valueOf(ring.getRadius()));
        for (int i = 0; i < MOVABLE_LABELS.length; i++) {
            if (MOVABLE_LABELS[i].equals(ring.getLabel())) {
                row.label.select(i);
                break;
            }
        }
        row.ring = ring;
    }

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
        row.radii[0] = new Text(fixedRingsComposite, SWT.SINGLE | SWT.BORDER);
        row.radii[1] = new Text(fixedRingsComposite, SWT.SINGLE | SWT.BORDER);
        row.radii[2] = new Text(fixedRingsComposite, SWT.SINGLE | SWT.BORDER);
        row.radii[0].setLayoutData(new GridData(width, height));
        row.radii[1].setLayoutData(new GridData(width, height));
        row.radii[2].setLayoutData(new GridData(width, height));
        row.label = new Combo(fixedRingsComposite, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        row.label.setItems(FIXED_LABELS);
        row.label.select(0);
        fixedRings.add(row);
        return row;
    }

    private void fillFixedRow(FixedRingRow row, RangeRing ring) {
        row.enabled.setSelection(ring.isVisible());
        row.id.setText(ring.getId());
        int[] radii = ring.getRadii();
        row.radii[0].setText(String.valueOf(radii[0]));
        row.radii[1].setText(String.valueOf(radii[1]));
        row.radii[2].setText(String.valueOf(radii[2]));
        for (int i = 0; i < FIXED_LABELS.length; i++) {
            if (FIXED_LABELS[i].equals(ring.getLabel())) {
                row.label.select(i);
                break;
            }
        }
        row.ring = ring;
    }

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
                fillMovableRow(row, ring);
            }
        }
        getShell().layout(true, true);
        getShell().pack();
    }

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
            double lat = checkedParseDouble(row.lat.getText());
            double lon = checkedParseDouble(row.lon.getText());
            Coordinate center = new Coordinate(lon, lat);
            String label = row.label.getText();
            rangeRings.add(new RangeRing(id, center, radius, label, enabled));
        }
        toolsDataManager.setRangeRings(rangeRings);
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, null);
        load();
    }

    private int checkedParseInt(String string) {
        try {
            return Integer.parseInt(string);
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    private double checkedParseDouble(String string) {
        try {
            return Double.parseDouble(string);
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        // override, so can add calculate as default button.
        Button cancelButton = createButton(parent, SWT.CANCEL, "Cancel", false);
        Button applyButton = createButton(parent, 1234, "Apply", false);
        Button okButton = createButton(parent, SWT.OK, "OK", false);

        cancelButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                // passing false closes the dialog
                resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, false);
            }
        });
        applyButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                saveChanges();
            }
        });
        okButton.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                saveChanges();
                resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, false);
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
                    fillMovableRow(row, newRing);
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

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close() {
        // passing true tells the resource that editable should be turned off
        this.resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, true);
        pointsDataManager.removePointsChangedListener(this);
        return super.close();
    }

    /* (non-Javadoc)
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
}
