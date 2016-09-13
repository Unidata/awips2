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
package com.raytheon.viz.mpe.ui.dialogs.polygon;

import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData.PolygonEditAction;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The Delete Polygon Dialog box.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 2, 2009  2685       mpduff      Initial creation
 * Jan 26, 2011 7761       bkowal      Polygon values will no
 *                                     longer be divided by 100.
 *                                     Polygon values will now be
 *                                     displayed for polygons with
 *                                     the "sub" action.
 * Jan 7, 2015  16954      cgobs       Fix for cv_use issue - using getFieldName() in certain parts.
 * Feb 15, 2016 5338       bkowal      Keep track of any persistent polygons that are deleted.
 * Apr 08, 2016 5504       bkowal      Fix GUI sizing issues. Display tabular data in a {@link Table}.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DeletePolygonDlg extends CaveSWTDialog {

    private static final int NUM_POLYGON_ROWS = 7;

    private static final String POLY_TRUE = "T";

    private static final String POLY_FALSE = "F";

    private static final int DISPLAY_COL_INDEX = 1;

    private enum MPE_TABLE_COLUMNS {
        NUMBER("Number", 12), DISPLAYED("Displayed", 16), PERSISTENT(
                "Persistent", 20), ACTION("Action", 20), VALUE("Value", 16);

        private final String text;

        private final int numCharacters;

        private MPE_TABLE_COLUMNS(String text, int numCharacters) {
            this.text = text;
            this.numCharacters = numCharacters;
        }

        public String getText() {
            return text;
        }

        public int getNumCharacters() {
            return numCharacters;
        }
    }

    /**
     * {@link Table} to display information about the current mpe polygons.
     */
    private Table table;

    /**
     * Date/Time Text Label.
     */
    private Label dateTimeLbl;

    /**
     * Product Label.
     */
    private Label productLbl;

    private Button displayBtn;

    private Button undisplayBtn;

    private Button deleteBtn;

    private Button deleteAllBtn;

    /**
     * Polygon list.
     */
    private List<RubberPolyData> polygonList = Collections.emptyList();

    /**
     * Simple date formatter.
     */
    private SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The parent shell for this dialog.
     */
    public DeletePolygonDlg(Shell parentShell) {
        super(parentShell);
        setText("Delete Polygons");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        Composite comp = createMainComposite();
        createDateTimeProduct(comp);
        createPolygonTable(comp);
        createPolygonButtons(comp);
        createCloseButton(comp);
        populateDlg();
    }

    /**
     * Create the main composite.
     * 
     * @return the Composite
     */
    private Composite createMainComposite() {
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayoutData(gd);

        return comp;
    }

    /**
     * Create the Date/Time and product composite
     * 
     * @param comp
     *            The main composite
     */
    private void createDateTimeProduct(Composite comp) {
        // Create adjust group
        Composite headerComp = new Composite(comp, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        gl.marginWidth = 0;
        headerComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        headerComp.setLayoutData(gd);

        Composite dateTimeComp = new Composite(headerComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        dateTimeComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dateTimeComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        Label dtLbl = new Label(dateTimeComp, SWT.NONE);
        dtLbl.setLayoutData(gd);
        dtLbl.setText("Date/Time:");
        dtLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        dateTimeLbl = new Label(dateTimeComp, SWT.BORDER);
        dateTimeLbl.setLayoutData(gd);

        Composite productComp = new Composite(headerComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        productComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        productComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        Label prodLbl = new Label(productComp, SWT.NONE);
        prodLbl.setLayoutData(gd);
        prodLbl.setText("MPE Product:");
        prodLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        productLbl = new Label(productComp, SWT.BORDER);
        productLbl.setLayoutData(gd);
    }

    /**
     * Create the polygon list widget
     * 
     * @param comp
     *            The main composite
     */
    private void createPolygonTable(Composite comp) {
        table = new Table(comp, SWT.BORDER | SWT.V_SCROLL | SWT.MULTI);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = table.getItemHeight() * NUM_POLYGON_ROWS;
        table.setLayoutData(gd);

        /*
         * Add table columns.
         */
        GC gc = new GC(table);
        gc.setFont(table.getFont());

        for (MPE_TABLE_COLUMNS mpeTableColumn : MPE_TABLE_COLUMNS.values()) {
            TableColumn tc = new TableColumn(table, SWT.CENTER);
            tc.setText(mpeTableColumn.getText());
            tc.setWidth(gc.getFontMetrics().getAverageCharWidth()
                    * mpeTableColumn.getNumCharacters());
        }

        gc.dispose();

        table.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleTableSelection(e);
            }
        });
    }

    /**
     * Create the dialog buttons.
     * 
     * @param comp
     *            The main composite
     */
    private void createPolygonButtons(Composite comp) {
        Composite buttonComp = new Composite(comp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(4, true));
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        buttonComp.setLayoutData(gd);

        final int minimumButtonWidth = buttonComp.getDisplay().getDPI().x;

        displayBtn = new Button(buttonComp, SWT.PUSH);
        displayBtn.setText("Display");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        displayBtn.setLayoutData(gd);
        displayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                display(true);
            }
        });
        displayBtn.setEnabled(false);

        undisplayBtn = new Button(buttonComp, SWT.PUSH);
        undisplayBtn.setText("Undisplay");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        undisplayBtn.setLayoutData(gd);
        undisplayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                display(false);
            }
        });
        undisplayBtn.setEnabled(false);

        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                delete();
            }
        });
        deleteBtn.setEnabled(false);

        deleteAllBtn = new Button(buttonComp, SWT.PUSH);
        deleteAllBtn.setText("Delete All");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        deleteAllBtn.setLayoutData(gd);
        deleteAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteAll();
            }
        });
        deleteAllBtn.setEnabled(false);
    }

    /**
     * Create the close button.
     */
    private void createCloseButton(Composite comp) {
        // Add separator
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label sepLbl = new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        Button closeBtn = new Button(comp, SWT.PUSH);
        closeBtn.setText("Close");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = closeBtn.getDisplay().getDPI().x;
        closeBtn.setAlignment(SWT.CENTER);
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Populate the dialog with polygon edits from {@link PolygonEditManager}
     */
    private void populateDlg() {
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        Date editDate = displayManager.getCurrentEditDate();
        DisplayFieldData fieldData = displayManager.getDisplayFieldType();
        dateTimeLbl.setText(sdf.format(editDate));

        String type = displayManager.getDisplayFieldType().getFieldName();
        productLbl.setText(type);
        polygonList = PolygonEditManager.getPolygonEdits(fieldData, editDate);
        populatePolygonTable();
    }

    private void populatePolygonTable() {
        table.removeAll();

        if (polygonList.isEmpty()) {
            displayBtn.setEnabled(false);
            undisplayBtn.setEnabled(false);
            deleteBtn.setEnabled(false);
            deleteAllBtn.setEnabled(false);
            return;
        }

        for (int i = 0; i < polygonList.size(); i++) {
            RubberPolyData data = polygonList.get(i);
            final PolygonEditAction action = data.getEditAction();

            final String number = String.valueOf(i + 1);
            final String displayed = data.isVisible() ? POLY_TRUE : POLY_FALSE;
            final String persist = data.isPersistent() ? POLY_TRUE : POLY_FALSE;
            final String value = (action == PolygonEditAction.SUB) ? data
                    .getSubDrawSource().getFieldName() : Double.toString(data
                    .getPrecipValue());

            TableItem ti = new TableItem(table, SWT.NONE);
            ti.setData(data);
            final String[] tableItemValues = new String[] { number, displayed,
                    persist, action.toPrettyName(), value };
            ti.setText(tableItemValues);
        }
        deleteAllBtn.setEnabled(true);
    }

    private void handleTableSelection(SelectionEvent e) {
        if (table.getSelectionCount() <= 0) {
            this.displayBtn.setEnabled(false);
            this.undisplayBtn.setEnabled(false);
            this.deleteBtn.setEnabled(false);
            return;
        }

        RubberPolyData data = (RubberPolyData) table.getSelection()[0]
                .getData();
        final boolean visible = data.isVisible();
        this.displayBtn.setEnabled(!visible);
        this.undisplayBtn.setEnabled(visible);
        this.deleteBtn.setEnabled(true);
    }

    /**
     * Delete the selected polygon.
     */
    private void delete() {
        // Remove selected from list and apply
        RubberPolyData data = (RubberPolyData) table.getSelection()[0]
                .getData();
        polygonList.remove(data);
        applyPolygonList(data.isPersistent(), true);
    }

    /**
     * Delete all polygons.
     */
    private void deleteAll() {
        // Clear the list and apply
        boolean persistentRemoved = false;
        for (RubberPolyData polygon : polygonList) {
            if (polygon.isPersistent()) {
                persistentRemoved = true;
                break;
            }
        }
        polygonList.clear();
        applyPolygonList(persistentRemoved, true);
    }

    /**
     * Show the polygon on the screen or not.
     * 
     * @param display
     *            Show if true, not if false
     * 
     * @param polygon
     *            The polygon to display/undisplay
     */
    private void display(boolean display) {
        TableItem tableItem = table.getSelection()[0];
        RubberPolyData data = (RubberPolyData) tableItem.getData();
        data.setVisible(display);
        applyPolygonList(data.isPersistent(), false);

        tableItem
                .setText(DISPLAY_COL_INDEX, (display) ? POLY_TRUE : POLY_FALSE);

        displayBtn.setEnabled(!display);
        undisplayBtn.setEnabled(display);
    }

    private void applyPolygonList(final boolean persistentRemoved, final boolean populate) {
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        DisplayFieldData fieldData = displayManager.getDisplayFieldType();
        Date editDate = displayManager.getCurrentEditDate();
        PolygonEditManager.writePolygonEdits(fieldData, editDate, polygonList,
                persistentRemoved);
        if (populate) {
            populatePolygonTable();
        }
    }
}
