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
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

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
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DeletePolygonDlg extends CaveSWTDialog {
    private static final String format = "%-2s %27s %27s %21s                  %2.2f";

    private static final String format2 = "%-2s %27s %27s %21s                 %8s";

    /**
     * Date/Time Text Field.
     */
    private Text dateTimeTF = null;

    /**
     * Product Text Field.
     */
    private Text productTF = null;

    /**
     * Polygon List Text Field.
     */
    private List polygonListBox = null;

    /**
     * Polygon list.
     */
    private java.util.List<RubberPolyData> polygonList = new ArrayList<RubberPolyData>();

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
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        Composite comp = createMainComposite();
        createDateTimeProduct(comp);
        createPolygonList(comp);
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
        comp.setLayout(new GridLayout(2, true));
        GridData gd = new GridData(500, SWT.DEFAULT);
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
        Composite dateTimeComp = new Composite(comp, SWT.NONE);
        dateTimeComp.setLayout(new GridLayout(2, false));

        Label dateTimeLbl = new Label(dateTimeComp, SWT.NONE);
        dateTimeLbl.setText("Date/Time:  ");

        GridData gd = new GridData(135, SWT.DEFAULT);
        dateTimeTF = new Text(dateTimeComp, SWT.BORDER);
        dateTimeTF.setLayoutData(gd);

        Composite productComp = new Composite(comp, SWT.NONE);
        productComp.setLayout(new GridLayout(2, false));

        Label prodLbl = new Label(productComp, SWT.NONE);
        prodLbl.setText("MPE Product:  ");

        gd = new GridData(125, SWT.DEFAULT);
        productTF = new Text(productComp, SWT.BORDER);
        productTF.setLayoutData(gd);
    }

    /**
     * Create the polygon list widget
     * 
     * @param comp
     *            The main composite
     */
    private void createPolygonList(Composite comp) {
        Composite dataComp = new Composite(comp, SWT.NONE);
        dataComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false, 2, 1);
        dataComp.setLayoutData(gd);

        Label label = new Label(dataComp, SWT.NONE);
        label.setText("Number            Displayed             Persistent           Action                  Value");

        polygonListBox = new List(dataComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gd.widthHint = 480;
        gd.heightHint = 150;
        polygonListBox.setLayoutData(gd);
    }

    /**
     * Create the dialog buttons.
     * 
     * @param comp
     *            The main composite
     */
    private void createPolygonButtons(Composite comp) {
        Composite buttonComp = new Composite(comp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(4, false));
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false, 2, 1);
        buttonComp.setLayoutData(gd);

        Button displayBtn = new Button(buttonComp, SWT.PUSH);
        displayBtn.setText("Display");
        gd = new GridData(116, SWT.DEFAULT);
        displayBtn.setAlignment(SWT.CENTER);
        displayBtn.setLayoutData(gd);
        displayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                display(true, polygonListBox.getSelectionIndex());
            }
        });

        Button undisplayBtn = new Button(buttonComp, SWT.PUSH);
        undisplayBtn.setText("Undisplay");
        gd = new GridData(116, SWT.DEFAULT);
        undisplayBtn.setAlignment(SWT.CENTER);
        undisplayBtn.setLayoutData(gd);
        undisplayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                display(false, polygonListBox.getSelectionIndex());
            }
        });

        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        gd = new GridData(116, SWT.DEFAULT);
        deleteBtn.setAlignment(SWT.CENTER);
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                delete();
            }
        });

        Button deleteAllBtn = new Button(buttonComp, SWT.PUSH);
        deleteAllBtn.setText("Delete All");
        gd = new GridData(116, SWT.DEFAULT);
        deleteAllBtn.setAlignment(SWT.CENTER);
        deleteAllBtn.setLayoutData(gd);
        deleteAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteAll();
            }
        });
    }

    /**
     * Create the close button.
     */
    private void createCloseButton(Composite comp) {
        // Add separator
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false, 2, 1);
        Label sepLbl = new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
        gd.widthHint = 480;
        sepLbl.setLayoutData(gd);

        Button closeBtn = new Button(comp, SWT.PUSH);
        closeBtn.setText("Close");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false, 2, 1);
        gd.widthHint = 90;
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
        dateTimeTF.setText(sdf.format(editDate));

        polygonListBox.removeAll();

        String type = displayManager.getDisplayFieldType().getCv_use()
                .toUpperCase();
        productTF.setText(type);
        polygonList = PolygonEditManager.getPolygonEdits(fieldData, editDate);
        recreatePolygonListBox();
    }

    /**
     * Recreates the polygonListBox based on polygonList field
     */
    private void recreatePolygonListBox() {
        int[] selected = polygonListBox.getSelectionIndices();
        polygonListBox.removeAll();
        for (int i = 0; i < polygonList.size(); i++) {
            RubberPolyData data = polygonList.get(i);
            String number = String.valueOf(i + 1);
            String displayed = "F";
            if (data.isVisible()) {
                displayed = "T";
            }

            String persist = "F";
            if (data.isPersistent()) {
                persist = "T";
            }

            PolygonEditAction action = data.getEditAction();
            if (action == PolygonEditAction.SUB) {
                String value = data.getSubDrawSource().getCv_use();
                polygonListBox.add(String.format(format2, number, displayed,
                        persist, action.toPrettyName(), value));
            } else {
                double value = data.getPrecipValue();
                polygonListBox.add(String.format(format, number, displayed,
                        persist, action.toPrettyName(), value));
            }
        }
        int numGood = 0;
        for (int idx : selected) {
            if (idx >= 0 && idx < polygonListBox.getItemCount()) {
                numGood += 1;
            }
        }
        int[] newSelected = new int[numGood];
        int i = 0;
        for (int idx : selected) {
            if (idx >= 0 && idx < polygonListBox.getItemCount()) {
                newSelected[i++] = idx;
            }
        }
        polygonListBox.select(newSelected);
    }

    /**
     * Delete the selected polygon.
     */
    private void delete() {
        // Make sure a selection has been made.
        if (polygonListBox.getSelectionIndex() < 0) {
            return;
        }
        // Remove selected from list and apply
        polygonList.remove(polygonListBox.getSelectionIndex());
        applyPolygonList();
    }

    /**
     * Delete all polygons.
     */
    private void deleteAll() {
        // Clear the list and apply
        polygonList.clear();
        applyPolygonList();
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
    private void display(boolean display, int polygon) {
        if (polygon >= 0 && polygon < polygonList.size()) {
            RubberPolyData data = polygonList.get(polygon);
            data.setVisible(display);
            applyPolygonList();
        }
    }

    private void applyPolygonList() {
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        DisplayFieldData fieldData = displayManager.getDisplayFieldType();
        Date editDate = displayManager.getCurrentEditDate();
        PolygonEditManager.writePolygonEdits(fieldData, editDate, polygonList);
        recreatePolygonListBox();
    }

}
