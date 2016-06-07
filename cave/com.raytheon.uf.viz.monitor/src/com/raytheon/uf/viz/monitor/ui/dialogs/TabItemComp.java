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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.MonitorAreaThresholds;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr.ThresholdKey;

/**
 * Abstract class is the foundation for a Tab Folders tab item control
 * (Composite).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 06, 2009           lvenable  Initial creation
 * Aug 05, 2010  6396     wkwock    Change the layout of threshold edit dialog
 * Nov 07, 2013  16703    gzhang    Check in code for Lee for FFMP and Safeseas
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class TabItemComp extends Composite {
    /**
     * Parent tabl folder.
     */
    private TabFolder parent;

    private boolean involveSwell = false;

    protected boolean rankSwellPeriodHigh = false;

    /**
     * Data table control.
     */
    protected Table dataTable;

    /**
     * Group composite
     */
    private Composite groupComp;

    /**
     * Header composite
     */
    private Composite headerComp;

    /**
     * Edit button.
     */
    private Button editBtn;

    protected DataUsageKey duKey;

    protected ThresholdKey threshKeyR = ThresholdKey.RED;

    protected ThresholdKey threshKeyY = ThresholdKey.YELLOW;

    protected String dataFmt = " %5s";

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent - tab folder.
     * @param duKey
     */
    public TabItemComp(TabFolder parent, DataUsageKey duKey) {
        super(parent, 0);

        this.parent = parent;

        this.duKey = duKey;

        init();
    }

    /**
     * Constructor with swell
     * 
     * @param parent
     * @param duKey
     * @param involveSwell
     */
    public TabItemComp(TabFolder parent, DataUsageKey duKey,
            Boolean involveSwell) {
        super(parent, 0);
        this.parent = parent;
        this.duKey = duKey;
        this.involveSwell = involveSwell;
        init();
    }

    /**
     * Initialize method to setup the data table and control buttons
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createDataTable();
        if (involveSwell) {
            createRankSwellPeriodHighLowRadios();
        }
        createControlButtons();
    }

    private void createRankSwellPeriodHighLowRadios() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite radioComp = new Composite(this, SWT.NONE);
        radioComp.setLayout(new GridLayout(1, false));
        radioComp.setLayoutData(gd);

        Button rankLowRdo = new Button(radioComp, SWT.RADIO);
        rankLowRdo.setText("Rank Swell Period Low");
        if (!MonitorAreaThresholds.isRankHighSwellPeriods()) {
            rankLowRdo.setSelection(true);
        } else {
            rankLowRdo.setSelection(false);
        }
        rankLowRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                rankSwellPeriodHigh = false;
                populateTable();
            }
        });

        Button rankHighRdo = new Button(radioComp, SWT.RADIO);
        rankHighRdo.setText("Rank Swell Period High");
        if (MonitorAreaThresholds.isRankHighSwellPeriods()) {
            rankHighRdo.setSelection(true);
        } else {
            rankHighRdo.setSelection(false);
        }
        rankHighRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                rankSwellPeriodHigh = true;
                populateTable();
            }
        });

    }

    /**
     * Create the threshold data table control.
     */
    private void createDataTable() {
        Composite comp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        comp.setLayout(gl);
        comp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, false, false));

        groupComp = new Composite(comp, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        groupComp.setLayoutData(gd);

        headerComp = new Composite(comp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        headerComp.setLayoutData(gd);

        dataTable = new Table(comp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = dataTable.getItemHeight() * 12;
        dataTable.setLayoutData(gd);
        dataTable.setLinesVisible(true);

        populateTable();

        gl = new GridLayout(dataTable.getColumnCount(), false);
        gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        groupComp.setLayout(gl);

        gl = new GridLayout(dataTable.getColumnCount(), false);
        gl.horizontalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        headerComp.setLayout(gl);

        createListHeader();
    }

    /**
     * Create the Select/Deselect/Edit control buttons.
     */
    private void createControlButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(this, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(mainButtonComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Button selectAllBtn = new Button(buttonComp, SWT.PUSH);
        selectAllBtn.setText("Select All");
        selectAllBtn.setLayoutData(gd);
        selectAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataTable.selectAll();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button deselectAllBtn = new Button(buttonComp, SWT.PUSH);
        deselectAllBtn.setText("Deselect All");
        deselectAllBtn.setLayoutData(gd);
        deselectAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataTable.deselectAll();
            }
        });

        gd = new GridData(170, SWT.DEFAULT);
        editBtn = new Button(buttonComp, SWT.PUSH);
        editBtn.setText("Edit Selected Areas...");
        editBtn.setLayoutData(gd);
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (dataTable.getSelectionCount() == 0) {
                    MessageBox mb = new MessageBox(parent.getShell(),
                            SWT.ICON_WARNING | SWT.OK);
                    mb.setText("Warning");
                    mb.setMessage("Please select data from the list to be edited.");
                    mb.open();

                    return;
                }

                editDataAction();
            }
        });
    }

    /**
     * Create a group header
     * 
     */
    protected void createGroupHeader(String text, int startCol, int endCol,
            boolean border) {
        Composite header = new Composite(groupComp, (border ? SWT.BORDER
                : SWT.NONE));
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        int width = 0;
        for (int c = startCol; c <= endCol; c++) {
            width += dataTable.getColumn(c).getWidth();
        }
        Rectangle trim = header.computeTrim(0, 0, 0, 0);
        gd.widthHint = width - trim.x - trim.width;
        gd.horizontalSpan = endCol - startCol + 1;
        header.setLayoutData(gd);
        header.setLayout(new GridLayout(2, true));

        Label label = new Label(header, SWT.CENTER);
        label.setText(text);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        label.setLayoutData(gd);
    }

    /**
     * Create a title label and the R/Y labels.
     */
    protected void createHeader(String text, int startCol, int endCol,
            boolean border) {

        Composite header = new Composite(headerComp, (border ? SWT.BORDER
                : SWT.NONE));
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        int width = 0;
        for (int c = startCol; c <= endCol; c++) {
            width += dataTable.getColumn(c).getWidth();
        }
        Rectangle trim = header.computeTrim(0, 0, 0, 0);
        gd.widthHint = width - trim.x - trim.width;
        gd.horizontalSpan = endCol - startCol + 1;
        header.setLayoutData(gd);
        header.setLayout(new GridLayout(2, true));

        Label label = new Label(header, SWT.CENTER);
        label.setText(text);
        gd = new GridData(SWT.CENTER, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        label.setLayoutData(gd);

        if (!text.isEmpty()) {
            String[] colorChar;
            int[] color;
            if (text.contains("(from)")) {
                colorChar = new String[] { "y", "r" };
                color = new int[] { SWT.COLOR_YELLOW, SWT.COLOR_RED };
            } else {
                colorChar = new String[] { "r", "y" };
                color = new int[] { SWT.COLOR_RED, SWT.COLOR_YELLOW };
            }

            for (int i = 0; i < 2; i++) {
                gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                Label lbl = new Label(header, SWT.CENTER);
                lbl.setText(colorChar[i]);
                lbl.setBackground(getDisplay().getSystemColor(color[i]));
                lbl.setLayoutData(gd);
            }
        }
    }

    /**
     * Pack the list controls.
     */
    protected void packListControls() {
        for (TableColumn column : dataTable.getColumns()) {
            column.pack();
        }
    }

    /**
     * Format and set the red and yellow values into the table item
     * 
     * @param item
     *            the table item
     * @param column
     *            starting column
     * @param rValue
     *            Red value.
     * @param yVal
     *            Yellow value.
     */
    protected void appendIntData(TableItem item, int column, double rValue,
            double yVal) {
        int intVal = (int) rValue;
        item.setText(column, String.format(dataFmt, String.valueOf(intVal)));

        intVal = (int) yVal;
        item.setText(column + 1, String.format(dataFmt, String.valueOf(intVal)));
    }

    protected void appendDecimalData(TableItem item, int column, double rValue,
            double yVal) {
        item.setText(column, String.format(dataFmt, String.valueOf(rValue)));

        item.setText(column + 1, String.format(dataFmt, String.valueOf(yVal)));
    }

    /**
     * Create a list header.
     * 
     */
    protected abstract void createListHeader();

    /**
     * Action for the edit data button.
     */
    protected abstract void editDataAction();

    /**
     * Populate the data table.
     */
    protected abstract void populateTable();

    /**
     * Reload data
     */
    public abstract void reloadData();

    /**
     * Commit data to XML
     */
    public abstract void commitDataToXML();
}
