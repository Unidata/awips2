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
package com.raytheon.uf.viz.archive.ui;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.archive.data.IArchiveTotals;

/**
 * Archive table composite that contains the SWT table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2013 #1964      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ArchiveTableComp extends Composite {

    /** Column to display label information. */
    private final int LABEL_COL_INDEX = 0;

    /** Column to display size information,. */
    private final int SIZE_COL_INDEX = 1;

    /** Table control. */
    private Table table;

    /** Popup menu. */
    private Menu popupMenu;

    /** Number of selected items label. */
    private Label selectedLbl;

    /** Size label. */
    private Label sizeLbl;

    /** Table type enumeration. */
    public enum TableType {
        Retention, Case
    };

    /** Current table type. */
    private final TableType type;

    /** Allows the parent dialog log to update other total displays. */
    private final IArchiveTotals iArchiveTotals;

    /** Data for the currently display table */
    private DisplayData[] tableData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param type
     *            Table type.
     */
    public ArchiveTableComp(Composite parent, TableType type,
            IArchiveTotals iTotalSelectedSize) {
        super(parent, 0);

        this.type = type;
        this.iArchiveTotals = iTotalSelectedSize;
        init();
    }

    /**
     * Initialize composites and controls.
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createTable();

        if (type != TableType.Retention) {
            createTableLabels();
        }
    }

    /**
     * Create the table control.
     */
    private void createTable() {
        GridData gd = null;

        table = new Table(this, SWT.CHECK | SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI | SWT.VIRTUAL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.widthHint = 730;
        gd.heightHint = 270;
        table.setLayoutData(gd);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);
        table.addListener(SWT.SetData, new Listener() {

            @Override
            public void handleEvent(Event event) {
                TableItem item = (TableItem) event.item;
                int index = table.indexOf(item);
                DisplayData displayData = tableData[index];
                item.setText(new String[] { displayData.getDisplayLabel(),
                        displayData.getSizeLabel() });
                item.setChecked(displayData.isSelected());
            }
        });

        TableColumn pathColumn = new TableColumn(table, SWT.LEFT);
        pathColumn.setText("Label");

        TableColumn sizeColumn = new TableColumn(table, SWT.CENTER);
        if (type == TableType.Retention) {
            sizeColumn.setText("Current Size");
        } else if (type == TableType.Case) {
            sizeColumn.setText("Size");
        }

        table.getColumn(0).setWidth(500);

        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent me) {
                if (me.button == 3) {
                    createPopupMenu(table);
                    popupMenu.setVisible(true);
                }
            }
        });

        table.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (e.detail == SWT.CHECK) {
                    updateSelectionLabels();
                }
            }
        });

        Listener sortListener = new Listener() {

            @Override
            public void handleEvent(Event event) {
                sortColumn(event);
            }
        };

        pathColumn.addListener(SWT.Selection, sortListener);
        sizeColumn.addListener(SWT.Selection, sortListener);
    }

    /**
     * Create the table labels.
     */
    private void createTableLabels() {
        Composite lblComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        lblComp.setLayout(gl);
        lblComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectedLbl = new Label(lblComp, SWT.NONE);
        selectedLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        sizeLbl = new Label(lblComp, SWT.NONE);
        sizeLbl.setLayoutData(gd);
    }

    /**
     * Sort table roles by desired column and direction.
     * 
     * @param e
     */
    private void sortColumn(Event e) {
        TableColumn sortColumn = table.getSortColumn();
        TableColumn eventColumn = (TableColumn) e.widget;

        int sortDir = table.getSortDirection();
        int index = eventColumn == table.getColumn(LABEL_COL_INDEX) ? LABEL_COL_INDEX
                : SIZE_COL_INDEX;

        if (sortColumn == eventColumn) {
            sortDir = ((sortDir == SWT.UP) ? SWT.DOWN : SWT.UP);
        } else {
            table.setSortColumn(eventColumn);
            sortDir = SWT.UP;
        }

        switch (index) {
        case LABEL_COL_INDEX:
            Arrays.sort(tableData, DisplayData.LABEL_ORDER);
            if (sortDir == SWT.DOWN) {
                ArrayUtils.reverse(tableData);
            }
            break;
        case SIZE_COL_INDEX:
            Arrays.sort(tableData, DisplayData.SIZE_ORDER);
            if (sortDir == SWT.DOWN) {
                ArrayUtils.reverse(tableData);
            }
            break;
        default:
            // Programmer error should never get here.
            throw new IndexOutOfBoundsException("Unknown column index.");
        }
        table.setSortDirection(sortDir);
        table.clearAll();
    }

    /**
     * Update the selection items labels.
     */
    private void updateSelectionLabels() {
        int count = 0;
        long tableTotalSize = 0;

        for (int index = 0; index < tableData.length; ++index) {
            DisplayData displayData = tableData[index];
            TableItem item = table.getItem(index);
            if (item.getChecked()) {
                ++count;
                displayData.setSelected(true);
                if (tableTotalSize >= 0) {
                    long diSize = displayData.getSize();
                    if (diSize < 0) {
                        tableTotalSize = diSize;
                    } else {
                        tableTotalSize += diSize;
                    }
                }
            } else {
                displayData.setSelected(false);
            }
        }
        List<DisplayData> displayDatas = Arrays.asList(tableData);

        if (selectedLbl != null) {
            selectedLbl.setText("Table Selected Items: " + count);

            String sizeString = DisplayData.UNKNOWN_SIZE_LABEL;
            if (tableTotalSize >= 0) {
                sizeString = SizeUtil.prettyByteSize(tableTotalSize);
            }
            sizeLbl.setText("Table Selected Size: " + sizeString);
        }
        iArchiveTotals.updateTotals(displayDatas);
    }

    /**
     * Create the table popup menu.
     * 
     * @param parent
     *            Parent control.
     */
    private void createPopupMenu(Control parent) {
        if (popupMenu != null) {
            popupMenu.dispose();
        }

        popupMenu = new Menu(parent);

        MenuItem checkSelectedItem = new MenuItem(popupMenu, SWT.NONE);
        checkSelectedItem.setText("Check selected rows");
        checkSelectedItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCheckSelectedRow(true);
            }
        });

        MenuItem uncheckSelectedItem = new MenuItem(popupMenu, SWT.NONE);
        uncheckSelectedItem.setText("Uncheck selected rows");
        uncheckSelectedItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCheckSelectedRow(false);
            }
        });

        new MenuItem(popupMenu, SWT.SEPARATOR);

        MenuItem checkAllItem = new MenuItem(popupMenu, SWT.NONE);
        checkAllItem.setText("Check all rows");
        checkAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCheckAll(true);
            }
        });

        MenuItem uncheckAllItem = new MenuItem(popupMenu, SWT.NONE);
        uncheckAllItem.setText("Uncheck all rows");
        uncheckAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCheckAll(false);
            }
        });

        // Set the pop-up menu as the pop-up for the shell
        parent.setMenu(popupMenu);
    }

    /**
     * Action performed when the item in the table is checked/unchecked.
     * 
     * @param check
     *            Checked flag.
     */
    private void handleCheckSelectedRow(boolean check) {
        TableItem[] itemArray = table.getSelection();
        for (TableItem ti : itemArray) {
            ti.setChecked(check);
        }

        updateSelectionLabels();
    }

    /**
     * Check or uncheck all of the table items.
     * 
     * @param check
     *            Flag indicating check or uncheck all items.
     */
    private void handleCheckAll(boolean check) {
        TableItem[] itemArray = table.getItems();

        for (TableItem ti : itemArray) {
            ti.setChecked(check);
        }

        updateSelectionLabels();
    }

    /**
     * Check the current table to see if the size of any entries needs to be
     * updated.
     * 
     * @param displayDatas
     */
    public void updateSize(List<DisplayData> displayDatas) {
        if (tableData != null && tableData.length > 0) {
            for (DisplayData displayData : displayDatas) {
                for (int index = 0; index < tableData.length; ++index) {
                    if (displayData.equals(tableData[index])) {
                        table.getItem(index)
                                .setText(displayData.getSizeLabel());
                        table.clear(index);
                    }
                }
            }
            updateSelectionLabels();
        }
    }

    /**
     * Set up table with values in the list.
     * 
     * @param displayDatas
     */
    protected void populateTable(List<DisplayData> displayDatas) {
        tableData = displayDatas.toArray(new DisplayData[0]);
        table.removeAll();
        table.setItemCount(tableData.length);

        for (int i = 0; i < 2; i++) {
            table.getColumn(i).setResizable(false);
            table.getColumn(i).setMoveable(false);
            table.getColumn(i).pack();
        }
        table.getColumn(0).setWidth(600);
        table.setSortColumn(table.getColumn(LABEL_COL_INDEX));
        table.setSortDirection(SWT.UP);
        table.clearAll();
    }
}
