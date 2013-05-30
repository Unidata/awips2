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

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager.DisplayData;
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

    /** Key for getting Display Information state. */
    private final String DISPLAY_INFO_KEY = "displayInfo";

    /** Table control. */
    private Table table;

    /** Popup menu. */
    private Menu popupMenu;

    /** Number of selected items label. */
    private Label selectedLbl;

    /** Total selected items for all tables */
    private Label totalSelectedLbl;

    /** Size label. */
    private Label sizeLbl;

    /** Table type enumeration. */
    public enum TableType {
        Retention, Case
    };

    /** Current table type. */
    private TableType tableType = TableType.Retention;

    private IArchiveTotals iTotalSelectedSize;

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

        tableType = type;
        this.iTotalSelectedSize = iTotalSelectedSize;
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
        createTableLabels();

        updateSelectionLabels();
    }

    /**
     * Create the table control.
     */
    private void createTable() {
        GridData gd = null;

        table = new Table(this, SWT.CHECK | SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.widthHint = 730;
        gd.heightHint = 270;
        table.setLayoutData(gd);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        TableColumn pathColumn = new TableColumn(table, SWT.LEFT);
        pathColumn.setText("Label");

        TableColumn sizeColumn = new TableColumn(table, SWT.CENTER);
        if (tableType == TableType.Retention) {
            sizeColumn.setText("Current Size");
        } else if (tableType == TableType.Case) {
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
                updateSelectionLabels();
            }
        });
    }

    /**
     * Create the table labels.
     */
    private void createTableLabels() {
        Composite lblComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        lblComp.setLayout(gl);
        lblComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectedLbl = new Label(lblComp, SWT.NONE);
        selectedLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        sizeLbl = new Label(lblComp, SWT.NONE);
        sizeLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        totalSelectedLbl = new Label(lblComp, SWT.NONE);
        totalSelectedLbl.setLayoutData(gd);
    }

    /**
     * Update the selection items labels.
     */
    private void updateSelectionLabels() {
        TableItem[] itemArray = table.getItems();
        int count = 0;
        long tableTotalSize = 0;

        for (TableItem ti : itemArray) {
            DisplayData displayInfo = (DisplayData) ti
                    .getData(DISPLAY_INFO_KEY);
            if (ti.getChecked()) {
                ++count;
                displayInfo.setSelected(true);
                if (tableTotalSize >= 0) {
                    long diSize = displayInfo.getSize();
                    if (diSize < 0) {
                        tableTotalSize = diSize;
                    } else {
                        tableTotalSize += diSize;
                    }
                }
            } else {
                displayInfo.setSelected(false);
            }
        }

        selectedLbl.setText("Table Selected Items: " + count);
        int totalSelectedSize = iTotalSelectedSize.getTotalSelectedItems();
        totalSelectedLbl.setText("Total Selected Items: " + totalSelectedSize);

        String sizeString = "????";
        if (tableTotalSize >= 0) {
            sizeString = SizeUtil.prettyByteSize(tableTotalSize);
        }
        sizeLbl.setText("Table Selected Size: " + sizeString);
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
        System.out.println("handleCheckSelectedRow: " + check);
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

    public void updateSize(List<DisplayData> displayInfos) {
        TableItem[] itemArray = table.getItems();

        for (DisplayData displayInfo : displayInfos) {
            for (TableItem ti : itemArray) {
                if (displayInfo.equals(ti.getData(DISPLAY_INFO_KEY))) {
                    setItemSize(ti);
                }
            }
        }
    }

    /*
     * TODO : this is just for display purposes. This will go away when the
     * functionality is implemented.
     */
    protected void populateTable(List<DisplayData> displayInfoArray) {
        table.removeAll();
        for (DisplayData displayInfo : displayInfoArray) {
            TableItem item = new TableItem(table, SWT.NONE);
            item.setData(DISPLAY_INFO_KEY, displayInfo);
            item.setChecked(displayInfo.isSelected());
            item.setText(0, displayInfo.getDisplayLabel());

            item.setChecked(displayInfo.isSelected());
            setItemSize(item);
        }
        for (int i = 0; i < 2; i++) {
            table.getColumn(i).setResizable(false);
            table.getColumn(i).setMoveable(false);
            table.getColumn(i).pack();
        }
        table.getColumn(0).setWidth(600);
        updateSelectionLabels();
    }

    private void setItemSize(TableItem item) {
        long size = ((DisplayData) item.getData(DISPLAY_INFO_KEY)).getSize();
        if (size < 0L) {
            item.setText(1, "????");
        } else {
            item.setText(1, SizeUtil.prettyByteSize(size));
        }
    }
}
