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
package com.raytheon.uf.viz.gisdatastore.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.dataplugin.gfe.type.Pair;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource.IDoubleClickSelectionListener;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * GIS Attribute Viewer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012      #1326 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class AttributeViewer extends CaveJFACEDialog implements RemoveListener {
    private static final String HIGHLIGHT = "HighLight";

    private static final String VISIBLE = "Visible";

    private static Map<DataStoreResource, AttributeViewer> map = new HashMap<DataStoreResource, AttributeViewer>();

    /**
     * Open attribute viewer for the selected rsc (or bring to top if already
     * open)
     * 
     * @param shell
     * @param rsc
     * @return
     */
    public static AttributeViewer openDialog(Shell shell, DataStoreResource rsc) {
        AttributeViewer dlg = map.get(rsc);
        if (dlg == null) {

            dlg = new AttributeViewer(shell, rsc);
            dlg.open();
        } else {
            dlg.bringToTop();
        }
        return dlg;
    }

    private static final int DEFAULT_WIDTH = 900;

    private static final int MAX_INITIAL_COLUMN_WIDTH = DEFAULT_WIDTH / 4;

    protected static final int MINIMUM_COLUMN_WIDTH = 10;

    /**
     * Provides text for the columns in the attribute viewer from the attributes
     * array
     * 
     */
    private class TableLabelProvider extends LabelProvider implements
            ITableLabelProvider {

        @Override
        public Image getColumnImage(Object element, int columnIndex) {
            return null;
        }

        @Override
        public String getColumnText(Object element, int columnIndex) {
            return String.valueOf(((Object[]) element)[columnIndex]);
        }
    }

    /**
     * Compares rows using values from the selected column and direction for
     * sorting the table in the desired order.
     * 
     */
    private class ColumnComparator extends ViewerComparator {
        private List<Pair<String, Integer>> sortOrder;

        @SuppressWarnings("unchecked")
        @Override
        public void sort(Viewer viewer, Object[] elements) {
            Arrays.sort(elements, new Comparator<Object>() {
                @Override
                public int compare(Object a, Object b) {
                    List<String> nameList = Arrays.asList(names);
                    int retval = 0;
                    for (Pair<String, Integer> p : sortOrder) {
                        String field = p.getFirst();
                        int column = nameList.indexOf(field);
                        int direction = p.getSecond();

                        Object aObj = ((Object[]) a)[column];
                        Object bObj = ((Object[]) b)[column];

                        if ((aObj instanceof Comparable)
                                && (bObj instanceof Comparable)) {
                            retval = ((Comparable<Object>) aObj)
                                    .compareTo(bObj);
                        } else {
                            retval = String.valueOf(aObj).compareTo(
                                    String.valueOf(bObj));
                        }
                        retval *= direction;

                        if (retval != 0) {
                            return retval;
                        }
                    }
                    return retval;
                }
            });
        }

        /**
         * Returns the sort order as a pair of column names and integers
         * indicating sort direction (1 for ascending, -1 for descending)
         * 
         * @return the sort order
         */
        public List<Pair<String, Integer>> getSortOrder() {
            return sortOrder;
        }

        /**
         * Set the sort order as a pair of column names and integers indicating
         * sort direction (1 for ascending, -1 for descending)
         * 
         * @param sortOrder
         */
        public void setSortOrder(List<Pair<String, Integer>> sortOrder) {
            this.sortOrder = sortOrder;
        }
    }

    private ColumnComparator comparator;

    private DataStoreResource rsc;

    private String title;

    private String[] names;

    private Object[][] attributes;

    private Shell parentShell;

    private TableViewer viewer;

    protected String[] selectedColumns;

    SortOrderDialog sortDlg;

    protected ColumnSelectDialog selectDlg;

    private int[] columnWidth;

    private IDoubleClickSelectionListener selectedListener;

    /**
     * Create an attribute viewer for the specified DataStoreResource
     * 
     * @param parentShell
     *            (only used for positioning the dialog)
     * @param rsc
     *            the DataStoreResource
     */
    public AttributeViewer(Shell parentShell, DataStoreResource rsc) {
        // TODO: make open centered in parentShell
        super(null);
        this.setShellStyle(SWT.SHELL_TRIM | SWT.MENU);
        this.rsc = rsc;
        this.title = rsc.getName();
        this.names = rsc.getAttributeNames();
        this.selectedColumns = this.names;
        this.parentShell = parentShell;

        // TODO: may need to deep copy this in case it changes out from under us
        this.attributes = rsc.getAttributes();

        map.put(rsc, this);
        rsc.getDescriptor().getResourceList().addPreRemoveListener(this);
    }

    @Override
    protected boolean isResizable() {
        return true;
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Attributes: " + this.title);

        Menu menuBar = new Menu(newShell, SWT.BAR);

        MenuItem annotationMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        annotationMenuHeader.setText("Annotation");

        Menu annotationMenu = new Menu(newShell, SWT.DROP_DOWN);
        annotationMenuHeader.setMenu(annotationMenu);

        MenuItem makeAllVisible = new MenuItem(annotationMenu, SWT.PUSH);
        makeAllVisible.setText("Make All Visible");
        makeAllVisible.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                makeVisible(true, viewer.getTable().getItems());
            }
        });

        MenuItem clearHighlights = new MenuItem(annotationMenu, SWT.PUSH);
        clearHighlights.setText("Clear Highlights");
        clearHighlights.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                highlight(false, viewer.getTable().getItems());
            }
        });

        MenuItem dataMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        dataMenuHeader.setText("Data");
        Menu dataMenu = new Menu(newShell, SWT.DROP_DOWN);
        dataMenuHeader.setMenu(dataMenu);

        MenuItem columnsItem = new MenuItem(dataMenu, SWT.PUSH);
        columnsItem.setText("Select Columns...");
        columnsItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                selectColumns();
            }
        });

        MenuItem sortItem = new MenuItem(dataMenu, SWT.PUSH);
        sortItem.setText("Sort...");
        sortItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                sortColumns();
            }
        });

        newShell.setMenuBar(menuBar);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout();
        composite.setLayout(layout);

        viewer = new TableViewer(composite, SWT.MULTI | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL | SWT.FULL_SELECTION
                | SWT.VIRTUAL);
        Table table = viewer.getTable();

        table.setHeaderVisible(true);
        // table.setLinesVisible(true);
        table.setSortDirection(SWT.UP);
        comparator = new ColumnComparator();
        ArrayList<Pair<String, Integer>> sortOrder = new ArrayList<Pair<String, Integer>>(
                1);
        comparator.setSortOrder(sortOrder);
        viewer.setComparator(comparator);

        int i = 0;
        GC gc = new GC(table);
        columnWidth = new int[names.length];
        int totalWidth = 0;
        for (String attrName : names) {
            int index = i++;
            int alignment = SWT.LEFT;
            if (this.attributes[0][index] instanceof Integer) {
                alignment = SWT.RIGHT;
            }
            TableViewerColumn tvc = new TableViewerColumn(viewer, alignment,
                    index);
            final TableColumn column = tvc.getColumn();
            column.setText(attrName);
            // column.addSelectionListener(new
            // ColumnSelectionAdapter(tableViewer,
            // index));
            column.pack();
            int extent = column.getWidth();
            for (Object[] atts : this.attributes) {
                int width = gc.stringExtent(String.valueOf(atts[index])).x + 10;

                if (width > MAX_INITIAL_COLUMN_WIDTH) {
                    extent = MAX_INITIAL_COLUMN_WIDTH;
                    break;
                } else if (width > extent) {
                    extent = width;
                }
            }
            column.setWidth(extent);
            columnWidth[index] = extent;
            column.addControlListener(new ControlAdapter() {

                @Override
                public void controlResized(ControlEvent e) {
                    TableColumn column = (TableColumn) e.widget;
                    int index = viewer.getTable().indexOf(column);
                    int width = column.getWidth();
                    if (width > MINIMUM_COLUMN_WIDTH) {
                        columnWidth[index] = column.getWidth();
                    }

                    if (width == 0) {
                        List<String> newSelected = new ArrayList<String>(Arrays
                                .asList(selectedColumns));
                        newSelected.remove(names[index]);
                        selectedColumns = newSelected
                                .toArray(new String[newSelected.size()]);
                    }
                }

            });
            totalWidth += extent;
        }
        gc.dispose();
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        if (totalWidth > DEFAULT_WIDTH) {
            layoutData.widthHint = DEFAULT_WIDTH;
        }
        layoutData.heightHint = table.getItemHeight() * 5;
        table.setLayoutData(layoutData);

        IContentProvider contentProvider = new ArrayContentProvider();
        viewer.setLabelProvider(new TableLabelProvider());
        viewer.setContentProvider(contentProvider);
        viewer.setInput(this.attributes);

        table.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                Table table = (Table) e.widget;
                TableItem[] selected = table.getSelection();
                boolean highlight = (Boolean) selected[0].getData(HIGHLIGHT);
                if (!highlight) {
                    recenter(selected[0]);
                }
                highlight(!highlight, selected);
            }
        });

        table.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseDown(MouseEvent e) {
                Table table = (Table) e.widget;
                if (e.button == 3) {
                    Point p = new Point(e.x, e.y);
                    TableItem clicked = table.getItem(p);
                    TableItem[] selected = table.getSelection();
                    createPopupMenu(table, clicked, selected);
                }
            }

        });

        Label label = new Label(composite, SWT.NONE);
        label.setText(this.attributes.length + " rows");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        label.setLayoutData(layoutData);

        initializeColors();

        selectedListener = new IDoubleClickSelectionListener() {

            @Override
            public void selectedFeaturesChanged(List<String> selectedIds) {
                Table table = viewer.getTable();
                int i = 0;
                table.deselectAll();
                for (TableItem item : table.getItems()) {
                    if (selectedIds.contains(item.getText(0))) {
                        table.select(i);
                    }
                    i++;
                }
                if (table.getSelectionCount() > 0) {
                    table.showItem(table.getSelection()[0]);
                }
            }
        };

        table.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                rsc.removeDoubleClickSelectionListener(selectedListener);
            }
        });
        rsc.addDoubleClickSelectionListener(selectedListener);

        return composite;
    }

    private void initializeColors() {
        for (TableItem item : viewer.getTable().getItems()) {
            String id = item.getText(0);
            item.setData(VISIBLE, rsc.getVisible(id));
            item.setData(HIGHLIGHT, rsc.getHighlighted(id));
            setColors(item);
        }
    }

    private void createPopupMenu(Table table, final TableItem clicked,
            final TableItem[] selected) {
        Menu popupMenu = new Menu(table);

        MenuItem visible = new MenuItem(popupMenu, SWT.CHECK);
        visible.setText("Visible");
        visible.setSelection((Boolean) clicked.getData(VISIBLE));
        visible.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                MenuItem item = (MenuItem) e.widget;
                makeVisible(item.getSelection(), selected);
                item.getParent().dispose();
            }
        });

        MenuItem highLight = new MenuItem(popupMenu, SWT.CHECK);
        highLight.setText("Highlighted");
        highLight.setSelection((Boolean) clicked.getData(HIGHLIGHT));
        highLight.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                MenuItem item = (MenuItem) e.widget;
                highlight(item.getSelection(), selected);
                item.getParent().dispose();
            }
        });
        popupMenu.setVisible(true);
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        return null;
    }

    @Override
    public boolean close() {
        map.remove(this.rsc);
        return super.close();
    }

    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc instanceof DataStoreResource) {
            AttributeViewer dlg = map.remove(rsc);
            if (dlg != null) {
                dlg.close();
            }
        }
    }

    private void makeVisible(boolean visible, TableItem[] items) {
        for (TableItem item : items) {
            item.setData(VISIBLE, visible);
            setColors(item);
            rsc.setVisible(item.getText(0), visible);
        }
    }

    private void highlight(boolean highlight, TableItem[] items) {
        for (TableItem item : items) {
            item.setData(HIGHLIGHT, highlight);
            setColors(item);
            rsc.setHighlighted(item.getText(0), highlight);
        }
    }

    private void recenter(TableItem item) {
        rsc.recenter(item.getText(0));
    }

    private void setColors(TableItem item) {
        boolean visible = (Boolean) item.getData(VISIBLE);
        boolean highlight = (Boolean) item.getData(HIGHLIGHT);

        if (visible) {
            if (highlight) {
                item.setBackground(item.getDisplay().getSystemColor(
                        SWT.COLOR_YELLOW));
            } else {
                item.setBackground(null);
            }
        } else {
            item.setBackground(item.getDisplay().getSystemColor(SWT.COLOR_GRAY));
        }
    }

    private void selectColumns() {
        if (selectDlg == null) {
            selectDlg = new ColumnSelectDialog(getShell(), names,
                    selectedColumns);
            selectDlg.setBlockOnOpen(false);
            selectDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    int returnCode = (Integer) returnValue;
                    if (returnCode == Window.OK) {
                        String[] selectedColumns = selectDlg
                                .getSelectedColumns();
                        setSelectedColumns(selectedColumns);
                        viewer.refresh();
                    }
                    selectDlg = null;
                }
            });
            selectDlg.open();
        } else {
            selectDlg.bringToTop();
        }
    }

    /**
     * Set the columns displayed in the desired order.
     * 
     * @param selectedColumns
     */
    private void setSelectedColumns(String[] selectedColumns) {
        this.selectedColumns = selectedColumns;
        List<String> namesList = Arrays.asList(names);
        List<String> selectedList = Arrays.asList(selectedColumns);
        List<String> unselected = new ArrayList<String>(namesList);
        unselected.removeAll(selectedList);
        List<String> orderedList = new ArrayList<String>(names.length);
        orderedList.addAll(selectedList);
        orderedList.addAll(unselected);
        int[] order = new int[names.length];
        for (int i = 0; i < names.length; i++) {
            order[i] = namesList.indexOf(orderedList.get(i));
            TableColumn column = viewer.getTable().getColumn(i);
            if (!selectedList.contains(names[i])) {
                column.setWidth(0);
            } else {
                column.setWidth(columnWidth[i]);
            }
        }
        viewer.getTable().setColumnOrder(order);
    }

    private void sortColumns() {

        if (sortDlg == null) {
            sortDlg = new SortOrderDialog(getShell(), names,
                    comparator.getSortOrder());
            sortDlg.setBlockOnOpen(false);
            sortDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    int returnCode = (Integer) returnValue;
                    if (returnCode == Window.OK) {
                        List<Pair<String, Integer>> sortOrder = sortDlg
                                .getSortOrder();
                        comparator.setSortOrder(sortOrder);
                        viewer.refresh();
                    }
                    sortDlg = null;
                }
            });
            sortDlg.open();
        } else {
            sortDlg.bringToTop();
        }
    }
}
