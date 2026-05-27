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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.alertviz.ConfigurationManager;
import com.raytheon.uf.viz.alertviz.INeedsSaveListener;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration.TrayMode;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertVizTips.TIP;

/**
 * This class displays category controls and gives the user the ability to
 * determine which category message appear in which text controls.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Oct 05, 2008           lvenable   Initial creation.
 * Nov 23, 2010  2235     cjeanbap   Clear All Layouts needs to refresh the
 *                                   list.
 * Nov 23, 2010  6343     cjeanbap   Add if-statement in initControls() to
 *                                   correct layout functionality.
 * Jan 28, 2011  4617     cjeanbap   Added Monitor Only functionality.
 * Feb 03, 2011  4617     cjeanbap   Add getSelectedLayoutTrayMode();
 * Mar 24, 2011  5853     cjeanbap   Add createLayoutControls() to
 *                                   reloadConfig().
 * May 02, 2011  9067     cjeanbap   Remove createLayoutControls() from
 *                                   reloadConfig().
 * Feb 07, 2013  15490    Xiaochuan  Add configDialog to handle the updated
 *                                   setting on Category layers.
 * Apr 19, 2016  5517     randerso   Fix GUI sizing issues
 * Sep 14, 2018  7459     tgurney    Fix index out of bounds exception
 * Sep 24, 2018  7481     randerso   Converted category list to table. Moved
 *                                   Clear All Layouts button under Remove
 *                                   Selection button for better logical
 *                                   grouping and tighter dialog layout. Code
 *                                   Cleanup.
 * Sep 26, 2018  7491     dgilling   Re-select previously selected item in
 *                                   Category list when making changes.
 * Sep 26, 2018  7492     dgilling   Ensure changes to layout are not lost when
 *                                   closing config dialog.
 * Oct 01, 2018  7455     randerso   Changed updateCellNumbers to use
 *                                   TrayMode.getNumberOfBoxes.
 * Oct 08, 2018  7514     randerso   Added sorting for Category table
 * Mar 13, 2019  7763     randerso   Moved tool tip text to a separate container
 *                                   class. Added additional tool tips that were
 *                                   missing.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class LayoutControlsComp extends Composite implements MouseListener {

    /**
     * Drawing canvas.
     */
    private Canvas canvas;

    /**
     * Combo box containing the different text control layouts.
     */
    private Combo layoutCombo;

    /**
     * Category list control.
     */
    private Table categoryTable;

    /**
     * Selected mode rectangles.
     */
    private List<Rectangle> selectedModeRecs;

    /**
     * Remove selection buttons.
     */
    private Button removeSelectionBtn;

    /**
     * Delete button
     */
    private Button deleteBtn;

    /**
     * Selected cell.
     */
    private int selectedCell = 0;

    /**
     * Layout text string.
     */
    private static final String layoutText = "Message Text Layout:";

    /**
     * New category dialog.
     */
    private NewSourceCategoryDlg newSrcCatDlg;

    /**
     * Selected mode.
     */
    private TrayConfiguration.TrayMode selectedMode;

    /**
     * Configuration data.
     */
    private Configuration configData;

    /**
     * Map of all the categories.
     */
    private Map<String, Category> categoryMap;

    /**
     * Array of category names.
     */
    private ArrayList<String> categoryNames;

    private Menu popupMenuCList;

    private MenuItem menuItem;

    private INeedsSaveListener needsSaveListener;

    private AlertVisConfigDlg configDialog;

    /**
     * Constructor.
     *
     * @param parentComp
     *            Parent composite.
     * @param configData
     *            Configuration data.
     * @param needsSaveListener
     * @param configDialog
     */
    public LayoutControlsComp(Composite parentComp,
            INeedsSaveListener needsSaveListener,
            AlertVisConfigDlg configDialog) {
        super(parentComp, SWT.NONE);
        this.setToolTipText(parentComp.getToolTipText());

        this.needsSaveListener = needsSaveListener;
        this.configDialog = configDialog;

        init();
    }

    /**
     * Initialize method.
     */
    public void init() {
        GridLayout gl = new GridLayout(2, true);
        this.setLayout(gl);
        this.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        createCategoryListControls();
        createLayoutControls();
    }

    /**
     * Initialize values.
     */
    private void initValues() {
        categoryMap = configData.getCategories();
        categoryNames = new ArrayList<>();
    }

    /**
     * Update when configData changes
     *
     * @param configData
     */
    public void reloadConfig(Configuration configData) {
        this.configData = configData;

        initValues();
        populateCategoryList();
        updateLayoutCombo();
        handleSourceSelection();
    }

    /**
     * Create the category list controls.
     */
    private void createCategoryListControls() {
        Composite listComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        listComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        listComp.setLayoutData(gd);

        categoryTable = new Table(listComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.FULL_SELECTION);
        categoryTable.setHeaderVisible(true);
        TableColumn categoryColumn = new TableColumn(categoryTable, SWT.LEFT);
        categoryColumn.setText("Category");
        categoryColumn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setSortColumn((TableColumn) e.getSource());
            }
        });
        categoryTable.setToolTipText(AlertVizTips.getTip(TIP.CATEGORIES));

        TableColumn cellColumn = new TableColumn(categoryTable, SWT.CENTER);
        cellColumn.setText("Cell #");
        cellColumn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setSortColumn((TableColumn) e.getSource());
            }
        });
        setSortColumn(categoryColumn);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = categoryTable.getHeaderHeight()
                + categoryTable.getItemHeight() * 8
                - categoryTable.getHorizontalBar().getSize().y;
        gd.horizontalSpan = 2;
        categoryTable.setLayoutData(gd);

        categoryTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleSourceSelection();
            }
        });

        int buttonWidth = listComp.getDisplay().getDPI().x;

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = buttonWidth;
        Button newBtn = new Button(listComp, SWT.PUSH);
        newBtn.setText("New...");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                createNewCategory();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = buttonWidth;
        deleteBtn = new Button(listComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteCategory();
            }
        });

        // Right-click pop-up menu
        popupMenuCList = new Menu(categoryTable);
        categoryTable.setMenu(popupMenuCList);
        popupMenuCList.addListener(SWT.Show, new Listener() {
            @Override
            public void handleEvent(Event event) {
                MenuItem[] menuItems = popupMenuCList.getItems();

                for (MenuItem m : menuItems) {
                    m.dispose();
                }

                if (!categoryMap.get(getListIndexToKey()).isLocked()) {
                    menuItem = new MenuItem(popupMenuCList, SWT.PUSH);
                    menuItem.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            deleteCategory();
                        }
                    });
                    menuItem.setText("Delete");
                }
            }
        });
    }

    /**
     * Create layout controls.
     */
    private void createLayoutControls() {
        Composite controlComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        controlComp.setLayoutData(gd);
        controlComp.setToolTipText(this.getToolTipText());

        Label layoutComboLbl = new Label(controlComp, SWT.NONE);
        layoutComboLbl.setText("Layout:");

        layoutCombo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateLayoutCombo();
        layoutCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = layoutCombo.getSelectionIndex();
                selectedMode = TrayConfiguration.TrayMode
                        .valueOf(layoutCombo.getItem(index));
                selectedModeRecs = computeRectangles(selectedMode);
                canvas.redraw();
                updateCellNumbers();
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 15;
        Label lbl = new Label(controlComp, SWT.NONE);
        lbl.setText("Click cell to add to category:");
        lbl.setLayoutData(gd);

        addCanvas(controlComp);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        removeSelectionBtn = new Button(controlComp, SWT.PUSH);
        removeSelectionBtn.setText("Remove Selection");
        removeSelectionBtn.setEnabled(false);
        removeSelectionBtn.setLayoutData(gd);
        removeSelectionBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = categoryTable.getSelectionIndex();
                Category cat = categoryMap.get(getListIndexToKey());
                cat.setTextBox(0);
                categoryTable.getItem(index).setText(1, getCellString(cat));
                selectedCell = 0;
                removeSelectionBtn.setEnabled(false);
                canvas.redraw();
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Button clearAllBtn = new Button(controlComp, SWT.PUSH);
        clearAllBtn.setText("Clear All Layouts");
        clearAllBtn.setLayoutData(gd);
        clearAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearAllCategoryTextBoxes();
            }
        });

        if (categoryTable.getSelectionIndex() != -1) {
            removeSelectionBtn.setEnabled(true);
        }
    }

    /**
     * Add the canvas to the display.
     *
     * @param parent
     *            Parent composite.
     */
    private void addCanvas(Composite parent) {
        canvas = new Canvas(parent, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = computeCanvasHeight();
        gd.horizontalSpan = 2;
        canvas.setLayoutData(gd);

        canvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        canvas.addMouseListener(this);
    }

    /**
     * Draw the canvas.
     *
     * @param gc
     *            Graphic context.
     */
    private void drawCanvas(GC gc) {
        Point canvasSize = canvas.getSize();

        gc.setBackground(
                getDisplay().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(0, 0, canvasSize.x, canvasSize.y);

        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        Point extent = gc.textExtent(layoutText);
        gc.drawString(layoutText, (canvasSize.x - extent.x) / 2, 2, true);

        drawRectangles(gc);
    }

    /**
     * Draw the rectangles.
     *
     * @param gc
     *            Graphic context.
     */
    private void drawRectangles(GC gc) {
        gc.setLineWidth(3);
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        for (int i = 0; i < selectedModeRecs.size(); i++) {
            if ((selectedCell - 1) == i) {
                gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
            } else {
                gc.setBackground(getDisplay()
                        .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
            }
            Rectangle rect = selectedModeRecs.get(i);
            gc.fillRectangle(rect);
            gc.drawRectangle(rect);

            String text = Integer.toString(i + 1);
            Point extent = gc.textExtent(text);
            gc.drawText(text, rect.x + (rect.width - extent.x) / 2,
                    rect.y + (rect.height - extent.y) / 2, true);
        }

        gc.setBackground(
                getDisplay().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
    }

    /**
     * Populate the layout combo box.
     */
    private void populateLayoutCombo() {
        for (TrayConfiguration.TrayMode m : TrayConfiguration.TrayMode
                .values()) {
            String str = m.name();
            layoutCombo.add(str);
        }
    }

    private void updateLayoutCombo() {

        selectedMode = configData.getGlobalConfiguration().getMode();

        layoutCombo.select(selectedMode.ordinal());
        selectedModeRecs = computeRectangles(selectedMode);
        String key = getListIndexToKey();
        if (key != null && !key.isEmpty()) {
            selectedCell = categoryMap.get(key).getTextBox();
        } else {
            selectedCell = 0;
        }
    }

    private int computeCanvasHeight() {
        int maxVerticalRects = 0;
        for (TrayMode mode : TrayMode.values()) {
            maxVerticalRects = Math.max(maxVerticalRects,
                    mode.getVerticalBoxes());
        }

        GC gc = new GC(canvas);
        Point charSize = gc.textExtent("0");
        gc.dispose();

        int canvasHeight = (maxVerticalRects + 1) * (charSize.y * 3 / 2);
        return canvasHeight;
    }

    private List<Rectangle> computeRectangles(TrayMode mode) {
        List<Rectangle> rectangles = new ArrayList<>();
        GC gc = new GC(canvas);
        Point charSize = gc.textExtent("0");
        gc.dispose();

        Point canvasSize = canvas.getSize();
        int rectGap = charSize.x;
        int rectHeight = charSize.y;
        int rectWidth = (canvasSize.x
                - (rectGap * (mode.getHorizontalBoxes() + 1)))
                / mode.getHorizontalBoxes();

        int ry = rectHeight * 3 / 2;
        for (int y = 0; y < mode.getVerticalBoxes(); y++) {
            int rx = rectGap;
            for (int x = 0; x < mode.getHorizontalBoxes(); x++) {
                rectangles.add(new Rectangle(rx, ry, rectWidth, rectHeight));
                rx += rectWidth + rectGap;
            }
            ry += rectHeight * 3 / 2;
        }

        return rectangles;
    }

    /**
     * Mouse double-click. (Not used)
     */
    @Override
    public void mouseDoubleClick(MouseEvent e) {
    }

    /**
     * Mouse down.
     *
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDown(MouseEvent e) {
        if (selectedModeRecs == null) {
            return;
        }

        for (int i = 0; i < selectedModeRecs.size(); i++) {
            if (selectedModeRecs.get(i).contains(e.x, e.y)) {
                selectedCell = i + 1;
                categoryMap.get(getListIndexToKey()).setTextBox(selectedCell);
                categoryTable.getItem(categoryTable.getSelectionIndex())
                        .setText(1, getCellString(
                                categoryMap.get(getListIndexToKey())));
                removeSelectionBtn.setEnabled(true);
                canvas.redraw();
            }
        }

        configDialog.setNewConfig();
        needsSaveListener.saveNeeded(true);
    }

    /**
     * Mouse up. (Not used)
     */
    @Override
    public void mouseUp(MouseEvent e) {
    }

    private void setSortColumn(TableColumn sortColumn) {
        TableColumn currentColumn = categoryTable.getSortColumn();
        int dir = categoryTable.getSortDirection();
        if (sortColumn == currentColumn) {
            dir = (dir == SWT.UP ? SWT.DOWN : SWT.UP);
        } else {
            categoryTable.setSortColumn(sortColumn);
            dir = SWT.UP;
        }
        categoryTable.setSortDirection(dir);

        populateCategoryList();
    }

    /**
     * Populate the category list control.
     */
    private void populateCategoryList() {
        TableItem[] selectedItems = categoryTable.getSelection();
        String selectedCategory = StringUtils.EMPTY;
        if (!CollectionUtil.isNullOrEmpty(selectedItems)) {
            selectedCategory = selectedItems[0].getText(0);
        }

        if (categoryMap != null) {
            List<Entry<String, Category>> entries = new ArrayList<>(
                    categoryMap.entrySet());

            Comparator<Entry<String, Category>> comparator;
            if ("Category".equals(categoryTable.getSortColumn().getText())) {
                comparator = new Comparator<Map.Entry<String, Category>>() {
                    @Override
                    public int compare(Entry<String, Category> o1,
                            Entry<String, Category> o2) {
                        return o1.getKey().compareTo(o2.getKey());
                    }
                };
            } else {
                comparator = new Comparator<Map.Entry<String, Category>>() {
                    @Override
                    public int compare(Entry<String, Category> o1,
                            Entry<String, Category> o2) {
                        int rval = o1.getValue().getTextBox()
                                - o2.getValue().getTextBox();
                        if (rval == 0) {
                            rval = o1.getKey().compareTo(o2.getKey());
                        }
                        return rval;
                    }
                };
            }

            if (categoryTable.getSortDirection() == SWT.DOWN) {
                comparator = comparator.reversed();
            }
            Collections.sort(entries, comparator);

            categoryNames.clear();
            categoryTable.removeAll();

            for (Entry<String, Category> entry : entries) {
                categoryNames.add(entry.getKey());

                TableItem tableItem = new TableItem(categoryTable, SWT.NONE);
                tableItem.setText(0, entry.getKey());
                tableItem.setText(1, getCellString(entry.getValue()));
            }

            if (categoryTable.getItemCount() > 0) {
                int index = categoryNames.indexOf(selectedCategory);
                if (index < 0) {
                    index = 0;
                }
                categoryTable.select(index);

                handleSourceSelection();
            }

            for (TableColumn column : categoryTable.getColumns()) {
                column.pack();
            }
            getShell().pack();
        }
    }

    /**
     * Update the cell numbers.
     */
    private void updateCellNumbers() {
        TrayConfiguration.TrayMode layout = TrayConfiguration.TrayMode
                .valueOf(layoutCombo.getItem(layoutCombo.getSelectionIndex()));

        int numCells = layout.getNumberOfBoxes();
        Set<String> keys = categoryMap.keySet();

        for (String key : keys) {
            if (categoryMap.get(key).getTextBox() > numCells) {
                categoryMap.get(key).setTextBox(0);

                int listIndex = categoryNames.indexOf(key);
                categoryTable.getItem(listIndex).setText(1,
                        getCellString(categoryMap.get(key)));
            }
        }

        if (categoryMap.get(getListIndexToKey()).getTextBox() == 0) {
            removeSelectionBtn.setEnabled(false);
        }
    }

    /**
     * Create a new category.
     */
    private void createNewCategory() {
        if (newSrcCatDlg == null) {
            newSrcCatDlg = new NewSourceCategoryDlg(getShell(), true,
                    configData.getCategories().keySet());
            Boolean saveInfo = (Boolean) newSrcCatDlg.open();

            if (saveInfo != null && saveInfo) {
                String name = newSrcCatDlg.getTextKey();
                String desc = newSrcCatDlg.getDescription();
                Category cat = new Category(name, desc, 0);

                categoryMap.put(name, cat);

                // AlertViz Customization Update
                ConfigurationManager.getInstance().addToCustomization(cat);
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }

            newSrcCatDlg = null;
            populateCategoryList();
            drawSelectedTextBoxCell();
        }
    }

    /**
     * Delete the selected category.
     */
    private void deleteCategory() {
        if (categoryMap.get(getListIndexToKey()).isLocked()) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Delete");
            mb.setMessage(
                    "The selected Category is locked and cannot be deleted.");
            mb.open();
            return;
        }

        MessageBox mb = new MessageBox(getShell(),
                SWT.ICON_ERROR | SWT.YES | SWT.NO);
        mb.setText("Delete");
        mb.setMessage("Do you wish to delete the selected Category?");
        int val = mb.open();

        if (val == SWT.YES) {
            // AlertViz Customization Update
            Category removedCat = categoryMap.remove(getListIndexToKey());
            ConfigurationManager.getInstance()
                    .removeFromCustomization(removedCat);
            populateCategoryList();
            drawSelectedTextBoxCell();
            configDialog.setNewConfig();
            needsSaveListener.saveNeeded(true);
        }
    }

    /**
     * Reset all of the category text box values to "None" (0).
     */
    private void clearAllCategoryTextBoxes() {
        Set<String> keys = categoryMap.keySet();

        for (String key : keys) {
            categoryMap.get(key).setTextBox(0);
            int listIndex = categoryNames.indexOf(key);

            categoryTable.getItem(listIndex).setText(1,
                    getCellString(categoryMap.get(key)));
        }
        configDialog.setNewConfig();
        needsSaveListener.saveNeeded(true);
    }

    /**
     * Get the cell number as a formatted string.
     *
     * @param data
     *            Category data.
     * @return Formatted cell string.
     */
    private String getCellString(Category data) {
        String cellNum;
        if (data.getTextBox() == 0) {
            cellNum = "None";
        } else {
            cellNum = String.valueOf(data.getTextBox());
        }
        return cellNum;
    }

    /**
     * Determine which cell is selected and redraw the canvas.
     */
    private void drawSelectedTextBoxCell() {
        String key = getListIndexToKey();
        if (key != null && !key.isEmpty()) {
            selectedCell = categoryMap.get(key).getTextBox();
        } else {
            selectedCell = 0;
        }

        if (removeSelectionBtn == null || canvas == null) {
            return;
        }

        if (selectedCell == 0) {
            removeSelectionBtn.setEnabled(false);
        } else {
            removeSelectionBtn.setEnabled(true);
        }

        canvas.redraw();
    }

    /**
     * Get the category map key associated with the category list.
     *
     * @return Category map key.
     */
    private String getListIndexToKey() {
        String key = "";
        if (categoryTable != null) {
            int catListIndex = categoryTable.getSelectionIndex();
            if (catListIndex != -1 && categoryNames.size() > catListIndex) {
                key = categoryNames.get(catListIndex);
            }
        }

        return key;
    }

    /**
     * Respond to change in the source list's selection by updating the text box
     * selection and enabling or disabling the Delete button.
     */
    private void handleSourceSelection() {
        drawSelectedTextBoxCell();
        if (deleteBtn != null) {
            boolean enable = false;
            Category c = categoryMap.get(getListIndexToKey());
            if (c != null) {
                enable = !c.isLocked();
            }
            deleteBtn.setEnabled(enable);
        }
    }

    /**
     * Get the current Layout Mode.
     *
     * @return the TrayMode.
     */
    public TrayConfiguration.TrayMode getSelectedLayoutTrayMode() {
        return selectedMode;
    }
}
