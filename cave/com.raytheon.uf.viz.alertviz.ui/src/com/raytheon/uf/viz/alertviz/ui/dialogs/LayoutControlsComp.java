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
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;

import com.raytheon.uf.viz.alertviz.ConfigurationManager;
import com.raytheon.uf.viz.alertviz.INeedsSaveListener;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

/**
 * This class displays category controls and gives the user the ability to
 * determine which category message appear in which text controls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 23 Nov 2010  2235       cjeanbap    Clear All Layouts needs to refresh the list. 
 * 23 Nov 2010  6343       cjeanbap    Add if-statement in initControls() to correct
 *                                     layout functionality.
 * 28 Jan 2011  4617       cjeanbap    Added Monitor Only functionality.
 * 03 Feb 2011  4617       cjeanbap    Add getSelectedLayoutTrayMode();
 * 24 Mar 2011	5853	   cjeanbap    Add createLayoutControls() to reloadConfig().
 * 02 May 2011  9067       cjeanbap    Remove createLayoutControls() from reloadConfig().
 * 07 Feb 2013	15490	   Xiaochuan   Add configDialog to handle the updated setting 
 * 									   on Category layers. 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class LayoutControlsComp extends Composite implements MouseListener {

    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Drawing canvas.
     */
    private Canvas canvas;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 180;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 180;

    /**
     * Layout row 1 Y coordinate.
     */
    private int row1YCoord = 0;

    /**
     * Layout row 2 Y coordinate.
     */
    private int row2YCoord = 0;

    /**
     * Layout row 3 Y coordinate.
     */
    private int row3YCoord = 0;

    /**
     * Layout row 4 Y coordinate.
     */
    private int row4YCoord = 0;

    /**
     * Average font width.
     */
    private int aveFontWidth = 0;

    /**
     * Combo box containing the different text control layouts.
     */
    private Combo layoutCombo;

    /**
     * Category list control.
     */
    private List categoryList;

    /**
     * Layout rectangles.
     */
    private LayoutRectangles layoutRecs;

    /**
     * Selected mode rectangles.
     */
    private ArrayList<Rectangle> selectedModeRecs;

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
    private final String layoutText = "Message Text Layout:";

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
     */
    public LayoutControlsComp(Composite parentComp, Configuration configData,
            INeedsSaveListener needsSaveListener, AlertVisConfigDlg configDialog) {
        super(parentComp, SWT.NONE);

        this.parentComp = parentComp;
        this.configData = configData;
        this.needsSaveListener = needsSaveListener;
        this.configDialog = configDialog;

        init(configData);
    }

    /**
     * Initialize method.
     */
    public void init(Configuration configData) {
        this.configData = configData;
        GridLayout gl = new GridLayout(2, true);
        gl.horizontalSpacing = 45;
        this.setLayout(gl);

        initValues();
        initControls();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent arg0) {
                controlFont.dispose();
            }
        });
    }

    /**
     * Initialize values.
     */
    private void initValues() {
        layoutRecs = new LayoutRectangles();
        controlFont = new Font(parentComp.getDisplay(), "Monospace", 10,
                SWT.NORMAL);

        row1YCoord = layoutRecs.getRow1YCoord();
        row2YCoord = layoutRecs.getRow2YCoord();
        row3YCoord = layoutRecs.getRow3YCoord();
        row4YCoord = layoutRecs.getRow4YCoord();

        categoryMap = configData.getCategories();
        categoryNames = new ArrayList<String>();
        // populateCategories();
    }

    /**
     * Initialize the controls.
     */
    private void initControls() {
        createCategoryListControls();
        createLayoutControls();
        handleSourceSelection();
    }

    public void reloadConfig(Configuration configData) {
        this.configData = configData;
        GridLayout gl = new GridLayout(2, true);
        gl.horizontalSpacing = 45;
        this.setLayout(gl);

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
        listComp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalIndent = 4;
        gd.horizontalSpan = 2;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 200;
        gd.heightHint = 205;
        gd.horizontalSpan = 2;
        categoryList = new List(listComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        categoryList.setLayoutData(gd);
        categoryList.setFont(controlFont);
        categoryList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                handleSourceSelection();
            }
        });

        populateCategoryList();

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button newBtn = new Button(listComp, SWT.PUSH);
        newBtn.setText("New...");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                createNewCategory();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        deleteBtn = new Button(listComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteCategory();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Button clearAllBtn = new Button(listComp, SWT.PUSH);
        clearAllBtn.setText("Clear All Layouts");
        clearAllBtn.setLayoutData(gd);
        clearAllBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                clearAllCategoryTextBoxes();
            }
        });

        // Right-click pop-up menu
        popupMenuCList = new Menu(categoryList);
        categoryList.setMenu(popupMenuCList);
        popupMenuCList.addListener(SWT.Show, new Listener() {
            public void handleEvent(Event event) {
                MenuItem[] menuItems = popupMenuCList.getItems();

                for (int i = 0; i < menuItems.length; i++) {
                    menuItems[i].dispose();
                }

                if (categoryMap.get(getListIndexToKey()).isLocked() != true) {
                    menuItem = new MenuItem(popupMenuCList, SWT.PUSH);
                    menuItem.addSelectionListener(new SelectionAdapter() {
                        public void widgetSelected(SelectionEvent e) {
                            deleteCategory();
                        }
                    });
                    menuItem.setText("Delete");
                }
            }// handleEvent()
        });
    }

    /**
     * Create layout controls.
     */
    private void createLayoutControls() {
        Composite controlComp = new Composite(this, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));

        Label layoutComboLbl = new Label(controlComp, SWT.NONE);
        layoutComboLbl.setText("Layout:");

        layoutCombo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateLayoutCombo();
        layoutCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                int index = layoutCombo.getSelectionIndex();
                selectedMode = TrayConfiguration.TrayMode.valueOf(layoutCombo
                        .getItem(index));
                selectedModeRecs = layoutRecs.getRectangles(selectedMode);
                canvas.redraw();
                updateCellNumbers();
				configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        GridData gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 15;
        Label lbl = new Label(controlComp, SWT.NONE);
        lbl.setText("Click cell to add to category:");
        lbl.setLayoutData(gd);

        addCanvas(controlComp);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 2;
        removeSelectionBtn = new Button(controlComp, SWT.PUSH);
        removeSelectionBtn.setText("Remove Selection");
        removeSelectionBtn.setEnabled(false);
        removeSelectionBtn.setLayoutData(gd);
        removeSelectionBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                int index = categoryList.getSelectionIndex();

                categoryMap.get(getListIndexToKey()).setTextBox(0);

                categoryList.setItem(index,
                        getFormattedCategoryData(categoryMap
                                .get(getIndexToKey(index))));
                selectedCell = 0;
                removeSelectionBtn.setEnabled(false);
                canvas.redraw();
                needsSaveListener.saveNeeded(true);
            }
        });

        if (categoryList.getSelectionIndex() != -1) {
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
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 2;
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        canvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);

        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
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
        gc.setFont(controlFont);
        aveFontWidth = (int) gc.getFontMetrics().getAverageCharWidth();

        gc.setLineWidth(3);

        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));

        gc.drawString(layoutText, (CANVAS_WIDTH / 2)
                - (layoutText.length() * aveFontWidth / 2), 2, true);

        TrayConfiguration.TrayMode layout = TrayConfiguration.TrayMode
                .valueOf(layoutCombo.getItem(layoutCombo.getSelectionIndex()));

        switch (layout) {
        case Q4:
            drawQ4(gc);
            break;
        case H1:
            drawH1(gc);
            break;
        case H2:
            drawH2(gc);
            break;
        case V2:
            drawV2(gc);
            break;
        case V3:
            drawV3(gc);
            break;
        case V4:
            drawV4(gc);
            break;
        case MO:
            break;
        }
    }

    /**
     * Draw the Quad 4 layout.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawQ4(GC gc) {
        drawRectangles(gc);

        gc.drawString("1", 50 - aveFontWidth, row1YCoord + 2, true);
        gc.drawString("2", 130 - aveFontWidth, row1YCoord + 2, true);
        gc.drawString("3", 50 - aveFontWidth, row2YCoord + 2, true);
        gc.drawString("4", 130 - aveFontWidth, row2YCoord + 2, true);

    }

    /**
     * Draw the horizontal 1 text control layout.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawH1(GC gc) {
        drawRectangles(gc);

        gc.drawString("1", 90 - aveFontWidth, row1YCoord + 2, true);
    }

    /**
     * Draw the horizontal 2 text controls layout.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawH2(GC gc) {
        drawRectangles(gc);

        gc.drawString("1", 50 - aveFontWidth, row1YCoord + 2, true);
        gc.drawString("2", 130 - aveFontWidth, row1YCoord + 2, true);
    }

    /**
     * Draw the vertical 2 text controls layout.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawV2(GC gc) {
        drawRectangles(gc);

        gc.drawString("1", 90 - aveFontWidth, row1YCoord + 2, true);
        gc.drawString("2", 90 - aveFontWidth, row2YCoord + 2, true);
    }

    /**
     * Draw the vertical 3 text controls layout.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawV3(GC gc) {
        drawRectangles(gc);

        gc.drawString("1", 90 - aveFontWidth, row1YCoord + 2, true);
        gc.drawString("2", 90 - aveFontWidth, row2YCoord + 2, true);
        gc.drawString("3", 90 - aveFontWidth, row3YCoord + 2, true);
    }

    /**
     * Draw the vertical 4 text controls layout.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawV4(GC gc) {
        drawRectangles(gc);

        gc.drawString("1", 90 - aveFontWidth, row1YCoord + 2, true);
        gc.drawString("2", 90 - aveFontWidth, row2YCoord + 2, true);
        gc.drawString("3", 90 - aveFontWidth, row3YCoord + 2, true);
        gc.drawString("4", 90 - aveFontWidth, row4YCoord + 2, true);
    }

    /**
     * Draw the rectangles.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawRectangles(GC gc) {
        for (int i = 0; i < selectedModeRecs.size(); i++) {
            if ((selectedCell - 1) == i) {
                gc.setBackground(parentComp.getDisplay().getSystemColor(
                        SWT.COLOR_YELLOW));
            } else {
                gc.setBackground(parentComp.getDisplay().getSystemColor(
                        SWT.COLOR_WIDGET_BACKGROUND));
            }

            gc.fillRectangle(selectedModeRecs.get(i));
            gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_BLACK));
            gc.drawRectangle(selectedModeRecs.get(i));
        }

        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
    }

    /**
     * Populate the layout combo box.
     */
    private void populateLayoutCombo() {
        for (TrayConfiguration.TrayMode m : TrayConfiguration.TrayMode.values()) {
            String str = m.name();
            layoutCombo.add(str);
        }
        updateLayoutCombo();
    }

    private void updateLayoutCombo() {

        selectedMode = configData.getGlobalConfiguration().getMode();

        layoutCombo.select(selectedMode.ordinal());
        selectedModeRecs = layoutRecs.getRectangles(selectedMode);
        String key = getListIndexToKey();
        if (key != null && !("".equals(key))) {
            selectedCell = categoryMap.get(key).getTextBox();
        } else {
            selectedCell = 0;
        }
    }

    /**
     * Get the text for the list label.
     * 
     * @return Label text.
     */
    private String getListLabelText() {
        String format = "%S          %S";

        String labelStr = String.format(format, "Category", "Cell #");

        return labelStr;
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
                categoryMap.get(getListIndexToKey()).setTextBox(i + 1);
                categoryList.setItem(categoryList.getSelectionIndex(),
                        getFormattedCategoryData(categoryMap
                                .get(getListIndexToKey())));
                removeSelectionBtn.setEnabled(true);
                canvas.redraw();
            }
        }
        needsSaveListener.saveNeeded(true);
    }

    /**
     * Mouse up. (Not used)
     */
    @Override
    public void mouseUp(MouseEvent e) {
    }

    /**
     * Populate the category list control.
     */
    private void populateCategoryList() {
        String str;

        java.util.List<String> keys = new ArrayList<String>(
                categoryMap.keySet());
        Collections.sort(keys);
        categoryNames.clear();
        categoryList.removeAll();

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            String key = iterator.next();
            str = getFormattedCategoryData(categoryMap.get(key));
            categoryNames.add(key);

            categoryList.add(str);
        }

        if (categoryList.getItemCount() == 0) {
            return;
        }

        categoryList.select(0);

        handleSourceSelection();
    }

    /**
     * Update the cell numbers.
     */
    private void updateCellNumbers() {
        TrayConfiguration.TrayMode layout = TrayConfiguration.TrayMode
                .valueOf(layoutCombo.getItem(layoutCombo.getSelectionIndex()));

        int numCells = 0;

        switch (layout) {
        case H1:
            numCells = 1;
            break;
        case H2:
        case V2:
            numCells = 2;
            break;
        case V3:
            numCells = 3;
            break;
        case Q4:
        case V4:
            numCells = 4;
            break;
        case MO:
            return;
        }

        if (numCells == 4) {
            return;
        }

        Set<String> keys = categoryMap.keySet();

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            String key = iterator.next();

            if (categoryMap.get(key).getTextBox() > numCells) {
                categoryMap.get(key).setTextBox(0);

                int listIndex = categoryNames.indexOf(key);

                categoryList.setItem(listIndex,
                        getFormattedCategoryData(categoryMap.get(key)));
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
            newSrcCatDlg = new NewSourceCategoryDlg(parentComp.getShell(),
                    true, configData.getCategories().keySet());
            Boolean saveInfo = (Boolean) newSrcCatDlg.open();

            if (saveInfo != null && saveInfo == true) {
                String name = newSrcCatDlg.getTextKey();
                String desc = newSrcCatDlg.getDescription();
                Category cat = new Category(name, desc, 0);

                categoryMap.put(name, cat);

                // AlertViz Customization Update
                ConfigurationManager.getInstance().addToCustomization(cat);
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
        if (categoryMap.get(getListIndexToKey()).isLocked() == true) {
            MessageBox mb = new MessageBox(parentComp.getShell(),
                    SWT.ICON_ERROR | SWT.OK);
            mb.setText("Delete");
            mb.setMessage("The selected Category is locked and cannot be deleted.");
            mb.open();
            return;
        }

        MessageBox mb = new MessageBox(parentComp.getShell(), SWT.ICON_ERROR
                | SWT.YES | SWT.NO);
        mb.setText("Delete");
        mb.setMessage("Do you wish to delete the selected Category?");
        int val = mb.open();

        if (val == SWT.YES) {
            // AlertViz Customization Update
            Category removedCat = categoryMap.remove(getListIndexToKey());
            ConfigurationManager.getInstance().removeFromCustomization(
                    removedCat);
            populateCategoryList();
            drawSelectedTextBoxCell();
            needsSaveListener.saveNeeded(true);
        }
    }

    /**
     * Reset all of the category text box values to "None" (0).
     */
    private void clearAllCategoryTextBoxes() {
        Set<String> keys = categoryMap.keySet();

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            String key = iterator.next();
            categoryMap.get(key).setTextBox(0);
            int listIndex = categoryNames.indexOf(key);

            categoryList.setItem(listIndex,
                    getFormattedCategoryData(categoryMap.get(key)));
        }
        needsSaveListener.saveNeeded(true);
    }

    /**
     * Get the category data as a formatted string.
     * 
     * @param data
     *            Category data.
     * @return Formatted string.
     */
    private String getFormattedCategoryData(Category data) {
        String format = "%-18S %4s";
        String cellNum;

        if (data.getTextBox() == 0) {
            cellNum = "None";
        } else {
            cellNum = String.valueOf(data.getTextBox());
        }

        String labelStr = String
                .format(format, data.getCategoryName(), cellNum);

        return labelStr;
    }

    /**
     * Determine which cell is selected and redraw the canvas.
     */
    private void drawSelectedTextBoxCell() {
        String key = getListIndexToKey();
        if (key != null && !key.equals("")) {
            selectedCell = categoryMap.get(key).getTextBox();
        } else {
            selectedCell = 0;
        }

        if (removeSelectionBtn == null || canvas == null)
            return;

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
        String key = new String();
        if (categoryList != null && categoryList.getItemCount() > 0) {
            key = categoryNames.get(categoryList.getSelectionIndex());
        }

        return key;
    }

    /**
     * Get the category map key associated with the list index.
     * 
     * @param index
     *            List index.
     * @return Category map key.
     */
    private String getIndexToKey(int index) {
        // String key = categoryList.getItem(index);
        String key = categoryNames.get(index);

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
            try {
                enable = !categoryMap.get(getListIndexToKey()).isLocked();
            } catch (RuntimeException e) {
                // ignore
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
