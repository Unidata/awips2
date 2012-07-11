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
package com.raytheon.viz.ui.widgets;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

/**
 * This creates a composite that displays a button which is associated with a
 * menu. This class will take responsibility for disposing any menus assigned to
 * it via the setMenu method. When a non-cascading menu item is selected the
 * button's text is updated to reflect the selection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Jul 6, 2012  #875       rferrel     Move to common package and made more robust.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class MenuButton extends MinimumSizeComposite implements
        SelectionListener, DisposeListener {
    private Button button;

    private int defaultMinWidth = SWT.DEFAULT;

    private int defaultMinHeight = SWT.DEFAULT;

    private final ArrayList<SelectionListener> listeners = new ArrayList<SelectionListener>();

    private MenuItem selectedItem;

    public MenuButton(Composite parent) {
        super(parent, SWT.NONE);
        setLayout(new FillLayout(SWT.HORIZONTAL));
        button = new Button(this, SWT.PUSH);
        /*
         * // This produced annoying behavoir... button.addMouseListener(new
         * MouseAdapter() {
         * 
         * @Override public void mouseDown(MouseEvent e) { if (e.button == 1) {
         * doPopup(); } }
         * 
         * });
         */
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                doPopup();
            }
        });
        addDisposeListener(this);
    }

    public void setEnabled(boolean enabled) {
        button.setEnabled(enabled);
    }

    private void doPopup() {
        boolean isEnabled = button.isEnabled();
        if (!isEnabled)
            return;
        /*
         * button.setEnabled(false); button.setEnabled(true);
         */
        Menu menu = getMenu();
        if (menu == null)
            return;
        Point sz = getSize();
        Point p = toDisplay(0, sz.y);
        menu.setLocation(p);
        menu.setVisible(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.widgets.Control#setMenu(org.eclipse.swt.widgets.Menu)
     */
    @Override
    public void setMenu(Menu menu) {
        // Dispose of previous menu
        unrealizeMenu();
        super.setMenu(menu);
        realizeMenu();
    }

    public void updateMenu() {
        realizeMenu();
    }

    private void realizeMenu() {
        String text = button.getText();
        realizeMenu1(getMenu());
        // Minimize button's default size in so it will not interfere if
        // setSize is invoked.
        button.setText("");
        button.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        button.setText(text);
    }

    /**
     * Recursive call to set listeners on non-CASCADE menu items.
     * 
     * @param menu
     */
    private void realizeMenu1(Menu menu) {
        if (menu != null) {
            for (MenuItem mi : menu.getItems()) {
                if ((mi.getStyle() & SWT.CASCADE) == 0) {
                    mi.addSelectionListener(this);
                    // Adjust size so largest item's text will fit in the button
                    button.setText(mi.getText());
                    Point pt = button.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                    if (minWidth < pt.x) {
                        minWidth = pt.x;
                    }
                    if (minHeight < pt.y) {
                        minHeight = pt.y;
                    }
                } else {
                    realizeMenu1(mi.getMenu());
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse
     * .swt.events.SelectionEvent)
     */
    @Override
    public void widgetDefaultSelected(SelectionEvent e) {
        widgetSelected(e);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt
     * .events.SelectionEvent)
     */
    @Override
    public void widgetSelected(SelectionEvent e) {
        MenuItem mi;
        try {
            mi = (MenuItem) e.widget;
        } catch (ClassCastException exc) {
            return;
        }
        setSelectedItem(mi);

        Event ev = new Event();
        ev.widget = this;
        ev.data = mi;

        SelectionEvent selEvent = new SelectionEvent(ev);

        SelectionListener[] ls = listeners
                .toArray(new SelectionListener[listeners.size()]);
        for (SelectionListener l : ls) {
            l.widgetSelected(selEvent);
        }
    }

    @Override
    public void widgetDisposed(DisposeEvent e) {
        unrealizeMenu();
    }

    private void unrealizeMenu() {
        unrealizeMenu1(getMenu());
        super.setMinimumSize(defaultMinWidth, defaultMinHeight);
    }

    /**
     * Recursive call to remove listeners from menu items and dispose of menus.
     * 
     * @param menu
     */
    private void unrealizeMenu1(Menu menu) {
        if (menu != null) {
            for (MenuItem mi : menu.getItems()) {
                if (mi != null) {
                    if ((mi.getStyle() & SWT.CASCADE) == 0) {
                        mi.removeSelectionListener(this);
                    } else {
                        unrealizeMenu1(mi.getMenu());
                    }
                }
            }
            menu.dispose();
        }
    }

    /**
     * The selection event sent to the listener will have its data object set to
     * the menu item that was selected.
     * 
     * @param listener
     */
    public void addSelectionListener(SelectionListener listener) {
        if (listener == null)
            throw new NullPointerException();
        listeners.add(listener);
    }

    public MenuItem getSelectedItem() {
        return selectedItem;
    }

    /**
     * This becomes the selected item and changes the button's text to the
     * item's text.
     * 
     * @param selectedItem
     */
    public void setSelectedItem(MenuItem selectedItem) {
        this.selectedItem = selectedItem;
        if (selectedItem != null)
            button.setText(selectedItem.getText());
        else
            button.setText("");
    }

    /**
     * This changes the button's text to the selected text and the non-cascade
     * item whose text matches the selected text becomes the selected item. The
     * selected item is set to null when a match is not found.
     * 
     * @param selectedText
     */
    public void setSelectedItem(String selectedText) {
        if (selectedText == null) {
            button.setText("");
            this.selectedItem = null;
            button.setText(selectedText);
        } else {
            button.setText(selectedText);
            this.selectedItem = findItem(getMenu(), selectedText);
        }
    }

    /**
     * Search menu and its sub-menus for a non-cascade menu item with the
     * desired text value.
     * 
     * @param menu
     * @param text
     * @return item - null if no match is found
     */
    private MenuItem findItem(Menu menu, String text) {
        if (menu != null) {
            for (MenuItem item : menu.getItems()) {
                if (item != null) {
                    if ((item.getStyle() & SWT.CASCADE) != 0) {
                        MenuItem foundItem = findItem(item.getMenu(), text);
                        if (foundItem != null) {
                            return foundItem;
                        }
                    } else if (text.equals(item.getText())) {
                        return item;
                    }
                }
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.widgets.MinimumSizeComposite#setMinimumSize(org.eclipse
     * .swt.graphics.Point)
     */
    @Override
    public void setMinimumSize(Point sz) {
        defaultMinWidth = sz.x;
        defaultMinHeight = sz.y;
        super.setMinimumSize(sz);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.widgets.MinimumSizeComposite#setMinimumSize(int,
     * int)
     */
    @Override
    public void setMinimumSize(int width, int height) {
        defaultMinWidth = width;
        defaultMinHeight = height;
        super.setMinimumSize(width, height);
    }
}
