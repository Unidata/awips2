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
package com.raytheon.uf.viz.ui.menus.widgets.tearoff;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Holds the information for all the menu items in the dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2011            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class MenuItemComposite extends Composite {

    private Control firstItem;

    private Control secondItem;

    // backing data for executing listeners
    private MenuItem item;

    private Image arrow = null;

    private Image highlightedArrow = null;

    private Listener updateListener = null;

    private Listener showListener = null;

    private Menu menu = null;

    private Composite parent = null;

    private Menu topLevelMenu = null;

    private UIJob job = null;

    /**
     * @param parent
     * @param style
     */
    public MenuItemComposite(Composite parent, int style) {
        super(parent, style);
        this.parent = parent;
    }

    // creates both labels and ties them together
    public void addLabels(MenuItem it, int labelStyle) {
        if (it.isDisposed()) {
            return;
        }

        // going to hold the menu around so that if the cave menu gets opened
        // again (when the items get disposed), we will be able to go back in
        // and get the items from the menu and rebuild in the background
        menu = it.getParent();

        topLevelMenu = menu;
        while (topLevelMenu.getParentMenu() != null
                && topLevelMenu.getParentMenu().getParentMenu() != null) {
            topLevelMenu = topLevelMenu.getParentMenu();
        }

        item = it;
        String[] labels = item.getText().split("\t");
        // handle for a separator menu item
        if (item.getStyle() == SWT.SEPARATOR) {
            firstItem = new Label(this, SWT.SEPARATOR | SWT.HORIZONTAL);
            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            gd.horizontalSpan = 2;
            firstItem.setLayoutData(gd);
        } else {
            // radio items
            if (item.getStyle() == SWT.RADIO) {
                firstItem = new Button(this, SWT.RADIO);
                ((Button) firstItem).setSelection(item.getSelection());
                GridData gd = new GridData(18, 18);
                firstItem.setLayoutData(gd);

                secondItem = new Label(this, labelStyle);
                ((Label) secondItem).setText(labels[0]);
                gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
                secondItem.setLayoutData(gd);
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        if (e.widget instanceof MenuItem) {
                            // check that the radio groups match
                            for (Control comp : firstItem.getParent()
                                    .getParent().getChildren()) {
                                MenuItemComposite composite = (MenuItemComposite) comp;
                                if (composite.item.getText().equals(
                                        ((MenuItem) e.widget).getText())) {
                                    if (composite.firstItem instanceof Button) {
                                        ((Button) composite.firstItem)
                                                .setSelection(composite.item
                                                        .getSelection());
                                    }
                                } else {
                                    if (composite.firstItem instanceof Button) {
                                        ((Button) composite.firstItem)
                                                .setSelection(false);
                                    }
                                }
                            }
                        }
                    }
                });
            }
            // check boxes
            // else if (item.getStyle() == SWT.CHECK) {
            // // if (item.getStyle() == SWT.CHECK) {
            // firstItem = new Button(this, SWT.CHECK);
            // ((Button) firstItem).setSelection(item.getSelection());
            // GridData gd = new GridData(18, 18);
            // firstItem.setLayoutData(gd);
            //
            // secondItem = new Label(this, labelStyle);
            // ((Label) secondItem).setText(labels[0]);
            // gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
            // secondItem.setLayoutData(gd);
            // }
            // submenus (with arrows)
            else if (item.getStyle() == SWT.CASCADE) {
                firstItem = new Label(this, SWT.PUSH);
                firstItem.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                        true, false));
                ((Label) firstItem).setText(labels[0]);
                secondItem = new Label(this, labelStyle);
                createArrow();
                ((Label) secondItem).setImage(arrow);
            }
            // regular selectable menu items
            else {
                firstItem = new Label(this, labelStyle);
                firstItem.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                        true, false));
                ((Label) firstItem).setText(labels[0]);

                secondItem = new Label(this, labelStyle);
                if (labels.length > 1) {
                    ((Label) secondItem).setText(labels[1]);
                }

                createUpdateListener(this);
                item.addListener(SWT.Modify, updateListener);
            }
            showListener = new Listener() {
                @Override
                public void handleEvent(final Event event) {
                    job = new UIJob(Display.getCurrent(),
                            "Regenerate Tear Off Menus") {

                        @Override
                        public IStatus runInUIThread(IProgressMonitor monitor) {
                            if (parent == null || parent.isDisposed()
                                    || parent.getShell() == null
                                    || parent.getShell().isDisposed()) {
                                return Status.CANCEL_STATUS;
                            }
                            if (menu == null || menu.isDisposed()) {
                                for (MenuItem item : topLevelMenu.getItems()) {
                                    if (item.getMenu() != null) {
                                        ;
                                        for (Listener list : item.getMenu()
                                                .getListeners(SWT.Show)) {
                                            Event event = new Event();
                                            event.widget = item;
                                            event.type = SWT.Show;
                                            list.handleEvent(event);
                                        }
                                        if (getShell().getText().equals(
                                                item.getText())) {
                                            menu = item.getMenu();
                                            break;
                                        }
                                    }
                                }
                            }

                            int start = 0;
                            if (menu.getItemCount() != parent.getChildren().length) {
                                start = (menu.getItemCount() - parent
                                        .getChildren().length);
                            }
                            if (parent.getChildren().length > 0) {
                                for (int i = start; i < menu.getItemCount(); i++) {
                                    final MenuItemComposite mic = (MenuItemComposite) parent
                                            .getChildren()[i - start];
                                    if (mic.item.isDisposed()) {
                                        mic.item = menu.getItem(i);
                                        createUpdateListener(mic);
                                        mic.item.addListener(SWT.Modify,
                                                updateListener);

                                        for (Listener list : mic.item
                                                .getListeners(SWT.Modify)) {
                                            Event e = new Event();
                                            e.type = SWT.Modify;
                                            e.data = mic.item;
                                            list.handleEvent(e);
                                        }
                                    }
                                }
                            }

                            return Status.OK_STATUS;
                        }
                    };
                    job.schedule();
                }
            };
            item.getParent().addListener(SWT.Show, showListener);
            topLevelMenu.addListener(SWT.Show, showListener);

            if (item.isEnabled()) {
                // add the listeners to both the first and the second
                // control, so the same thing happens if you scroll over either,
                // or the MenuItemComposite
                MouseTrackAdapter mouseTrackAdapter = getMouseTrackAdapter();
                firstItem.addMouseTrackListener(mouseTrackAdapter);
                secondItem.addMouseTrackListener(mouseTrackAdapter);
                this.addMouseTrackListener(mouseTrackAdapter);

                MouseAdapter mouseAdapter = getMouseAdapter();
                firstItem.addMouseListener(mouseAdapter);
                secondItem.addMouseListener(mouseAdapter);
                this.addMouseListener(mouseAdapter);
            } else {
                setForeground(Display.getCurrent().getSystemColor(
                        SWT.COLOR_DARK_GRAY));
            }
        }
    }

    protected void createUpdateListener(final MenuItemComposite mic) {
        updateListener = new Listener() {
            @Override
            public void handleEvent(Event event) {
                if (mic.secondItem != null && !mic.secondItem.isDisposed()) {
                    if (mic.item == event.data) {
                        if (((MenuItem) event.data).getText().split("\t").length > 1) {
                            ((Label) mic.secondItem)
                                    .setText(((MenuItem) event.data).getText()
                                            .split("\t")[1]);
                            // don't want to make the times go off the
                            // screen
                            mic.layout();
                        }
                    }
                }
            }
        };
    }

    /**
     * Sets the background on all the visible items
     */
    @Override
    public void setBackground(Color color) {
        firstItem.setBackground(color);
        secondItem.setBackground(color);
        super.setBackground(color);
    }

    /**
     * Sets the foreground on all the visible items to the necessary color
     */
    @Override
    public void setForeground(Color color) {
        firstItem.setForeground(color);
        secondItem.setForeground(color);
        super.setForeground(color);
    }

    /**
     * Creates the arrows for submenus
     */
    private void createArrow() {
        int imgWidth = 11;
        int imgHeight = 11;

        arrow = new Image(Display.getCurrent(), imgWidth, imgHeight);
        highlightedArrow = new Image(Display.getCurrent(), imgWidth, imgHeight);

        // the normal arrow
        GC gc = new GC(arrow);
        drawArrowImage(gc, imgWidth, imgHeight, SWT.COLOR_WIDGET_BACKGROUND,
                SWT.COLOR_BLACK);

        // the highlighted arrow
        gc = new GC(highlightedArrow);
        drawArrowImage(gc, imgWidth, imgHeight, SWT.COLOR_LIST_SELECTION,
                SWT.COLOR_WHITE);

        gc.dispose();
    }

    /**
     * Create the arrow image.
     * 
     * @param gc
     *            Graphic context.
     * @param imgWidth
     *            Image width.
     * @param imgHeight
     *            Image height.
     */
    private void drawArrowImage(GC gc, int imgWidth, int imgHeight,
            int highlightColor, int arrowColor) {
        gc.setAntialias(SWT.ON);

        // "Erase" the canvas by filling it in with a white rectangle.
        gc.setBackground(Display.getCurrent().getSystemColor(highlightColor));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        gc.setBackground(Display.getCurrent().getSystemColor(arrowColor));

        int[] polyArray = new int[] { 2, 0, 8, 4, 2, 8 };

        gc.fillPolygon(polyArray);
    }

    private void addMenu(MenuItem item, int y) {
        PopupMenu men = new PopupMenu();
        men.addSubmenus(item, this.getShell(), y);
    }

    /**
     * Highlight the areas of the composite so that we get the "look" of the
     * whole thing being highlighted
     * 
     * @return
     */
    private MouseTrackAdapter getMouseTrackAdapter() {
        MouseTrackAdapter trackAdapter = new MouseTrackAdapter() {
            @Override
            public void mouseEnter(MouseEvent e) {
                // we want all the colors to be the same for background
                // and foreground, so we set that here, this is to tell
                // the whole thing to be highlighted
                setBackground(Display.getCurrent().getSystemColor(
                        SWT.COLOR_LIST_SELECTION));
                setForeground(Display.getCurrent().getSystemColor(
                        SWT.COLOR_LIST_SELECTION_TEXT));
                // changes the arrow image to the highlighted version
                if (secondItem instanceof Label) {
                    if (((Label) secondItem).getImage() != null) {
                        ((Label) secondItem).setImage(highlightedArrow);
                    }
                }
            }

            @Override
            public void mouseExit(MouseEvent e) {
                // we want all the colors to be the same for background
                // and foreground, so we set that here, this is to
                // unhighlight the whole thing
                setBackground(Display.getCurrent().getSystemColor(
                        SWT.COLOR_WIDGET_BACKGROUND));
                setForeground(Display.getCurrent().getSystemColor(
                        SWT.COLOR_WIDGET_FOREGROUND));
                // changes the arrow image to the unhighlighted version
                if (secondItem instanceof Label) {
                    if (((Label) secondItem).getImage() != null) {
                        ((Label) secondItem).setImage(arrow);
                    }
                }
            }
        };
        return trackAdapter;
    }

    /**
     * Select on either item being selected, so that we get the same action for
     * both being selected
     * 
     * @return
     */
    private MouseAdapter getMouseAdapter() {
        MouseAdapter mouseAdapter = new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (menu == null || menu.isDisposed()) {
                    for (MenuItem item : topLevelMenu.getItems()) {
                        if (item.getMenu() != null) {
                            for (Listener list : item.getMenu().getListeners(
                                    SWT.Show)) {
                                Event event = new Event();
                                event.widget = item;
                                event.type = SWT.Show;
                                list.handleEvent(event);
                            }
                            if (getShell().getText().equals(item.getText())) {
                                menu = item.getMenu();
                                break;
                            }
                        }
                    }
                }
                // if the menu has been opened, then the items need to
                // be regenerated if there is a submenu, then add the
                // ability to show it
                // TODO, actually need to keep them in sync
                if (item == null || item.isDisposed()) {
                    int start = 0;
                    if (menu.getItemCount() != parent.getChildren().length) {
                        start = 1;
                    }
                    for (int i = start; i < menu.getItemCount(); i++) {
                        MenuItemComposite mic = (MenuItemComposite) parent
                                .getChildren()[i - start];
                        if (mic.item.isDisposed()) {
                            mic.item = menu.getItem(i);
                            createUpdateListener(mic);
                            mic.item.addListener(SWT.Modify, updateListener);

                            for (Listener list : mic.item
                                    .getListeners(SWT.Modify)) {
                                Event ev = new Event();
                                ev.type = SWT.Modify;
                                ev.data = mic.item;
                                list.handleEvent(ev);
                            }
                        }
                    }
                }

                if (item.getMenu() != null) {
                    // get the y offset based on the location of the
                    // click
                    int y = 0;
                    if (e.widget instanceof MenuItemComposite) {
                        y = ((Control) e.widget).getLocation().y;
                    } else {
                        y = ((Control) e.widget).getParent().getLocation().y;
                    }
                    addMenu(item, y);
                    return;
                }

                // handle the selection event, so if it is able to load
                // something, do it (by looping over ALL the selection
                // listeners assigned to the item)
                for (Listener list : item.getListeners(SWT.Selection)) {
                    Event event = new Event();
                    event.type = SWT.Selection;
                    event.widget = item;
                    list.handleEvent(event);
                }

                // for commands that do not refresh the editor, menus worked
                // since the display was covered by the editor and when the menu
                // went away a refresh was forced... this doesn't happen with
                // tear-offs that are in the main dialog, since nothing about
                // the display changes, so we must force a refresh on the editor
                ((AbstractEditor) EditorUtil.getActiveEditor()).refresh();

                // handles the check boxes, if clicking the check box
                // need to not do this (because SWT does it already)
                // otherwise do it
                if (firstItem instanceof Button
                        && firstItem.getStyle() == SWT.CHECK) {
                    if (e.widget != firstItem) {
                        ((Button) firstItem).setSelection(!((Button) firstItem)
                                .getSelection());
                    }
                }

                for (int i = 0; i < parent.getChildren().length; i++) {
                    final MenuItemComposite mic = (MenuItemComposite) parent
                            .getChildren()[i];
                    if (mic.item.getStyle() == SWT.RADIO) {
                        try {
                            MenuItemComposite parent = null;
                            // check whether a Label is clicked or a
                            // MenuItemComposite
                            if (e.widget instanceof MenuItemComposite) {
                                parent = (MenuItemComposite) e.widget;
                            } else {
                                parent = (MenuItemComposite) ((Control) e.widget)
                                        .getParent();
                            }
                            // check that the radio groups match
                            if (mic.getData("radioGroup").equals(
                                    parent.getData("radioGroup"))) {
                                if (!parent.equals(mic)) {
                                    item.setSelection(false);
                                    ((Button) mic.firstItem)
                                            .setSelection(false);
                                } else {
                                    item.setSelection(true);
                                    ((Button) mic.firstItem).setSelection(true);
                                }
                            }
                        } catch (NullPointerException e1) {
                            e1.printStackTrace();
                        }
                    }
                }
            }
        };
        return mouseAdapter;
    }

    @Override
    // TODO XXX make sure we don't leak anything here
    public void dispose() {
        if (arrow != null) {
            arrow.dispose();
        }
        if (highlightedArrow != null) {
            highlightedArrow.dispose();
        }

        if (updateListener != null && !item.isDisposed()) {
            item.removeListener(SWT.Modify, updateListener);
        }
        if (showListener != null && !item.isDisposed()
                && !item.getParent().isDisposed()) {
            item.getParent().removeListener(SWT.Show, showListener);
            topLevelMenu.removeListener(SWT.Show, showListener);
        }
        if (firstItem != null) {
            firstItem.dispose();
        }
        if (secondItem != null) {
            secondItem.dispose();
        }
        if (job != null) {
            job.cancel();
        }
        updateListener = null;
        showListener = null;
        parent.dispose();
        item.dispose();
        super.dispose();
    }

    public void setSelection(boolean selection) {
        if (firstItem instanceof Button) {
            ((Button) firstItem).setSelection(selection);
        }
    }
}