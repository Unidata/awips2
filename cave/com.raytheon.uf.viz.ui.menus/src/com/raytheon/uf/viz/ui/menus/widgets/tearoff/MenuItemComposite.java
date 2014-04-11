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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.CommandEvent;
import org.eclipse.core.commands.ICommandListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.menus.CommandContributionItem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.ui.menus.widgets.tearoff.TearOffMenuDialog.MenuPathElement;

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
 * Apr 10, 2013 DR 15185   D. Friedman Preserve tear-offs over perspective switches.
 * Jev 26, 2014    2842    mpduff    Utilize the command listener.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class MenuItemComposite extends Composite {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MenuItemComposite.class);

    private boolean separator = false;

    private Control firstItem;

    private Control secondItem;

    // backing data for executing listeners
    private MenuItem item;

    private MenuPathElement itemPath;

    private Image arrow = null;

    private Image highlightedArrow = null;

    private Listener updateListener = null;

    private SelectionListener radioListener = null;

    private ICommandListener commandListener;

    private List<String> myPath;

    /** Enabled color */
    private final Color enabledColor;

    /** Disabled color */
    private final Color disabledColor;

    /**
     * @param parent
     * @param style
     */
    public MenuItemComposite(Composite parent, int style) {
        super(parent, style);
        enabledColor = Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);
        disabledColor = Display.getCurrent()
                .getSystemColor(SWT.COLOR_DARK_GRAY);
    }

    /**
     * Creates both labels and ties them together
     */
    public void addLabels(MenuItem it, int labelStyle) {
        if (it.isDisposed()) {
            return;
        }

        myPath = new ArrayList<String>();

        // Build the menu path to the MenuItem from highest menu level
        Menu parent = it.getParent();
        MenuItem toAdd = it;
        do {
            myPath.add(toAdd.getText());
            toAdd = parent.getParentItem();
            parent = parent.getParentMenu();
        } while (parent.getParentMenu() != null);

        Collections.reverse(myPath);

        item = it;

        itemPath = new MenuPathElement(it);

        String[] labels = item.getText().split("\t");
        // handle for a separator menu item
        if (item.getStyle() == SWT.SEPARATOR) {
            separator = true;
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
                createRadioListener();
            } else if (item.getStyle() == SWT.CASCADE) {
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

                // Create and add text update listener
                createUpdateListener();
            }

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

            if (!item.isEnabled()) {
                setForeground(disabledColor);
            }
        }

        addItemListeners();
    }

    /**
     * 
     */
    private void addItemListeners() {
        if (item == null) {
            return;
        }

        if (updateListener != null) {
            item.addListener(SWT.Modify, updateListener);
        }
        if (radioListener != null) {
            item.addSelectionListener(radioListener);
        }

        if (item.getData() instanceof CommandContributionItem) {
            final Command c = ((ICommandService) PlatformUI.getWorkbench()
                    .getService(ICommandService.class))
                    .getCommand(((CommandContributionItem) item.getData())
                            .getCommand().getId());

            commandListener = new ICommandListener() {
                @Override
                public void commandChanged(CommandEvent commandEvent) {
                    if (item.isDisposed() || firstItem.isDisposed()
                            || secondItem.isDisposed()) {
                        return;
                    }

                    if (item.getData() instanceof CommandContributionItem) {
                        CommandContributionItem itm = (CommandContributionItem) item
                                .getData();
                        if (itm.getCommand().getId().equals(c.getId())) {
                            boolean enabled = true;
                            if (commandEvent.getCommand().getHandler() != null) {
                                enabled = commandEvent.getCommand()
                                        .getHandler().isEnabled();
                            } else {
                                enabled = commandEvent.getCommand().isEnabled();
                            }

                            firstItem.setEnabled(enabled);
                            secondItem.setEnabled(enabled);
                            if (enabled) {
                                setForeground(enabledColor);
                            } else {
                                setForeground(disabledColor);
                                setBackground(Display.getCurrent()
                                        .getSystemColor(
                                                SWT.COLOR_WIDGET_BACKGROUND));

                                // changes the arrow image to the unhighlighted
                                // version
                                if (secondItem instanceof Label) {
                                    if (((Label) secondItem).getImage() != null) {
                                        ((Label) secondItem).setImage(arrow);
                                    }
                                }
                            }
                        }
                    }
                }
            };

            c.addCommandListener(commandListener);
        }
    }

    /**
     * 
     */
    private void createRadioListener() {
        radioListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (e.widget instanceof MenuItem) {
                    // check that the radio groups match
                    for (Control comp : firstItem.getParent().getParent()
                            .getChildren()) {
                        MenuItemComposite composite = (MenuItemComposite) comp;
                        if (composite.getItem().getText()
                                .equals(((MenuItem) e.widget).getText())) {
                            if (composite.firstItem instanceof Button) {
                                ((Button) composite.firstItem)
                                        .setSelection(composite.getItem()
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
        };
    }

    protected void createUpdateListener() {
        updateListener = new Listener() {
            @Override
            public void handleEvent(Event event) {
                if (secondItem != null && !secondItem.isDisposed()) {
                    if (getItem() == event.data) {
                        if (((MenuItem) event.data).getText().split("\t").length > 1) {
                            ((Label) secondItem)
                                    .setText(((MenuItem) event.data).getText()
                                            .split("\t")[1]);
                            // don't want to make the times go off the
                            // screen
                            layout();
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

    private void createSubMenu(MenuItem item, int y) {
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
                if (item.isEnabled()) {
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
            }

            @Override
            public void mouseExit(MouseEvent e) {
                // we want all the colors to be the same for background
                // and foreground, so we set that here, this is to
                // unhighlight the whole thing
                if (item.isEnabled()) {
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
                final MenuItem item = getItem();
                if (!item.isEnabled()) {
                    return;
                }
                if (item.getMenu() != null) {
                    // This is item opens a submenu, get the y offset based on
                    // the location of the click
                    int y = 0;
                    if (e.widget instanceof MenuItemComposite) {
                        y = ((Control) e.widget).getLocation().y;
                    } else {
                        y = ((Control) e.widget).getParent().getLocation().y;
                    }
                    createSubMenu(item, y);
                    return;
                }

                // handle the selection event, so if it is able to load
                // something, do it (by looping over ALL the selection
                // listeners assigned to the item)
                for (Listener list : item.getListeners(SWT.Selection)) {
                    Event event = new Event();
                    event.type = SWT.Selection;
                    event.widget = item;
                    if (item.isEnabled()) {
                        list.handleEvent(event);
                    }
                }

                if (isDisposed()) {
                    return;
                }

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

                // Handle radio selection changing...
                Control[] siblings = getParent().getChildren();
                for (int i = 0; i < siblings.length; i++) {
                    final MenuItemComposite mic = (MenuItemComposite) siblings[i];
                    if (mic.separator == false
                            && mic.getItem().getStyle() == SWT.RADIO) {
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
                                if (!parent
                                        .getItem()
                                        .getText()
                                        .replaceAll("&", "")
                                        .equals(mic.getItem().getText()
                                                .replaceAll("&", ""))) {
                                    mic.getItem().setSelection(false);
                                    ((Button) mic.firstItem)
                                            .setSelection(false);
                                } else {
                                    mic.getItem().setSelection(true);
                                    ((Button) mic.firstItem).setSelection(true);
                                }
                            }
                        } catch (NullPointerException e1) {
                            statusHandler.error("Error executing menu action.",
                                    e1);
                        }
                    }
                }
            }
        };
        return mouseAdapter;
    }

    @Override
    public void dispose() {
        if (arrow != null) {
            arrow.dispose();
        }
        if (highlightedArrow != null) {
            highlightedArrow.dispose();
        }

        if (item != null) {
            if (updateListener != null && !item.isDisposed()) {
                item.removeListener(SWT.Modify, updateListener);
            }

            if (radioListener != null && !item.isDisposed()) {
                item.removeSelectionListener(radioListener);
            }
        }

        if (item.getData() instanceof CommandContributionItem) {
            ICommandService service = (ICommandService) PlatformUI
                    .getWorkbench().getService(ICommandService.class);
            Command c = service.getCommand(((CommandContributionItem) item
                    .getData()).getCommand().getId());
            c.removeCommandListener(commandListener);
        }

        super.dispose();
    }

    public void setSelection(boolean selection) {
        if (firstItem instanceof Button) {
            ((Button) firstItem).setSelection(selection);
        }
    }

    private MenuItem getItem() {
        MenuItem item = getItemIfAvailable();
        if (item == null) {
            throw new IllegalStateException(String.format(
                    "Could not find target of tear-off menu item \"%s\"",
                    itemPath.getName()));
        }
        return item;
    }

    private MenuItem getItemIfAvailable() {
        if (item == null || item.isDisposed()) {
            item = findItem();
            addItemListeners();
        }
        return item;
    }

    private MenuItem findItem() {
        Menu menu = getTargetMenu();
        if (menu != null) {
            return TearOffMenuDialog.findItem(menu, itemPath);
        } else {
            return null;
        }
    }

    private Menu getTargetMenu() {
        return getDialog().getTargetMenu();
    }

    private TearOffMenuDialog getDialog() {
        return (TearOffMenuDialog) getShell().getData();
    }

    public void reconnect() {
        getItemIfAvailable();
    }
}