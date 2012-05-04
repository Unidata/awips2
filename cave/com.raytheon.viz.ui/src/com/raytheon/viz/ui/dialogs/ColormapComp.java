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
package com.raytheon.viz.ui.dialogs;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;

/**
 * Cascading control for colormaps
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColormapComp {

    public static interface IColormapCompChangeListener {
        public void colormapChanged(String colorMap);
    }

    private class ColorMapMenuItem {

        /** null if not leaf node */
        private LocalizationFile cmapFile;

        private Menu subMenu;

        private MenuItem actualItem;

        private Menu parent;

        private String text;

        /**
         * @param parent
         * @param style
         */
        public ColorMapMenuItem(Menu parent, Menu subMenu,
                LocalizationFile cmapFile, String text) {
            this.parent = parent;
            this.subMenu = subMenu;
            this.cmapFile = cmapFile;
            this.text = text;

            List<ColorMapMenuItem> children = childMap.get(parent);
            if (children == null) {
                children = new ArrayList<ColorMapMenuItem>();
                childMap.put(parent, children);
            }
            children.add(this);
        }

        public void initializeComponents() {
            actualItem = new MenuItem(parent, subMenu == null ? SWT.NONE
                    : SWT.CASCADE);
            actualItem.setText(text);
            if (subMenu != null) {
                actualItem.setMenu(subMenu);
            }

            actualItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (cmapFile != null) {
                        if (cmapButton != null) {
                            cmapButton.setText(cmapFile.getName()
                                    .replace("colormaps" + File.separator, "")
                                    .replace(".cmap", ""));
                        }

                        try {
                            String shortName = ColorMapLoader
                                    .shortenName(cmapFile);
                            IColorMap cxml = ColorMapLoader
                                    .loadColorMap(shortName);
                            ColorMap glColorMap = new ColorMap(ColorMapLoader
                                    .shortenName(cmapFile), (ColorMap) cxml);
                            params.setColorMap(glColorMap);
                            cap.notifyResources();

                            for (IColormapCompChangeListener listener : listeners) {
                                listener.colormapChanged(shortName);
                            }
                        } catch (VizException e1) {
                            e1.printStackTrace();
                        }
                    }
                }
            });
        }

        public Menu getSubMenu() {
            return subMenu;
        }

    }

    private Set<IColormapCompChangeListener> listeners = new HashSet<IColormapCompChangeListener>();

    private Map<Menu, List<ColorMapMenuItem>> childMap = new HashMap<Menu, List<ColorMapMenuItem>>();

    private Button cmapButton;

    private Shell shell;

    private Menu cmapPopupMenu;

    private ColorMapParameters params;

    private ColorMapCapability cap;

    private Menu parentMenu;

    /**
     * @param parent
     * @param style
     */
    public ColormapComp(Composite parent, ColorMapParameters params,
            ColorMapCapability cap) {
        this.params = params;
        this.cap = cap;
        this.shell = parent.getShell();
        cmapButton = new Button(parent, SWT.PUSH | SWT.DROP_DOWN);
        initializeComponents();
    }

    public ColormapComp(Menu parentMenu, ColorMapParameters params,
            ColorMapCapability cap) {
        this.parentMenu = parentMenu;
        this.params = params;
        this.cap = cap;
        this.shell = parentMenu.getShell();
        initializeComponents();
    }

    public void addChangeListener(IColormapCompChangeListener listener) {
        this.listeners.add(listener);
    }

    public Button getCMapButton() {
        return cmapButton;
    }

    public void setParams(ColorMapParameters params) {
        this.params = params;
    }

    public void setCap(ColorMapCapability cap) {
        this.cap = cap;
    }

    public void refreshItems() {
        if (cmapPopupMenu != null) {
            cmapPopupMenu.dispose();
        }

        if (cmapButton == null) {
            cmapPopupMenu = new Menu(parentMenu);
        } else {
            cmapPopupMenu = new Menu(cmapButton);
            if (params != null && params.getColorMapName() != null) {
                cmapButton.setText(params.getColorMapName());
            }
        }

        cmapPopupMenu.setVisible(false);

        LocalizationFile[] files = ColorMapLoader.listColorMapFiles();
        for (LocalizationFile file : files) {

            List<String> actualItems = new ArrayList<String>();

            String[] split = ColorMapLoader.shortenName(file).split("[/\\\\]"); // Win32

            Menu lastParent = cmapPopupMenu;

            // Just to be sure, get rid of empty items
            for (String text : split) {
                if (text.trim().equals("") == false) {
                    actualItems.add(text);
                }
            }

            for (int i = 0; i < actualItems.size(); ++i) {
                if (i == actualItems.size() - 1) {
                    // leaf node
                    new ColorMapMenuItem(lastParent, null, file,
                            actualItems.get(i));
                } else {
                    String text = actualItems.get(i);
                    boolean found = false;
                    // traverse lastParent's children to see if we already exist
                    List<ColorMapMenuItem> children = childMap.get(lastParent);
                    if (children == null) {
                        children = new ArrayList<ColorMapMenuItem>();
                        childMap.put(lastParent, children);
                    }
                    for (ColorMapMenuItem item : children) {
                        if (item.subMenu != null && item.text.equals(text)) {
                            found = true;
                            lastParent = item.subMenu;
                            break;
                        }
                    }

                    if (!found) {
                        Menu cmapParent = lastParent;
                        lastParent = new Menu(shell, SWT.DROP_DOWN);
                        new ColorMapMenuItem(cmapParent, lastParent, null, text);
                    }
                }
            }
        }
        sortMenus(cmapPopupMenu);
    }

    /**
     * @param parent
     */
    private void initializeComponents() {

        if (cmapButton != null) {
            GridData gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
            gd.widthHint = 250;
            cmapButton.setLayoutData(gd);

            cmapButton.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Point controlLoc = cmapButton.getDisplay().map(cmapButton,
                            null, e.x, e.y + cmapButton.getSize().y);
                    cmapPopupMenu.setLocation(controlLoc);
                    cmapPopupMenu.setVisible(true);
                }
            });
        }
        refreshItems();
    }

    /**
     * @param parent
     */
    private void sortMenus(Menu parent) {
        List<ColorMapMenuItem> children = childMap.get(parent);
        if (children == null || children.size() == 0) {
            return;
        }

        Collections.sort(children, new Comparator<ColorMapMenuItem>() {
            @Override
            public int compare(ColorMapMenuItem o1, ColorMapMenuItem o2) {
                // catch user menu item
                if (o1.text.toUpperCase().equals("USER")) {
                    // user is always last
                    return 1;
                } else if (o2.text.toUpperCase().equals("USER")) {
                    return -1;
                }

                // catch site menu item
                if (o1.text.toUpperCase().equals("SITE")) {
                    // site is after all but user ( user gets caught first )
                    return 1;
                } else if (o2.text.toUpperCase().equals("SITE")) {
                    return -1;
                }

                // all other menu items
                if (o1.subMenu != null && o2.subMenu == null) {
                    // Show those with submenus first
                    return -1;
                } else if (o1.subMenu == null && o2.subMenu != null) {
                    // Show those with submenus first
                    return 1;
                } else {
                    // else look at text
                    return o1.text.toLowerCase().compareTo(
                            o2.text.toLowerCase());
                }
            }
        });

        for (ColorMapMenuItem child : children) {
            child.initializeComponents();
        }

        for (ColorMapMenuItem child : children) {
            if (child.subMenu != null) {
                sortMenus(child.subMenu);
            }
        }
    }

    public Menu getMenu() {
        return cmapPopupMenu;
    }

    public boolean isDisposed() {
        if (cmapButton != null && cmapButton.isDisposed()) {
            return true;
        }
        return false;
    }
}
