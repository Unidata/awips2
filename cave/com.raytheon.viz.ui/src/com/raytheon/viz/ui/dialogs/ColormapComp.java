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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapTree;
import com.raytheon.uf.viz.core.drawables.ColorMapTreeFactory;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;

/**
 * Cascading control for colormaps
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 26, 2010           mschenke    Initial creation
 * Sep 18, 2013  2421     bsteffen    Use ColorMapTree for asyncronous loading.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColormapComp {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ColormapComp.class);

    public static interface IColormapCompChangeListener {
        public void colormapChanged(String colorMap);
    }

    private Set<IColormapCompChangeListener> listeners = new HashSet<IColormapCompChangeListener>();

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
        cmapPopupMenu.addMenuListener(new MenuPopulator(cmapPopupMenu,
                ColorMapTreeFactory.getBaseTree()));
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

    public Menu getMenu() {
        return cmapPopupMenu;
    }

    public boolean isDisposed() {
        if (cmapButton != null && cmapButton.isDisposed()) {
            return true;
        }
        return false;
    }

    protected void changeColormap(String name) {
        if (cmapButton != null) {
            cmapButton.setText(name);
        }

        try {
            params.setColorMap(ColorMapLoader.loadColorMap(name));
            cap.notifyResources();
            for (IColormapCompChangeListener listener : listeners) {
                listener.colormapChanged(name);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR, "Unable to change colormap.",
                    e);
        }
    }

    /**
     * Class to recursively populate a menu based off the contents of a
     * {@link ColorMapTree}. The menu is not populated until it is shown. This
     * is also an eclipse Job that will automatically try to prefetch some
     * information from the tree for faster population.
     */
    private class MenuPopulator extends Job implements MenuListener {

        private final Menu menu;

        private final ColorMapTree tree;

        public MenuPopulator(Menu menu, ColorMapTree tree) {
            super("Loading Color Maps");
            this.menu = menu;
            this.tree = tree;
            schedule();
        }

        @Override
        public void menuHidden(MenuEvent e) {
            /* Do Nothing */
        }

        /**
         * Get all the subTrees that will be displayed and call
         * optimizeIsEmpty(). isEmpty() is usually the slowest operation in
         * menuShown because it can go recursive. optimizeIsEmpty ensures that
         * future calls(from menuShown) are as fast as possible.
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            for (ColorMapTree subTree : tree.getSubTrees()) {
                subTree.optimizeIsEmpty();
            }
            for (ColorMapTree subTree : getLevelTrees()) {
                subTree.optimizeIsEmpty();
            }
            return Status.OK_STATUS;
        }

        /**
         * Fill the menu with items from the tree. Always create menus at the
         * top even if there are other items
         */
        @Override
        public void menuShown(MenuEvent e) {
            int index = 0;
            List<ColorMapTree> subTrees = tree.getSubTrees();
            Collections.sort(subTrees, new Comparator<ColorMapTree>() {

                @Override
                public int compare(ColorMapTree tree1, ColorMapTree tree2) {
                    return tree1.getName().compareToIgnoreCase(tree2.getName());
                }

            });
            for (ColorMapTree subTree : subTrees) {
                if (!subTree.isEmpty()) {
                    addSubTree(subTree, index++);
                }
            }
            List<LocalizationFile> files = tree.getColorMapFiles();
            Collections.sort(files, new Comparator<LocalizationFile>() {

                @Override
                public int compare(LocalizationFile file1,
                        LocalizationFile file2) {
                    return file1.getName().compareToIgnoreCase(file2.getName());
                }

            });
            for (LocalizationFile file : files) {
                addFile(file, index++);
            }

            for (ColorMapTree subTree : getLevelTrees()) {
                if (!subTree.isEmpty()) {
                    addSubTree(subTree, index++);
                }
            }
            menu.removeMenuListener(this);
        }

        /**
         * The root menu should show not only the base tree, but also an
         * additional menu item for each localization level(SITE, USER ...),
         * this method gets those trees when they are needed so menu items can
         * be added.
         */
        private List<ColorMapTree> getLevelTrees() {
            if (menu == cmapPopupMenu) {
                List<ColorMapTree> trees = new ArrayList<ColorMapTree>();
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationLevel[] levels = pm.getAvailableLevels();
                for (LocalizationLevel level : levels) {
                    if (level != LocalizationLevel.BASE) {
                        ColorMapTree tree = ColorMapTreeFactory
                                .getTreeForLevel(level);
                        trees.add(tree);

                    }
                }
                return trees;
            } else {
                return Collections.emptyList();
            }
        }

        private void addSubTree(ColorMapTree tree, int index) {
            MenuItem item = new MenuItem(menu, SWT.CASCADE, index);
            item.setText(tree.getName());
            Menu subMenu = new Menu(shell, SWT.DROP_DOWN);
            item.setMenu(subMenu);
            subMenu.addMenuListener(new MenuPopulator(subMenu, tree));
        }

        private void addFile(LocalizationFile file, int index) {
            MenuItem item = new MenuItem(menu, SWT.None, index);
            final String name = ColorMapLoader.shortenName(file);
            int start = name.lastIndexOf(PathManager.SEPARATOR);
            if (start >= 0) {
                item.setText(name.substring(start + 1));
            } else {
                item.setText(name);
            }
            item.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    changeColormap(name);
                }

            });
        }

    }

}
