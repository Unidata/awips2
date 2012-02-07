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
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuListener2;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

/**
 * Menu listener that adds item to menu which will open dialog which is the menu
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TearOffMenuListener implements IMenuListener2 {

    private List<Object> openDialogs = new ArrayList<Object>();

    private static final String ID = "tearOffMenuContributionItem";

    private static final String ACTION_ID = "tearOffMenuAction";

    public static final String TEAROFF_PREFERENCE_ID = "tearoffmenus";

    private static boolean enabled;
    static {
        final IPreferenceStore store = com.raytheon.uf.viz.core.Activator
                .getDefault().getPreferenceStore();
        enabled = store.getBoolean(TEAROFF_PREFERENCE_ID);
        store.addPropertyChangeListener(new IPropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent event) {
                enabled = store.getBoolean(TEAROFF_PREFERENCE_ID);
            }
        });
    }

    public TearOffMenuListener() {

    }

    public TearOffMenuListener(IMenuManager mgr) {
        register(mgr.getItems(), this);
    }

    /**
     * When the menu is about to show, check if the list contains that menu
     * manager and if it doesn't add a TearOffContributionItem
     */
    @Override
    public void menuAboutToShow(final IMenuManager manager) {
        register(manager.getItems(), this);
        if (openDialogs.contains(getKey(manager)) == false) {
            // We need to add our item to be first so we need to remove others
            // then add ourself
            IContributionItem[] items = manager.getItems();
            manager.removeAll();
            manager.add(new TearOffContributionItem(manager));
            for (IContributionItem item : items) {
                manager.add(item);
            }
        }
    }

    /**
     * Remove the menu manager from the list, so that the menu item will show
     * back up
     */
    @Override
    public void menuAboutToHide(IMenuManager manager) {
        if (openDialogs.contains(getKey(manager)) == false) {
            manager.remove(ID);
            manager.remove(ACTION_ID);
            unregister(manager.getItems(), this);
        }
    }

    public static void register(IContributionItem[] items,
            IMenuListener listener) {
        for (IContributionItem item : items) {
            if (item instanceof IMenuManager) {
                ((IMenuManager) item).addMenuListener(listener);
            }
        }
    }

    public static void unregister(IContributionItem[] items,
            IMenuListener listener) {
        for (IContributionItem item : items) {
            if (item instanceof IMenuManager) {
                ((IMenuManager) item).removeMenuListener(listener);
            }
        }
    }

    private static Object getKey(IMenuManager manager) {
        Object key = manager;
        if (manager.getId() != null) {
            key = manager.getId();
        }
        return key;
    }

    private class TearOffContributionItem extends ContributionItem {

        private IMenuManager manager;

        /**
         * @param action
         */
        public TearOffContributionItem(IMenuManager manager) {
            super(ID);
            this.manager = manager;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.action.ActionContributionItem#fill(org.eclipse.
         * swt.widgets.Menu, int)
         */
        @Override
        public void fill(Menu menu, int index) {
            String longest = "";
            for (MenuItem item : menu.getItems()) {
                String check = item.getText();
                if (check.length() > longest.length()) {
                    longest = check;
                }
            }
            byte[] bytes = new byte[longest.length() * 2];
            Arrays.fill(bytes, (byte) '|');
            // String filled = new String(bytes);
            String filled = "- - - - - - TEAR-OFF : "
                    + menu.getParentItem().getText() + " - - - - - -";
            // String filled = "-" * bytes.length

            new ActionContributionItem(new TearOffAction(filled, manager, menu))
                    .fill(menu, index);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.ContributionItem#isVisible()
         */
        @Override
        public boolean isVisible() {
            return super.isVisible() && enabled;
        }

    }

    private class TearOffAction extends Action {

        private IMenuManager manager;

        private Menu menu;

        private TearOffAction(String text, final IMenuManager manager, Menu menu) {
            super(text);
            this.manager = manager;
            this.menu = menu;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            TearOffMenuDialog dialog = new TearOffMenuDialog(menu);
            dialog.addListener(SWT.Dispose, new Listener() {
                @Override
                public void handleEvent(Event event) {
                    openDialogs.remove(getKey(manager));
                    manager.remove(ID);
                    manager.remove(ACTION_ID);
                    unregister(manager.getItems(), TearOffMenuListener.this);
                }
            });
            openDialogs.add(getKey(manager));
            register(manager.getItems(), TearOffMenuListener.this);
            dialog.open();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getId()
         */
        @Override
        public String getId() {
            return ACTION_ID;
        }
    }
}
