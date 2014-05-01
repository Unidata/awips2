package com.raytheon.uf.viz.productbrowser2;

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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserUtils;

/**
 * Product browser view implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

/** @author mnash */

public class ProductBrowserView2 extends ViewPart {

    public static final String ID = "com.raytheon.uf.viz.productbrowser.ProductBrowserView";

    private Action loadProductAction;

    private Action productInfoAction;

    private Action refreshAction;

    private TreeViewer productTreeViewer;

    public ProductBrowserView2() {
        super();
    }

    @Override
    public void createPartControl(Composite parent) {
        createActions();
        createFullVersion(parent);
    }

    private void createFullVersion(Composite parent) {
        Composite fullComp = new Composite(parent, SWT.FILL);
        fullComp.setLayout(new GridLayout(1, true));
        createToolbar();
        createProductTree(fullComp);
        createProductBrowserContextMenu();
    }

    private void createProductTree(Composite parent) {
        Composite product = new Composite(parent, SWT.FILL);
        GridLayout gridLayout = new GridLayout(1, true);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        product.setLayout(gridLayout);
        product.setLayoutData(gridData);
        productTreeViewer = new TreeViewer(product, SWT.MULTI | SWT.BORDER
                | SWT.FILL | SWT.VIRTUAL);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        productTreeViewer.getTree().setLayoutData(gridData);
        productTreeViewer
                .setContentProvider(new ProductBrowserContentProvider());
        productTreeViewer.setLabelProvider(new ProductBrowserLabelProvider());
        productTreeViewer.setInput(new ProductBrowserLabel());
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        // TODO Auto-generated method stub
    }

    /**
     * Provides the actions available to the product tree
     */
    private void createActions() {
        refreshAction = new Action("Refresh Browser") {
            @Override
            public void run() {
                productTreeViewer.refresh();
            }
        };
        refreshAction.setId("refreshAction");
        refreshAction.setImageDescriptor(ProductBrowserUtils
                .getImageDescriptor("refresh.gif"));

        productInfoAction = new Action("Product Info") {
            @Override
            public void run() {
                // displayInfo();
            }
        };
        productInfoAction.setId("productInfoAction");
        productInfoAction.setImageDescriptor(ProductBrowserUtils
                .getImageDescriptor("help.gif"));

        loadProductAction = new Action("Load Product") {
            @Override
            public void run() {
                // loadProduct(null);
            }
        };
        loadProductAction.setId("loadProductAction");
        loadProductAction.setImageDescriptor(ProductBrowserUtils
                .getImageDescriptor("run.gif"));
    }

    /**
     * Builds the toolbar at the top of the view
     */
    private void createToolbar() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        mgr.add(refreshAction);
    }

    /**
     * Creating the right click menu, for the product tree
     */
    private void createProductBrowserContextMenu() {
        MenuManager menuMgr = new MenuManager();
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {
            @Override
            public void menuAboutToShow(IMenuManager mgr) {
                fillProductBrowserContextMenu(mgr);
            }
        });

        Menu menu = menuMgr.createContextMenu(productTreeViewer.getControl());
        productTreeViewer.getControl().setMenu(menu);
        getSite().registerContextMenu(menuMgr, productTreeViewer);
    }

    /**
     * Takes the menu during the right click and fills it with actions pertinent
     * to the product tree and pertinent to the item that is selected
     * 
     * @param mgr
     */
    protected void fillProductBrowserContextMenu(IMenuManager mgr) {
        if (productTreeViewer.getSelection() != null) {
            // AbstractProductBrowserDataDefinition<?> prod =
            // (AbstractProductBrowserDataDefinition<?>) productTreeViewer
            // .getTree().getSelection()[0].getData("class");
            // // if not a product, do not give opportunity to load things
            // if ((Boolean) productTreeViewer.getTree().getSelection()[0]
            // .getData("product")) {
            // if (prod.getDisplayTypes() != null
            // && !prod.getDisplayTypes().isEmpty()) {
            // MenuManager menuMgr = new MenuManager("Load As...",
            // ProductBrowserUtils.getImageDescriptor("run.gif"),
            // "");
            // for (ResourceType type : prod.getDisplayTypes().keySet()) {
            // if (prod.getDisplayTypes().keySet().size() <= 1) {
            // for (DisplayType types : prod.getDisplayTypes()
            // .get(type)) {
            // menuMgr.add(getDisplayTypeAction(types));
            // }
            // }
            // }
            // mgr.add(menuMgr);
            // } else {
            // mgr.add(loadProductAction);
            // }
            // if (prod instanceof
            // AbstractRequestableProductBrowserDataDefinition<?>) {
            // // mgr.add(bookmarkProductsAction);
            // }
            // }
            mgr.add(productInfoAction);
        }
    }

    private Action getDisplayTypeAction(final DisplayType type) {
        char[] name = type.name().toLowerCase().toCharArray();
        name[0] = Character.toUpperCase(name[0]);
        Action action = new Action((String.valueOf(name))) {
            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.jface.action.Action#run()
             */
            @Override
            public void run() {
                // loadProduct(type);
            }
        };
        return action;
    }
}
