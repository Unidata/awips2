package edu.ucar.unidata.uf.viz.radarbrowser;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;

import edu.ucar.unidata.uf.viz.radarbrowser.jobs.RadarBrowserInitializeJob;
import edu.ucar.unidata.uf.viz.radarbrowser.jobs.RadarBrowserQueryJob;

/**
 * Product browser view implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 03, 2010           mnash       Initial creation
 * May 02, 2013  1949     bsteffen    Switch Product Browser from uengine to
 *                                    DbQueryRequest.
 * May 13, 2014  3135     bsteffen    Make all queries async.
 * 
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class RadarBrowserView extends ViewPart {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarBrowserView.class);

    public static final String ID = "edu.ucar.unidata.uf.viz.radarbrowser.RadarBrowserView";

    public static final String LABEL_DATA_KEY = "label";

    public static final String DEF_DATA_KEY = "defintion";

    private static IExtension[] extensions;

    private Tree productTree;

    private Action loadProductAction;

    private Action productInfoAction;

    private Action refreshAction;

    @Override
    public void createPartControl(Composite parent) {
        Composite fullComp = new Composite(parent, SWT.FILL);
        fullComp.setLayout(new GridLayout(1, true));
        getDataTypes();
        createActions();
        createToolbar();
        createProductTree(fullComp);
        createProductBrowserContextMenu();
    }

    /**
     * Provides the actions available to the product tree
     */
    private void createActions() {
        refreshAction = new Action("Refresh Browser") {
            @Override
            public void run() {
                populateInitialProductTree();
            }
        };
        refreshAction.setId("refreshAction");
        refreshAction.setImageDescriptor(RadarBrowserUtils
                .getImageDescriptor("refresh.gif"));

        productInfoAction = new Action("Product Info") {
            @Override
            public void run() {
                displayInfo();
            }
        };
        productInfoAction.setId("productInfoAction");
        productInfoAction.setImageDescriptor(RadarBrowserUtils
                .getImageDescriptor("help.gif"));

        loadProductAction = new Action("Load Product") {
            @Override
            public void run() {
                loadProduct(null);
            }
        };
        loadProductAction.setId("loadProductAction");
        loadProductAction.setImageDescriptor(RadarBrowserUtils
                .getImageDescriptor("run.gif"));
    }


    /**
     * Creates the product browser that spans the entire view...
     * 
     * @param parent
     */
    public void createProductTree(final Composite parent) {
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        productTree = new Tree(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.BORDER);
        productTree.setLayoutData(gridData);
        productTree.addListener(SWT.MouseDoubleClick, new Listener() {
            @Override
            public void handleEvent(Event event) {
                if (event.button == 1) {
                    loadProductAction.run();
                }
            }
        });

        productTree.addListener(SWT.Expand, new Listener() {
            @Override
            public void handleEvent(Event event) {
                RadarBrowserQueryJob.startJob((TreeItem) event.item);
            }
        });

        productTree.addListener(SWT.MouseMove, new Listener() {
            @Override
            public void handleEvent(Event event) {
                productTree.setToolTipText("");
            }
        });
        populateInitialProductTree();
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

        Menu menu = menuMgr.createContextMenu(productTree);
        productTree.setMenu(menu);
    }


    /**
     * Takes the menu during the right click and fills it with actions pertinent
     * to the product tree and pertinent to the item that is selected
     * 
     * @param mgr
     */
    protected void fillProductBrowserContextMenu(IMenuManager mgr) {
        TreeItem[] selection = productTree.getSelection();
        if (selection != null && selection.length > 0) {
            RadarBrowserLabel label = getLabel(selection[0]);
            if (label == null) {
                return;
            }
            // if not a product, do not give opportunity to load things
            if (label.isProduct()) {
                AbstractProductBrowserDataDefinition<?> def = getDataDef(selection[0]);
                Map<ResourceType, List<DisplayType>> displayTypes = def
                        .getDisplayTypes();
                if (displayTypes != null && displayTypes.isEmpty() == false) {
                    MenuManager menuMgr = new MenuManager("Load As...",
                            RadarBrowserUtils.getImageDescriptor("run.gif"),
                            "");
                    for (ResourceType type : displayTypes.keySet()) {
                        if (displayTypes.keySet().size() <= 1) {
                            for (DisplayType types : displayTypes.get(type)) {
                                menuMgr.add(getDisplayTypeAction(types));
                            }
                        }
                    }
                    mgr.add(menuMgr);
                } else {
                    mgr.add(loadProductAction);
                }
            }
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
                loadProduct(type);
            }
        };
        return action;
    }

    /**
     * Take the initial plugins available and populate them if they have any
     * data. This function does not populate the tree with the data, just the
     * plugin names.
     * 
     * @param popData
     */
    public void populateInitialProductTree() {
        productTree.removeAll();
        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();
            for (IConfigurationElement element : config) {
                AbstractProductBrowserDataDefinition<?> prod = null;
                try {
                    prod = (AbstractProductBrowserDataDefinition<?>) element
                            .createExecutableExtension("class");
                } catch (CoreException e) {
                    statusHandler
                            .error("A product browser data definition has failed to load.",
                                    e);
                    continue;
                }
                String displayText = "Checking Availability of "
                        + prod.displayName + "...";
                /* Sort alphabetically. */
                int index = 0;
                for (TreeItem items : productTree.getItems()) {
                    if (items.getText().compareToIgnoreCase(displayText) > 0) {
                        break;
                    } else {
                        index++;
                    }
                }
                TreeItem ti = new TreeItem(productTree, SWT.NONE, index);
                ti.setText(displayText);
                RadarBrowserLabel label = new RadarBrowserLabel(
                        prod.displayName, prod.displayName);
                if (prod instanceof AbstractRequestableProductBrowserDataDefinition<?>) {
                    AbstractRequestableProductBrowserDataDefinition<?> arpbdd = (AbstractRequestableProductBrowserDataDefinition<?>) prod;
                    label.setData(arpbdd.productName);
                    label.setProduct(arpbdd.order.length == 0);
                }
                ti.setData(LABEL_DATA_KEY, label);
                ti.setData(DEF_DATA_KEY, prod);
                Font font = ti.getFont();
                FontData fontData = font.getFontData()[0];
                fontData = new FontData(fontData.getName(),
                        fontData.getHeight(), SWT.BOLD);
                font = new Font(ti.getDisplay(), fontData);
                ti.setFont(font);
                new RadarBrowserInitializeJob(ti).schedule();
            }
        }
    }

    /**
     * Using reflection and the eclipse registry, agnostically finds the
     * available types to populate the tree
     */
    private void getDataTypes() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint(RadarBrowserUtils.DATA_DEFINITION_ID);
        if (point != null) {
            extensions = point.getExtensions();
        } else {
            extensions = new IExtension[0];
        }
    }

    private void loadProduct(DisplayType type) {
        TreeItem[] selection = productTree.getSelection();
        if (selection != null) {
            for (TreeItem item : selection) {
                RadarBrowserLabel label = getLabel(selection[0]);
                if (label == null) {
                    return;
                }
                // if not a product, do not give opportunity to load things
                if (label.isProduct()) {
                    AbstractProductBrowserDataDefinition<?> def = getDataDef(item);
                    String[] path = getProductURI(item, false);
                    if (type != null) {
                        def.loadProperties
                                .getCapabilities()
                                .getCapability(def.resourceData,
                                        DisplayTypeCapability.class)
                                .setDisplayType(type);
                    }
                    def.constructResource(path, null);
                }
            }
        }
    }

    /**
     * Adds tooltip text to the tree with information about the selected item
     */
    private void displayInfo() {
        StringBuilder stringBuilder = new StringBuilder();
        for (TreeItem ti : productTree.getSelection()) {
            if (stringBuilder.length() > 0) {
                stringBuilder.append("\n---------------\n");
            }
            AbstractProductBrowserDataDefinition<?> prod = getDataDef(ti);
            String[] info = getProductURI(ti, false);
            if (prod instanceof AbstractRequestableProductBrowserDataDefinition<?>) {
                AbstractRequestableProductBrowserDataDefinition<?> aProd = (AbstractRequestableProductBrowserDataDefinition<?>) prod;
                stringBuilder.append(aProd.PLUGIN_NAME + " = "
                        + aProd.productName);
            } else {
                stringBuilder.append(prod.displayName);
            }
            for (int i = 1; i < info.length; i++) {
                stringBuilder.append("\n");
                if (prod instanceof AbstractRequestableProductBrowserDataDefinition<?>) {
                    stringBuilder
                            .append(((AbstractRequestableProductBrowserDataDefinition<?>) prod).order[i - 1]
                                    + " = " + info[i]);
                } else {
                    stringBuilder.append(info[i]);
                }
            }
            productTree.setToolTipText(stringBuilder.toString());
        }
    }

    @Override
    public void setFocus() {
        productTree.setFocus();
    }

    /**
     * Using the tree item, gets the URI based on where it is within the tree
     * 
     * @param item
     * @return
     */
    public static String[] getProductURI(TreeItem item, boolean isName) {
        List<String> data = new ArrayList<String>();
        while (item != null) {
            RadarBrowserLabel label = getLabel(item);
            if (isName || label.getData() == null) {
                data.add(label.getName());
            } else {
                data.add(label.getData());
            }
            item = item.getParentItem();
        }
        Collections.reverse(data);
        return data.toArray(new String[0]);
    }

    public static RadarBrowserLabel getLabel(TreeItem item) {
        return (RadarBrowserLabel) item.getData(LABEL_DATA_KEY);
    }

    public static AbstractProductBrowserDataDefinition<?> getDataDef(
            TreeItem item) {
        return (AbstractProductBrowserDataDefinition<?>) item
                .getData(DEF_DATA_KEY);
    }
}
