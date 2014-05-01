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
package com.raytheon.uf.viz.productbrowser2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;

/**
 * Product browser content provider to hold the necessary content to be used by
 * ProductBrowserView2
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ProductBrowserContentProvider implements ITreeContentProvider {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductBrowserContentProvider.class);

    private IConfigurationElement[] config;

    private IExtension[] extensions;

    private static final String DATA_DEFINITION_ID = "com.raytheon.uf.viz.productbrowser.dataDefinition";

    private final String NO_PRODUCTS_AVAILABLE = "No Products Available";

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IContentProvider#dispose()
     */
    @Override
    public void dispose() {
        // TODO Auto-generated method stub
    }

    /**
     * When updates come in, need to change input and add items to tree
     */
    @Override
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        // add item to tree
        Job addJob = new Job("Adding Item...") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                System.out.println("updating input");
                return Status.OK_STATUS;
            }
        };
        addJob.schedule();
    }

    /**
     * Take the initial plugins available and populate them if they have any
     * data. This function does not populate the tree with the data, just the
     * plugin names.
     * 
     * @param popData
     */
    @Override
    public Object[] getElements(Object inputElement) {
        // TODO
        populateDataTypes();
        try {
            return populateInitialProductTree().toArray();
        } catch (Exception e) {
            e.printStackTrace();
        }
        ProductBrowserLabel[] none = new ProductBrowserLabel[1];
        ProductBrowserLabel empty = new ProductBrowserLabel();
        empty.setProduct(false);
        empty.setName(NO_PRODUCTS_AVAILABLE);
        none[0] = empty;
        return none;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.
     * Object)
     */
    @Override
    public Object[] getChildren(Object parentElement) {
        ProductBrowserLabel popData = (ProductBrowserLabel) parentElement;
        List<String> elements = new ArrayList<String>();
        ProductBrowserLabel element = (ProductBrowserLabel) parentElement;
        while (element != null) {
            elements.add(element.getData());
            Object o = getParent(element);
            if (o instanceof ProductBrowserLabel) {
                element = (ProductBrowserLabel) getParent(element);
            } else {
                break;
            }
        }
        try {
            List<ProductBrowserLabel> products = popData
                    .getDefinition()
                    .populateData(elements.toArray(new String[elements.size()]));
            for (ProductBrowserLabel label : products) {
                label.setDefinition(popData.getDefinition());
            }
            return products.toArray();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to expand product browser tree", e);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object
     * )
     */
    @Override
    public Object getParent(Object element) {
        if (element instanceof ProductBrowserLabel) {
            return ((ProductBrowserLabel) element).getData();
        }
        return null;
    }

    /**
     * Using reflection and the eclipse registry, agnostically finds the
     * available types to populate the tree
     */
    private void populateDataTypes() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(DATA_DEFINITION_ID);
        if (point != null) {
            extensions = point.getExtensions();
        } else {
            extensions = new IExtension[0];
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ILazyTreeContentProvider#updateElement(java
     * .lang.Object, int)
     */
    // @Override
    public void updateElement(Object parent, int index) {
        // TODO Auto-generated method stub
        System.out.println("update element");
    }

    /**
     * If it is a product (loadable) and is not no data available then it
     * obviously has a child
     */
    @Override
    public boolean hasChildren(Object element) {
        if (((ProductBrowserLabel) element).isProduct()
                || NO_PRODUCTS_AVAILABLE.equals(((ProductBrowserLabel) element)
                        .getName())) {
            return false;
        }
        return true;
    }

    /**
     * Populate the initial tree
     */
    private List<ProductBrowserLabel> populateInitialProductTree() {
        List<ProductBrowserLabel> rootElements = new ArrayList<ProductBrowserLabel>();
        for (IExtension ext : extensions) {
            config = ext.getConfigurationElements();
            for (IConfigurationElement element : config) {
                try {
                    ProductBrowserLabel label = new ProductBrowserLabel();
                    AbstractProductBrowserDataDefinition<?> prod = (AbstractProductBrowserDataDefinition<?>) element
                            .createExecutableExtension("class");
                    String productName = prod.populateInitial();
                    if (productName == null) {
                        continue;
                    }
                    label.setName(productName);
                    label.setDefinition(prod);
                    if (prod instanceof AbstractRequestableProductBrowserDataDefinition<?>) {
                        label.setData(((AbstractRequestableProductBrowserDataDefinition<?>) prod).productName);
                    }
                    label.setProduct(false);
                    rootElements.add(label);
                } catch (CoreException e) {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Unable to get root elements of product browser",
                                    e);
                }
            }
        }
        Collections.sort(rootElements);
        return rootElements;
    }
}
