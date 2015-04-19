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
package edu.ucar.unidata.uf.viz.radarbrowser.jobs;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;

import edu.ucar.unidata.uf.viz.radarbrowser.RadarBrowserLabel;
import edu.ucar.unidata.uf.viz.radarbrowser.RadarBrowserView;

/**
 * 
 * Job for performing the population of the {@link RadarBrowserView} tree
 * asynchronously. To avoid querying multiple times on the same item all
 * instances of this class should be started with {@link #startJob(TreeItem)}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 13, 2014  3135     bsteffen    Initial creation
 * Jun 24, 2014  3279     bclement    added error handling and item.clearAll() 
 *                                      to run methods to fix eternal 'Loading...' problem
 * Jul 07, 2014  3135     bsteffen    Remove child nodes when there are no results.
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarBrowserQueryJob extends Job implements Runnable {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(RadarBrowserQueryJob.class);

    protected static final String JOB_DATA_KEY = "queryJob";

    protected TreeItem item;

    protected final AbstractProductBrowserDataDefinition<?> def;

    protected final String[] selection;

    protected List<ProductBrowserLabel> results;

    protected RadarBrowserQueryJob(TreeItem item) {
        super(extractName(item));
        this.item = item;
        /*
         * Fields must be pulled off the item on the UI thread.
         */
        this.def = RadarBrowserView.getDataDef(item);
        this.selection = RadarBrowserView.getProductURI(item, false);
        item.setData(JOB_DATA_KEY, this);
    }

    /**
     * Runnable method inherited from {@link Job} for running in the background.
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        IStatus rval = Status.OK_STATUS;
        try {
            results = def.populateData(selection);
            if (!item.isDisposed()) {
                item.getDisplay().syncExec(this);
            }
        } catch (Throwable e) {
            /* something has gone very wrong */
            log.error(e.getLocalizedMessage(), e);
            rval = Status.CANCEL_STATUS;
            /* clean up tree */
            if (!item.isDisposed()) {
                item.getDisplay().syncExec(new Runnable() {
                    @Override
                    public void run() {
                        for (TreeItem child : item.getItems()) {
                            child.dispose();
                        }
                    }
                });
            }
        }
        return rval;
    }

    /**
     * Runnable method inherited from {@link Runnable}. This will be run on the
     * UI thread.
     */
    @Override
    public void run() {
        if (item.isDisposed()) {
            return;
        }
        List<ProductBrowserLabel> results = this.results;
        if (results == null) {
            results = Collections.emptyList();
        }
        /*
         * If all children are disposed it can cause expand to disable so make
         * sure we re enable it.
         */
        boolean expanded = item.getExpanded();
        /*
         * Attempt to merge any existing nodes with the labels returned from the
         * query. If the order is the same then this should only perform simple
         * additions/removals. If the order has changed for some reason this
         * will copy some items to try to make the UI as consistent as possible.
         */
        for (int i = 0; i < results.size(); i += 1) {
            boolean create = true;
            ProductBrowserLabel label = results.get(i);
            while (i < item.getItemCount()) {
                TreeItem childItem = item.getItem(i);
                RadarBrowserLabel childLabel = RadarBrowserView
                        .getLabel(childItem);
                if (childLabel != null && childLabel.equals(label)) {
                    create = false;
                    break;
                } else if (results.contains(childLabel)) {
                    for (int j = i + 1; j < item.getItemCount(); j += 1) {
                        childItem = item.getItem(j);
                        childLabel = RadarBrowserView.getLabel(childItem);
                        if (childLabel.equals(label)) {
                            TreeItem child = new TreeItem(item, SWT.NONE, i);
                            copyItem(childItem, child);
                            childItem.dispose();
                            create = false;
                            break;
                        }
                    }
                    break;
                } else {
                    childItem.dispose();
                }
            }
            if (create) {
                TreeItem child = new TreeItem(item, SWT.NONE, i);
                child.setText(label.getName());
                child.setData(RadarBrowserView.LABEL_DATA_KEY, label);
                child.setData(RadarBrowserView.DEF_DATA_KEY, def);
                if (!label.isProduct()) {
                    TreeItem loading = new TreeItem(child, SWT.NONE);
                    loading.setText("Loading...");
                    loading.setGrayed(true);
                }
            }
        }
        if (!results.isEmpty()) {
            item.setExpanded(expanded);
        } else {
            for (TreeItem child : item.getItems()) {
                child.dispose();
            }
        }
        item.setData(JOB_DATA_KEY, null);
    }

    /**
     * Recursively copy an item. This is only needed if a query is repeated and
     * returns items in a new order.
     * 
     * @param oldItem
     * @param newItem
     */
    protected void copyItem(TreeItem oldItem, TreeItem newItem) {
        newItem.setText(oldItem.getText());
        for (TreeItem oldChild : oldItem.getItems()) {
            TreeItem newChild = new TreeItem(newItem, SWT.NONE);
            if (newItem.getItemCount() == 1) {
                /*
                 * For recursive expansion to work newItem must be expanded
                 * after newChild is created but before newChild is expanded.
                 */
                newItem.setExpanded(oldItem.getExpanded());
            }
            copyItem(oldChild, newChild);
        }
        newItem.setData(RadarBrowserView.LABEL_DATA_KEY,
                RadarBrowserView
                .getLabel(oldItem));
        newItem.setData(RadarBrowserView.DEF_DATA_KEY,
                RadarBrowserView.getDataDef(oldItem));
        RadarBrowserQueryJob job = (RadarBrowserQueryJob) oldItem
                .getData(JOB_DATA_KEY);
        if (job != null) {
            job.setItem(newItem);
            newItem.setData(JOB_DATA_KEY, job);
        }

    }

    protected void setItem(TreeItem item) {
        this.item = item;
    }

    protected static String extractName(TreeItem item) {
        StringBuilder name = new StringBuilder();
        if (item.getItemCount() == 1) {
            name.append("Querying");
        } else {
            name.append("Refreshing");
        }
        for (String label : RadarBrowserView.getProductURI(item, true)) {
            name.append(" ").append(label);
        }
        return name.toString();
    }

    /**
     * Start a new query job for the provided item only if there is not
     * currently a job running.
     * 
     * @param item
     *            a TreeItem from the {@link RadarBrowserView}
     */
    public static void startJob(TreeItem item) {
        if (item.getData(JOB_DATA_KEY) == null) {
            new RadarBrowserQueryJob(item).schedule();
        }
    }

}
