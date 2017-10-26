package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import gov.noaa.gsd.viz.ensemble.control.IResourceRegisteredListener;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/***
 * 
 * Allows caller to be able to wait for all registered resources (see method
 * <code>resourceRegistered</code> to
 * 
 * TODO: Currently this is a simple work-around which will be replaced by a more
 * robust solution once we come up with the final 16.2.2 solution.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2016            polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class WaitForLoadingToComplete extends Job implements
        IResourceRegisteredListener {

    private List<AbstractVizResource<?, ?>> resources = null;

    private IMemoryUsageProvider memoryUsageProvider = null;

    public WaitForLoadingToComplete(IMemoryUsageProvider mup) {
        super("Wait for loading complete");
        memoryUsageProvider = mup;
        resources = new CopyOnWriteArrayList<>();
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {

        IStatus status = Status.OK_STATUS;
        while (!monitor.isCanceled()) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                /* ignore */
            }
            memoryUsageProvider.updateMemoryUsage();
        }
        return status;
    }

    @Override
    public void resourceRegistered(AbstractVizResource<?, ?> rsc) {
        resources.add(rsc);
    }

}
