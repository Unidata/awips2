package gov.noaa.nws.ncep.viz.rsc.timeseries.view;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

/**
 * 
 * Sampling readout Action for GeoMagResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 07/07/2014    R4079       qzhou     Initial Creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class SamplingAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        /*
         * The viewID string is in the XML file for extension point.
         */

        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(SamplingView.samplingId);

        try {

            if (vpart == null) {
                vpart = wpage.showView(SamplingView.samplingId);

            } else {
                if (!wpage.isPartVisible(vpart))
                    vpart = wpage.showView(SamplingView.samplingId);

            }
        } catch (Exception e) {

            e.printStackTrace();

        }

        return null;
    }

}
