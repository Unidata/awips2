/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.actions;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.rtkp.GeoMagRTKpDescriptor;
import gov.noaa.nws.ncep.viz.rtkp.rsc.GeoMagRTKpResourceData;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Handler to display Real-time Kp Monitor in national centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2014  1122       sgurung     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 * 
 */
public class GeoMagRTKpAction extends AbstractHandler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagRTKpAction.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        File bundleFile = NcPathManager.getInstance().getStaticFile(
                "ncep/Bundles/GeoMagRTKpMonitorPlots.xml");

        Map<String, String> vars = new HashMap<String, String>();
        DataTime startTime = null;
        DataTime endTime = null;

        Bundle b;
        try {
            b = Bundle.unmarshalBundle(bundleFile, vars);

            IRenderableDisplay renderableDisplay = b.getDisplays()[0];
            IDescriptor bundleDescriptor = renderableDisplay.getDescriptor();
            if (bundleDescriptor instanceof GeoMagRTKpDescriptor) {
                GeoMagRTKpDescriptor geo = (GeoMagRTKpDescriptor) bundleDescriptor;
                for (ResourcePair pair : geo.getSerializableResources()) {
                    if (pair.getResourceData() instanceof GeoMagRTKpResourceData) {
                        GeoMagRTKpResourceData rscData = (GeoMagRTKpResourceData) pair
                                .getResourceData();
                        rscData.setPlotLengthInHours(6);
                        rscData.setUpdating(true);
                        // dataTime[] availableTimes =
                        // rscData.getAvailableTimes();
                        startTime = new DataTime(
                                RTKpUtil.calcPrevSynPerStartTime());
                        rscData.setStartTime(startTime);
                        endTime = rscData.getEndTime();
                    }
                }
            }
            String bundleEditorId = DescriptorMap.getEditorId(bundleDescriptor
                    .getClass().getName());

            AbstractEditor editor = UiUtil.createOrOpenEditor(bundleEditorId,
                    renderableDisplay);
            BundleLoader.loadTo(editor, b);

            // show the "Data Block" and "Recent Kp Estimates Block" view/window
            RTKpUtil.showDataBlock(
                    RTKpUtil.calcDataBlockStartTime(RTKpUtil.calcCurTime()),
                    RTKpUtil.calcCurTime());
        } catch (VizException e) {

            statusHandler.handle(Priority.PROBLEM,
                    "Error loading RTKp Monitor", e);

        }

        return null;
    }

}
