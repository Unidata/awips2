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

import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * May 5, 2014   1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagRTKpRecentKpEstAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView("");

        try {

            if (vpart == null) {
                vpart = wpage.showView(RTKpUtil.RECENTKP_VIEW_ID);
            } else {
                if (!wpage.isPartVisible(vpart))
                    vpart = wpage.showView(RTKpUtil.RECENTKP_VIEW_ID);

            }
        } catch (Exception e) {

            e.printStackTrace();

        }

        return null;
    }

}
