package gov.noaa.gsd.viz.ensemble.action;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

import gov.noaa.gsd.viz.ensemble.display.control.contour.ContourControlDialog;

/**
 * 
 * This action is for popping up the Contour Control Dialog.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2017  19598       polster    Initial creation
 * Mar 31, 2017  19598       jing       Changed dialog creation and added log
 * Jun 27, 2017  19325       jing       Upgrade to 17.3.1
 *
 * </pre>
 *
 * @author polster
 * @author jing
 */
public class ContourControlSelectionAction extends AbstractRightClickAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContourControlSelectionAction.class);

    @Override
    public void run() {
        AbstractVizResource<?, ?> rsc = getTopMostSelectedResource();
        if (rsc instanceof AbstractGridResource
                && ((AbstractGridResource<?>) rsc)
                        .getDisplayType() == DisplayType.CONTOUR) {
            List<AbstractGridResource<?>> rscList = new ArrayList<AbstractGridResource<?>>();
            rscList.add((AbstractGridResource<?>) rsc);
            try {
                ContourControlDialog dialog = new ContourControlDialog(
                        VizWorkbenchManager.getInstance().getCurrentWindow()
                                .getShell(),
                        rsc.getName(), rscList);
                dialog.open();
            } catch (StyleException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

    }

    @Override
    public String getText() {
        return "Contour Control";
    }

}
