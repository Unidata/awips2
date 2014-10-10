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

import gov.noaa.nws.ncep.viz.rtkp.KsPlotCapability;
import gov.noaa.nws.ncep.viz.rtkp.controls.EditKsPlotAttributesDialog;

import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * June 2, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class EditKsPlotAttributesAction extends AbstractRightClickAction {

    private static String title = "Edit Ks Attributes...";

    public EditKsPlotAttributesAction() {
        super(title);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {

        EditKsPlotAttributesDialog pd = new EditKsPlotAttributesDialog(
                VizWorkbenchManager.getInstance().getCurrentWindow().getShell(),
                title, getTopMostSelectedResource().getCapability(
                        KsPlotCapability.class));

        KsPlotCapability result = pd.open();

        if (result != null) {
            getTopMostSelectedResource().getCapability(KsPlotCapability.class)
                    .setPlotColors(result.getPlotColors());
            getTopMostSelectedResource().getCapability(KsPlotCapability.class)
                    .setTextFont(result.getTextFont());
            getTopMostSelectedResource().getCapability(KsPlotCapability.class)
                    .setTextSize(result.getTextSize());
            getTopMostSelectedResource().getCapability(KsPlotCapability.class)
                    .setTextStyle(result.getTextStyle());
            getTopMostSelectedResource().getCapability(KsPlotCapability.class)
                    .setTextColor(result.getTextColor());
            getTopMostSelectedResource().getCapability(KsPlotCapability.class)
                    .setPointStyle(result.getPointStyle());
            getTopMostSelectedResource().getCapability(KsPlotCapability.class)
                    .setPointSize(result.getPointSize());
        }

        getContainer().refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return title;
    }

}
