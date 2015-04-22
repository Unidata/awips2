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

import gov.noaa.nws.ncep.viz.rtkp.KpPlotCapability;
import gov.noaa.nws.ncep.viz.rtkp.controls.EditKpPlotAttributesDialog;

import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * 09-03-2014    R4078       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class EditKpPlotAttributesAction extends AbstractRightClickAction {

    private static String title = "Edit Kp Attributes...";

    public EditKpPlotAttributesAction() {
        super(title);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {

        EditKpPlotAttributesDialog pd = new EditKpPlotAttributesDialog(
                VizWorkbenchManager.getInstance().getCurrentWindow().getShell(),
                title, getTopMostSelectedResource().getCapability(
                        KpPlotCapability.class));

        KpPlotCapability result = pd.open();

        if (result != null) {
            getTopMostSelectedResource().getCapability(KpPlotCapability.class)
                    .setPlotColor(result.getPlotColor());
            getTopMostSelectedResource().getCapability(KpPlotCapability.class)
                    .setTextFont(result.getTextFont());
            getTopMostSelectedResource().getCapability(KpPlotCapability.class)
                    .setTextSize(result.getTextSize());
            getTopMostSelectedResource().getCapability(KpPlotCapability.class)
                    .setTextStyle(result.getTextStyle());
            getTopMostSelectedResource().getCapability(KpPlotCapability.class)
                    .setTextColor(result.getTextColor());
            getTopMostSelectedResource().getCapability(KpPlotCapability.class)
                    .setPointStyle(result.getPointStyle());
            getTopMostSelectedResource().getCapability(KpPlotCapability.class)
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
