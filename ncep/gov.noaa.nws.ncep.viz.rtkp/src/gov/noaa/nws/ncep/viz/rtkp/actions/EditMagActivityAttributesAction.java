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

import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerTextSize;
import gov.noaa.nws.ncep.viz.rtkp.MagActivityCapability;
import gov.noaa.nws.ncep.viz.rtkp.controls.EditMagActivityAttributesDialog;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * June 3, 2014  1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class EditMagActivityAttributesAction extends AbstractRightClickAction {

    private static String title = "Edit Mag Activity Attributes...";

    public EditMagActivityAttributesAction() {
        super(title);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {

        RGB[] colors = getTopMostSelectedResource().getCapability(
                MagActivityCapability.class).getColors();
        RGB[] textColors = getTopMostSelectedResource().getCapability(
                MagActivityCapability.class).getkIndicesTextColors();
        Float markerSize = getTopMostSelectedResource().getCapability(
                MagActivityCapability.class).getMarkerSize();
        RGB nonNetwrkStnColor = getTopMostSelectedResource().getCapability(
                MagActivityCapability.class).getNonNetwrkStnColor();
        // Integer markerWidth = getTopMostSelectedResource().getCapability(
        // MagActivityCapability.class).getMarkerWidth();
        MarkerTextSize markerTextSize = getTopMostSelectedResource()
                .getCapability(MagActivityCapability.class).getMarkerTextSize();
        EditMagActivityAttributesDialog pd = new EditMagActivityAttributesDialog(
                VizWorkbenchManager.getInstance().getCurrentWindow().getShell(),
                title, colors, textColors, nonNetwrkStnColor, markerSize,
                markerTextSize);

        MagActivityCapability result = pd.open();

        if (result != null) {

            getTopMostSelectedResource().getCapability(
                    MagActivityCapability.class).setColors(result.getColors());
            getTopMostSelectedResource().getCapability(
                    MagActivityCapability.class).setkIndicesTextColors(
                    result.getkIndicesTextColors());
            getTopMostSelectedResource().getCapability(
                    MagActivityCapability.class).setNonNetwrkStnColor(
                    result.getNonNetwrkStnColor());
            getTopMostSelectedResource().getCapability(
                    MagActivityCapability.class).setMarkerSize(
                    result.getMarkerSize());
            // getTopMostSelectedResource().getCapability(
            // MagActivityCapability.class).setMarkerWidth(
            // result.getMarkerWidth());
            getTopMostSelectedResource().getCapability(
                    MagActivityCapability.class).setMarkerTextSize(
                    result.getMarkerTextSize());
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
