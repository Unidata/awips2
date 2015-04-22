/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.ContoursAttrDlg;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;

//import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

/**
 * Implements Hotkeys for PGEN contours drawing tool.
 * 
 * ARROW_UP - move the contour value up a level. ARROW_DOWN - move the contour
 * value down a level.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/14        #1117       J. Wu       Initial creation
 * 
 * </pre>
 * 
 * @author J. Wu
 */
public class PgenContoursHotkeyHandler extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String actionStr = event.getParameter("action");
        if (actionStr == null || actionStr.isEmpty()) {
            return null;
        }

        ContoursAttrDlg cdlg = ContoursAttrDlg.getInstance(PlatformUI
                .getWorkbench().getActiveWorkbenchWindow().getShell());

        if (cdlg != null && cdlg.getShell() != null
                && !cdlg.getShell().isDisposed()) {
            cdlg.upDownLabelSelection(actionStr);
        }

        return null;
    }

}