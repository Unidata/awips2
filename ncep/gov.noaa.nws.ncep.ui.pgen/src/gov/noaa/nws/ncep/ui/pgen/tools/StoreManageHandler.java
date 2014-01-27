/*
 * gov.noaa.nws.ncep.ui.pgen.controls.StoreManageHandler
 * 
 * 27 March 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.controls.StoreActivityDialog;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Define a handler for PGEN product/activity store to EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/13		#977		S. Gilbert	modified from PgenFileManageHandler.
 * 11/13        #1077       J. Wu       Pop up dialog for "Save All".
 * 
 * </pre>
 * 
 * @author S. Gilbert
 * @version 0.0.1
 */
public class StoreManageHandler extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        String btnClicked = (String) event.getApplicationContext();

        // Set "active" icon for the palette button corresponding to this tool
        String btnName = event.getParameter("name");
        PgenSession.getInstance().getPgenPalette().setActiveIcon(btnName);

        PgenResource rsc = PgenSession.getInstance().getPgenResource();
        String curFile = rsc.getActiveProduct().getOutputFile();

        if (curFile != null && btnClicked.equalsIgnoreCase("Save")) {
            rsc.storeCurrentProduct(curFile);
        } else if (btnClicked.equalsIgnoreCase("Save All")) {

            // Save the current one first
            Product curPrd = rsc.getActiveProduct();
            if (curFile != null) {
                rsc.storeCurrentProduct(curFile);
            } else {
                storeActivity(btnClicked);
            }

            // Save the rest
            for (Product pp : rsc.getProducts()) {
                if (pp == curPrd)
                    continue;

                rsc.setActiveProduct(pp);
                if (pp.getOutputFile() != null) {
                    rsc.storeCurrentProduct(pp.getOutputFile());
                } else {
                    storeActivity(btnClicked);
                }

                rsc.setActiveProduct(curPrd);
            }
        } else { // "Save As"
            storeActivity(btnClicked);
        }

        // Reset the original icon for the palette button corresponding to this
        // tool
        if (PgenSession.getInstance().getPgenPalette() != null) {
            PgenSession.getInstance().getPgenPalette().resetIcon(btnName);
        }

        // Set to "selecting" mode.
        PgenUtil.setSelectingMode();

        return null;
    }

    /*
     * Pops up the Save/Store dialog to save an activity.
     * 
     * @param btnClicked
     */
    private void storeActivity(String btnClicked) {

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        StoreActivityDialog storeDialog = null;

        if (storeDialog == null) {
            try {
                storeDialog = new StoreActivityDialog(shell, btnClicked);
                storeDialog.setBlockOnOpen(true);
            } catch (VizException e) {
                e.printStackTrace();
            }
        }

        if (storeDialog != null)
            storeDialog.open();
    }

}