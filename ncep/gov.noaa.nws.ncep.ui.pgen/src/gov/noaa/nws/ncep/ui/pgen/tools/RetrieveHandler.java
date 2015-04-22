/*
 * gov.noaa.nws.ncep.ui.pgen.tools.RetrieveHandler
 * 
 * 11 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.controls.RetrieveActivityDialog;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Define a handler for PGEN Activity retrieve controls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/13		#977		S. Gilbert	Modified from PgenFileManageHandler
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 0.0.1
 */
public class RetrieveHandler extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        Shell shell = new Shell(SWT.SHELL_TRIM); /* RESIZE | SWT.PRIMARY_MODAL); */
        String btnClicked = (String) event.getApplicationContext();

        // Set "active" icon for the palette button corresponding to this tool
        String btnName = event.getParameter("name");
        PgenSession.getInstance().getPgenPalette().setActiveIcon(btnName);

        RetrieveActivityDialog retrieveDlg = null;

        try {
            retrieveDlg = new RetrieveActivityDialog(shell, btnClicked);
            retrieveDlg.setBlockOnOpen(true);
        } catch (VizException e) {
            e.printStackTrace();
        }

        if (retrieveDlg != null)
            retrieveDlg.open();

        // Reset the original icon for the palette button corresponding to this
        // tool
        if (PgenSession.getInstance().getPgenPalette() != null) {
            PgenSession.getInstance().getPgenPalette().resetIcon(btnName);
        }

        return null;
    }

}