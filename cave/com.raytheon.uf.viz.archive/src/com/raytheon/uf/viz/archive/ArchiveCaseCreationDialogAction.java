package com.raytheon.uf.viz.archive;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

/**
 * Action to bring up the Archive Case Creation dialog..
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2013 1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ArchiveCaseCreationDialogAction extends AbstractHandler {
    // ArchiveCaseCreationDialog dialog;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        // if (dialog == null || dialog.isDiposed()) {
        // Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
        // .getShell();
        // dialog = new ArchiveCaseCreationDialog(shell);
        // dialog.open();
        // } else {
        // dialog.bringToTop();
        // }
        // dialog.open();
        System.out.println("ArchiveCaseCreationDialogAction NYI.");
        return null;
    }

}
