package com.raytheon.uf.viz.personalities.cave.menu;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;

/**
 * An exit command handler that does not rely on a Workbench window having
 * the focus.  This allows it to be used from a tear-off menu.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2013-07-11   DR 15727   D. Friedman Initial creation
 * 
 * </pre>
 * 
 */

public class ExitHandler extends AbstractHandler {

    public Object execute(ExecutionEvent event) throws ExecutionException {
        PlatformUI.getWorkbench().close();
        return null;
    }

}
