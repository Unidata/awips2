/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.viz.mpe.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * RFCs QPE collaboration action
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * MAY 02, 2017 17911      wkwock      Initial creation.
 * 
 * @author wkwock
 * @version 1.0
 */

public class RFCsQPECollaborationAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        MPEDisplayManager dm = MPEDisplayManager.getCurrent();
        dm.loadRFCQPEData();

        return null;
    }
}
