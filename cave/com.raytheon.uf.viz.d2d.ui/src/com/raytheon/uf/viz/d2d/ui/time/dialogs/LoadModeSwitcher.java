/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.d2d.ui.time.dialogs;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.ui.actions.LoadModeHandler;

/**
 * Switches LoadMode and handles the display thread issues and exceptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class LoadModeSwitcher {
    public VizException exception;

    public void switchLoadMode(final LoadMode newMode) throws VizException {
        Display display = PlatformUI.getWorkbench().getDisplay();

        display.syncExec(new Runnable() {
            @Override
            public void run() {
                exception = null;
                LoadModeHandler handler = new LoadModeHandler();
                Map<String, String> params = new HashMap<String, String>();
                params.put("loadMode", newMode.toString());
                ExecutionEvent ee = new ExecutionEvent(null, params, null, null);
                try {
                    handler.execute(ee);
                } catch (ExecutionException e) {
                    exception = new VizException(e);
                }
            }
        });
        if (exception != null) {
            throw exception;
        }
    }
}
