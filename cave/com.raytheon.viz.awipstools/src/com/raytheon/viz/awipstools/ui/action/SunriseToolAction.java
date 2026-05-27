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

package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.viz.awipstools.ui.dialog.SunriseToolDialog;

/**
 * SunriseToolAction.java
 *
 * Action class called when using sunrise/sunset tools from too menu.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 08/15/07     426         Eric Babin    Initial Creation.
 * Nov 27, 2018 7633        tgurney     Remove try/catch (no longer needed)
 * </pre>
 *
 * @author Eric Babin
 */
public class SunriseToolAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent e) throws ExecutionException {
        SunriseToolDialog id = null;
        id = new SunriseToolDialog(HandlerUtil.getActiveShellChecked(e),
                "Sunrise/Sunset");
        id.open();
        return null;
    }

}
