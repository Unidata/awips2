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

package com.raytheon.viz.ui.tools.map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Dummy action
 * 
 * FIXME Delete me and my whole plugin!
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/1/06                   chammack    Initial Creation.
 * Sep 02, 2015  4749       njensen     Deprecated
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@Deprecated
public class NullTool extends AbstractTool {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        // do nothing
        return null;
    }

}
