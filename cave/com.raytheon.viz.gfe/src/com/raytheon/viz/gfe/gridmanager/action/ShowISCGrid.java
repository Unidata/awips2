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
package com.raytheon.viz.gfe.gridmanager.action;

import com.raytheon.viz.gfe.smarttool.SmartUtil;

/**
 * Stores information right click menu action ShowISCGrid.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2008              dfitch      Initial creation.
 * 08/28/2009   2959       bphillip    Added Show_ISC_Grid implementation
 * 
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class ShowISCGrid extends AbstractGridManagerAction {

    public ShowISCGrid() {
        super("Show_ISC_Grid");
    }

    @Override
    public void run() {
        SmartUtil.runTool("Show_ISC_Grid");
    }
}
