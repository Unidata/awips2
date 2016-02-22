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
package com.raytheon.viz.gfe.procedures;

import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.viz.localization.adapter.LocalizationPerspectiveAdapter;
import com.raytheon.uf.viz.localization.filetreeview.FileTreeEntryData;
import com.raytheon.viz.gfe.core.script.PythonFileTemplate;
import com.raytheon.viz.gfe.core.script.action.NewAction;
import com.raytheon.viz.gfe.procedures.util.ProcedureUtil;

/**
 * GFE Procedures Adapter file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ProceduresAdapter extends LocalizationPerspectiveAdapter {
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.localization.adapter.ILocalizationConfigAdapter#
     * addContextMenuItems(org.eclipse.jface.action.IMenuManager,
     * com.raytheon.uf.viz.localization.filetreeview.FileTreeEntryData[])
     */
    @Override
    public boolean addContextMenuItems(IMenuManager menuMgr,
            FileTreeEntryData[] selectedData) {
        PythonFileTemplate util = new ProcedureUtil();
        NewAction newAction = new NewAction(util);
        menuMgr.add(newAction);

        return true;
    }
}
