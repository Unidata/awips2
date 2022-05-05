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
package com.raytheon.viz.gfe.localization.adapters;

import java.util.Collection;

import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.localization.perspective.adapter.LocalizationPerspectiveAdapter;
import com.raytheon.uf.viz.localization.perspective.view.FileTreeEntryData;
import com.raytheon.uf.viz.localization.perspective.view.LocalizationFileEntryData;
import com.raytheon.uf.viz.localization.perspective.view.LocalizationFileGroupData;
import com.raytheon.viz.gfe.localization.actions.ExtendAction;
import com.raytheon.viz.gfe.localization.actions.NewAction;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil;
import com.raytheon.viz.gfe.localization.util.SmartInitUtil;

/**
 * Localization Adapter for GFE Smart Inits
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 10, 2016  5816     randerso  Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 */
public class SmartInitAdapter extends LocalizationPerspectiveAdapter {

    @Override
    public boolean addContextMenuItems(IMenuManager menuMgr,
            FileTreeEntryData[] selectedData) {

        AbstractScriptUtil util = new SmartInitUtil();
        NewAction newAction = new NewAction(util);
        menuMgr.add(newAction);

        // TODO: refactor this block into higher level class to be used in both
        // ServerConfigAdapter and SmartInitAdapter
        LocalizationFile lf = null;
        if (selectedData.length == 1) {
            if (selectedData[0] instanceof LocalizationFileEntryData) {
                LocalizationFileEntryData data = (LocalizationFileEntryData) selectedData[0];
                lf = data.getFile();
            } else if (selectedData[0] instanceof LocalizationFileGroupData) {
                LocalizationFileGroupData data = (LocalizationFileGroupData) selectedData[0];
                Collection<LocalizationFileEntryData> children = data
                        .getChildrenData();
                if (children.size() == 1) {
                    lf = children.iterator().next().getFile();
                }
            }
        }

        if (lf != null) {
            ExtendAction action = new ExtendAction(lf.getPath());
            menuMgr.add(action);
        }
        return true;
    }

}
