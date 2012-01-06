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
package com.raytheon.uf.viz.alertviz.localization;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.viz.alertviz.localization.actions.AlertVizFileImportAction;
import com.raytheon.uf.viz.localization.adapter.LocalizationPerspectiveAdapter;
import com.raytheon.uf.viz.localization.filetreeview.FileTreeEntryData;

/**
 * Localization perspective adapter for AlertViz.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 04, 2011  5853       bgonzale    Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class AlertVizLocalizationAdapter extends LocalizationPerspectiveAdapter {

    private static final Map<String, LocalizationLevel> localizationMap = getLocalizationMap();

    @Override
    public boolean addContextMenuItems(IMenuManager menuMgr,
            FileTreeEntryData[] selectedData) {
        if (selectedData.length == 1) {
            FileTreeEntryData selected = selectedData[0];

            if (selected.getClass() == FileTreeEntryData.class) {
                LocalizationLevel level = localizationMap.get(selected
                        .getName());
                menuMgr.add(new AlertVizFileImportAction(
                        (FileTreeEntryData) selected, level));
                return true;
            }
        }
        return false;
    }

    private static Map<String, LocalizationLevel> getLocalizationMap() {
        Map<String, LocalizationLevel> map = new HashMap<String, LocalizationLevel>();
        map.put("Audio", LocalizationLevel.SITE);
        map.put("Configurations", LocalizationLevel.USER);
        map.put("Scripts", LocalizationLevel.SITE);
        map.put("Python", LocalizationLevel.SITE);
        return map;
    }
}
