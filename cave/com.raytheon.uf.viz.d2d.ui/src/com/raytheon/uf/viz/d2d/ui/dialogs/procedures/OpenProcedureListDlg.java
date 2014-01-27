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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * This dialog will allow users to open existing procedures.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2011            mschenke     Initial creation
 * 11 Dec 2013  #2583      lvenable    Added a check to determine if current user,
 *                                     all users or all procedures should be requested.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class OpenProcedureListDlg extends ProcedureListDlg {

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public OpenProcedureListDlg(Shell parent) {
        super("Open Procedure", parent, Mode.OPEN);
    }

    @Override
    protected ProcedureTree populateDataList() {

        ProcedureTree root = new ProcedureTree("root", null);
        IPathManager pm = PathManagerFactory.getPathManager();
        Set<LocalizationContext> searchContexts = new HashSet<LocalizationContext>();

        searchContexts.addAll(Arrays.asList(pm
                .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC)));

        // If the show type is not the current user then look at loading the
        // remaining procedures.
        if (selectedShowType != ShowType.MINE) {
            // Use of LocalizationLevels.values() in this case should be okay
            // since we are requesting all possible context names for the level,
            // doesn't matter if our local context for the level is set
            LocalizationLevel[] levels = pm.getAvailableLevels();
            for (LocalizationLevel level : levels) {

                // If the show type is all users and the localization is USER or
                // show type is ALL then load the procedure. This will either
                // load all of the procedures or only the ones at the user
                // level.
                if ((selectedShowType == ShowType.ALL_USERS && level == LocalizationLevel.USER)
                        || selectedShowType == ShowType.ALL) {

                    if (level.isSystemLevel() == false) {
                        String[] available = pm.getContextList(level);
                        for (String s : available) {
                            LocalizationContext ctx = pm.getContext(
                                    LocalizationType.CAVE_STATIC, level);
                            ctx.setContextName(s);
                            searchContexts.add(ctx);
                        }
                    }
                }
            }
        }

        LocalizationFile[] files = pm.listFiles(searchContexts
                .toArray(new LocalizationContext[searchContexts.size()]),
                ProcedureDlg.PROCEDURES_DIR, new String[] { "xml" }, false,
                true);

        Map<String, ProcedureTree> locLevels = new HashMap<String, ProcedureTree>();

        for (int i = 0; i < files.length; i++) {
            String str = LocalizationUtil.extractName(files[i].getName());
            String level = "";

            LocalizationFile file = files[i];
            if (file.getContext().getLocalizationLevel().isSystemLevel() == false) {

                // place under the localization level
                level = String.format("%s - %s", file.getContext()
                        .getLocalizationLevel().name(), file.getContext()
                        .getContextName());
            } else {
                // place under the system localization level
                level = "SYSTEM";
            }

            if (!locLevels.containsKey(level)) {
                locLevels.put(level, root.addChild(level, null));
            }

            // place file in the appropriate node
            locLevels.get(level).addChild(str, file);
        }

        this.oneLevel = false;
        return root;
    }

}
