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
package com.raytheon.viz.gfe.ui;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Provides support for handling the Access Levels (BASE, SITE, USER) of files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2010     #4727  randerso    Initial creation
 * Sep 15, 2014     #3592  randerso    Fix logic to not include USER level.
 *                                     Code cleanup.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class AccessMgr {

    public static boolean verifyDelete(String itemName, LocalizationType type,
            boolean continueOnly) {
        // Verify that the user wants to delete the given item
        // If continueOnly is 1, warnings will be shown only if the
        // item will continue to appear after deletion (e.g. exists at
        // BASE or SITE level). altName is the user interface name to display.

        String altName = new File(itemName).getName();
        int ext = altName.lastIndexOf('.');
        if (ext > 0) {
            altName = altName.substring(0, ext);
        }

        IPathManager pm = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> tieredData = pm
                .getTieredLocalizationFile(type, itemName);
        tieredData.remove(LocalizationLevel.USER);

        List<LocalizationLevel> availableLevels = new ArrayList<LocalizationLevel>(
                Arrays.asList(pm.getAvailableLevels()));
        availableLevels.remove(LocalizationLevel.USER);

        boolean continueFlag = false;
        boolean answer = false;
        String message;

        if (tieredData.isEmpty()) {
            message = "There is no version of " + altName + " at the "
                    + levelString(availableLevels) + " levels.";
        } else {
            message = "Note that " + altName
                    + " will continue to appear in your "
                    + "GFESuite since a version of it exists " + "at the "
                    + levelString(tieredData.keySet()) + " level.";
            continueFlag = true;
        }
        message = altName + " will be deleted. \n\n" + message;

        if (!continueOnly || (continueOnly && continueFlag)) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            answer = MessageDialog.openConfirm(shell, "Item Delete", message);
        }
        return answer;
    }

    private static String levelString(Collection<LocalizationLevel> levels) {
        StringBuilder kind = new StringBuilder();

        int i = 1;
        for (LocalizationLevel level : levels) {
            if (level.equals(LocalizationLevel.USER)) {
                continue;
            }
            if (i == 1) {
                kind.append(level.toString());
            } else if (i == levels.size()) {
                kind.append(" and ").append(level.toString());
            } else {
                kind.append(", ").append(level.toString());
            }
            i++;
        }

        return kind.toString();
    }
}
