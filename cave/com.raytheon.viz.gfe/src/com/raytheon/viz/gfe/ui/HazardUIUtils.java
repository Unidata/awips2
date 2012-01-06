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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class HazardUIUtils {
    public static boolean tempHazardsExist(DataManager dataManager) {
        Parm[] displayedParms = dataManager.getParmManager()
                .getDisplayedParms();
        for (Parm p : displayedParms) {
            if (p.getParmID().getParmName().startsWith("haz")) {
                return true;
            }
        }

        return false;
    }

    public static Parm hazardsWEModified(DataManager dataManager) {
        Parm[] modParms = dataManager.getParmManager().getModifiedParms();
        for (Parm p : modParms) {
            if (p.getParmID().getParmName().contains("Hazards")) {
                return p;
            }
        }

        return null;
    }

    // displays a warning about unsaved Hazards grids
    public static void displayWarningDialog(Composite master) {
        Group msgFrame = new Group(master, SWT.BORDER);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        msgFrame.setLayoutData(layoutData);
        GridLayout layout = new GridLayout(1, false);
        msgFrame.setLayout(layout);

        // make the warning label
        Label warningLabel = new Label(msgFrame, SWT.CENTER);
        warningLabel.setText("WARNING!!!!!!");
        warningLabel.setForeground(warningLabel.getDisplay().getSystemColor(
                SWT.COLOR_RED));
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        warningLabel.setLayoutData(layoutData);

        // make the message label
        Label myLabel = new Label(msgFrame, SWT.CENTER);
        myLabel.setText("Hazards WE cannot be saved.  Please Merge hazards first.");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        myLabel.setLayoutData(layoutData);
    }
}
