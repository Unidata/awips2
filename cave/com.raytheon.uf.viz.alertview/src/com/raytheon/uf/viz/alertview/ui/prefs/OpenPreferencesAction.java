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
package com.raytheon.uf.viz.alertview.ui.prefs;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.ui.dialogs.PreferencesUtil;

/**
 * Action which opens {@link PreferenceDialog} with the
 * {@link AlertViewPreferencePage} and other associated {@link IPreferencePage}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 25, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class OpenPreferencesAction extends Action {

    public OpenPreferencesAction() {
        super("Preferences");
    }

    @Override
    public void run() {
        PreferencesUtil.createPreferenceDialogOn(
                null,
                AlertViewPreferencePage.class.getName(),
                new String[] { AlertViewPreferencePage.class.getName(),
                        StylePreferencePage.class.getName(),
                        PopupPreferencePage.class.getName() }, null).open();
    }

}
