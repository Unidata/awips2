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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.actions.ChatDisplayChangeEvent.ChangeType;
import com.raytheon.uf.viz.core.preferences.PreferenceConverter;

/**
 * Open change foreground color dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Oct 14, 2014 3709        mapeters    Initial creation.
 * 
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */

public class ChangeForegroundColorAction extends Action {

    public ChangeForegroundColorAction() {
        super("Change Foreground Color...");
    }

    @Override
    public void run() {
        ColorDialog dialog = new ColorDialog(Display.getCurrent()
                .getActiveShell());
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        RGB data = PreferenceConverter.getRGB(store, "fg", "black");
        dialog.setRGB(data);
        RGB postData = dialog.open();
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (postData != null && connection != null) {
            PreferenceConverter.setValue(store, "fg", postData);
            connection.postEvent(ChatDisplayChangeEvent.createColorEvent(
                    ChangeType.FOREGROUND, postData));
        }
    };
}