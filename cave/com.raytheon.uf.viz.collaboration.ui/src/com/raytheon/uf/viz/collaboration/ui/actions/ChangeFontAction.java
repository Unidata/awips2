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
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FontDialog;

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Open change font dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ChangeFontAction extends Action {

    public ChangeFontAction() {
        super("Change Font...", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "font.gif"));
    }

    @Override
    public void run() {
        FontDialog dialog = new FontDialog(Display.getCurrent()
                .getActiveShell());
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        FontData data = PreferenceConverter.getFontData(store, "font");
        dialog.setFontList(new FontData[] { data });
        FontData postData = dialog.open();
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (postData != null && connection != null) {
            PreferenceConverter.setValue(store, "font", postData);
            connection.postEvent(postData);
        }
    };
}
