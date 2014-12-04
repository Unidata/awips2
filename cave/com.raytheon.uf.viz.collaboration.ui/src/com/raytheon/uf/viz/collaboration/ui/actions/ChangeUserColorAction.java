package com.raytheon.uf.viz.collaboration.ui.actions;

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
import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.ColorInfoMap.ColorInfo;
import com.raytheon.uf.viz.collaboration.ui.UserColorConfigManager;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Action to change the foreground/background chat color of a selected user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/02/14     3709        mapeters    Initial creation.
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public class ChangeUserColorAction extends Action {

    private int type;

    private String user;

    private RGB defaultForeground;

    private UserColorConfigManager colorConfigManager;

    public ChangeUserColorAction(int type, String user, boolean me,
            RGB defaultForeground, UserColorConfigManager colorConfigManager) {
        super("Change " + (me ? "Your " : (user + "'s "))
                + (type == SWT.FOREGROUND ? "Foreground" : "Background")
                + " Color...", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "change_color.gif"));
        this.type = type;
        this.user = user;
        this.defaultForeground = defaultForeground;
        this.colorConfigManager = colorConfigManager;
    }

    @Override
    public void run() {
        ColorDialog dialog = new ColorDialog(Display.getCurrent()
                .getActiveShell());
        ColorInfo colorInfo = colorConfigManager.getColor(user);
        if (colorInfo != null) {
            dialog.setRGB(colorInfo.getColor(type));
        } else if (type == SWT.FOREGROUND) {
            /*
             * set the dialog to display default foreground color as
             * currently selected
             */
            dialog.setRGB(defaultForeground);
        }
        RGB rgb = dialog.open();
        if (rgb != null) {
            colorConfigManager.setColor(user, type, rgb,
                    defaultForeground);
        }
    }
}