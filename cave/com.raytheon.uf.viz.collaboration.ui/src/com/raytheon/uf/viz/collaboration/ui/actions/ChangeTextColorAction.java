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
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.ui.AbstractColorConfigManager;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.ColorInfoMap.ColorInfo;
import com.raytheon.uf.viz.collaboration.ui.FeedColorConfigManager;
import com.raytheon.uf.viz.collaboration.ui.ForegroundBackgroundColorDlg;
import com.raytheon.uf.viz.collaboration.ui.UserColorConfigManager;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action to change the foreground and background chat colors of a selected
 * user/site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/02/14     3709        mapeters    Initial creation.
 * 12/09/14     3709        mapeters    Uses {@link ForegroundBackgroundColorDlg}, renamed from 
 *                                      ChangeUserColorAction, support both user and site colors.
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public class ChangeTextColorAction extends Action {

    private final String key;

    private RGB defaultForeground;

    private AbstractColorConfigManager colorConfigManager;

    /**
     * Constructor for changing user colors.
     * 
     * @param user
     * @param me
     * @param displayName
     * @param defaultForeground
     * @param colorConfigManager
     */
    public ChangeTextColorAction(String user, boolean me, boolean displayName,
            RGB defaultForeground, UserColorConfigManager colorConfigManager) {
        this("Change " + (displayName ? (me ? "Your" : (user + "'s")) : "User")
                + " Text Colors...", user, defaultForeground,
                colorConfigManager);
    }

    /**
     * Constructor for changing site colors.
     * 
     * @param site
     * @param defaultForeground
     * @param colorConfigManager
     */
    public ChangeTextColorAction(String site, RGB defaultForeground,
            FeedColorConfigManager colorConfigManager) {
        this("Change Site Text Colors...", site, defaultForeground,
                colorConfigManager);
    }

    private ChangeTextColorAction(String text, String key,
            RGB defaultForeground, AbstractColorConfigManager colorConfigManager) {
        super(text, IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "change_color.gif"));
        this.key = key;
        this.defaultForeground = defaultForeground;
        this.colorConfigManager = colorConfigManager;
    }

    @Override
    public void run() {
        ColorInfo colorInfo = colorConfigManager.getColor(key);
        RGB background;
        RGB foreground;
        if (colorInfo != null) {
            background = colorInfo.getColor(SWT.BACKGROUND);
            foreground = colorInfo.getColor(SWT.FOREGROUND);
        } else {
            /*
             * Set dialog to display default colors (if defaultForeground is
             * null, ForegroundBackgroundColorDlg uses blue)
             */
            background = new RGB(255, 255, 255);
            foreground = defaultForeground;
        }

        ForegroundBackgroundColorDlg dialog = new ForegroundBackgroundColorDlg(
                Display.getCurrent().getActiveShell(), foreground, background);

        dialog.setCloseCallback(new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (returnValue == null) {
                    return;
                }

                if (returnValue instanceof RGB[]) {
                    RGB[] colors = (RGB[]) returnValue;
                    colorConfigManager.setColors(key, colors[0], colors[1]);
                }
            }
        });
        dialog.open();
    }
}