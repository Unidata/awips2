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
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.display.data.IColorManager;
import com.raytheon.uf.viz.collaboration.display.data.UserColorInfo;
import com.raytheon.uf.viz.collaboration.ui.colors.ForegroundBackgroundColorDlg;
import com.raytheon.uf.viz.collaboration.ui.colors.ForegroundColorDlg;
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
 * Dec 02, 2014 3709        mapeters    Initial creation.
 * Dec 09, 2014  3709       mapeters    Uses {@link ForegroundBackgroundColorDlg}, renamed from 
 *                                      ChangeUserColorAction, support both user and site colors.
 * Dec 12, 2014 3709        mapeters    Use static methods to call constructor, icon displays 
 *                                      current foreground and background colors.
 * Jan 05, 2015 3709        mapeters    Added getTextColors(), added me param to createChangeUserTextColorAction().
 * Jan 09, 2015 3709        bclement    color change manager API changes
 * Jan 12, 2015 3709        bclement    removed event handler for icon changes, added ChangeTextColorCallback
 * Jan 13, 2015 3709        bclement    unified color management, simplified construction, added foregroundOnly mode
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public class ChangeTextColorAction<T extends IUser> extends Action {

    private final T user;

    private IColorManager<T> colorManager;

    private boolean foregroundOnly;

    private Image icon;

    /**
     * Callback that receives new color information when the action results in a
     * new color selection for a user
     */
    public static interface ChangeTextColorCallback {
        public void newColor(IUser user, UserColorInfo colors);
    }

    private ChangeTextColorCallback actionCallback = null;

    /**
     * Generate display text string according to provided options
     * 
     * @param user
     * @param me
     *            true if user represents the currently logged in user
     * @param displayName
     *            true if the user's name should be used or false if a generic
     *            display text should be used
     * @return
     */
    public static String getDisplayText(IUser user, boolean me,
            boolean displayName) {
        String name = user.getName();
        String text = "Change ";
        if (displayName) {
            text += me ? "Your" : (name + "'s");
        } else {
            text += "User";
        }
        text += " Text Colors...";

        return text;
    }

    /**
     * @param user
     * @param me
     *            true if user represents the currently logged in user
     * @param displayName
     *            true if the user's name should be used or false if a generic
     *            display text should be used
     * @param foregroundOnly
     *            true if only the option to change the foreground should be
     *            provided
     * @param colorConfigManager
     */
    public ChangeTextColorAction(T user, boolean me, boolean displayName,
            boolean foregroundOnly, IColorManager<T> colorConfigManager) {
        super(getDisplayText(user, me, displayName));
        this.user = user;
        this.colorManager = colorConfigManager;
        this.foregroundOnly = foregroundOnly;
        UserColorInfo colors = getTextColors();
        setIconColors(colors);
    }

    @Override
    public void run() {
        UserColorInfo colors = getTextColors();
        ForegroundColorDlg dialog;
        Shell shell = Display.getCurrent().getActiveShell();
        String desc = colorManager.getDescription(user);
        if (foregroundOnly) {
            dialog = new ForegroundColorDlg(shell, desc, colors.getForeground());
        } else {
            dialog = new ForegroundBackgroundColorDlg(shell, desc,
                    colors.getForeground(), colors.getBackground());
        }

        dialog.setCloseCallback(new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (returnValue == null) {
                    return;
                }

                if (returnValue instanceof UserColorInfo) {
                    UserColorInfo colors = (UserColorInfo) returnValue;
                    colorManager.setColorForUser(user, colors);
                    setIconColors(colors);
                    if (actionCallback != null) {
                        actionCallback.newColor(user, colors);
                    }
                }
            }
        });
        dialog.open();
    }

    /**
     * Get the stored colors (or default colors) of this action's user
     * 
     * @return color selected by color manager or default color if none found
     */
    private UserColorInfo getTextColors() {
        return colorManager.getColorForUser(user);
    }

    /**
     * Change colors in menu icon which represents the user's text color
     * settings
     * 
     * @param colors
     */
    private void setIconColors(UserColorInfo colors) {
        Device device = Display.getCurrent();
        Color fg = new Color(device, colors.getForeground());
        Color bg = new Color(device, colors.getBackground());

        Image oldIcon = icon;
        icon = new Image(device, 15, 15);
        Rectangle bounds = icon.getBounds();

        GC gc = new GC(icon);
        gc.setForeground(fg);
        gc.setBackground(bg);
        gc.fillRectangle(bounds);
        gc.drawText("A", 4, 0);

        setImageDescriptor(ImageDescriptor.createFromImage(icon));

        gc.dispose();
        fg.dispose();
        bg.dispose();
        if (oldIcon != null) {
            oldIcon.dispose();
        }
    }

    public void dispose() {
        if (icon != null) {
            icon.dispose();
        }
    }

    /**
     * @param actionCallback
     *            the actionCallback to set
     */
    public void setActionCallback(ChangeTextColorCallback actionCallback) {
        this.actionCallback = actionCallback;
    }

}
