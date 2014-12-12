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
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.AbstractColorConfigManager;
import com.raytheon.uf.viz.collaboration.ui.ColorInfoMap.ColorInfo;
import com.raytheon.uf.viz.collaboration.ui.FeedColorConfigManager;
import com.raytheon.uf.viz.collaboration.ui.ForegroundBackgroundColorDlg;
import com.raytheon.uf.viz.collaboration.ui.UserColorConfigManager;
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
 * 12/12/14     3709        mapeters    Use static methods to call constructor, icon displays 
 *                                      current foreground and background colors.
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

    private Image icon;

    /**
     * Create and return new action for changing user colors.
     * 
     * @param user
     * @param displayName
     * @param defaultForeground
     * @param colorConfigManager
     * @return
     */
    public static ChangeTextColorAction createChangeUserTextColorAction(
            String user, boolean displayName, RGB defaultForeground,
            UserColorConfigManager colorConfigManager) {
        String text = "Change ";
        if (displayName) {
            boolean me = CollaborationConnection.getConnection().getUser()
                    .getName().equals(user);
            text += me ? "Your" : (user + "'s");
        } else {
            text += "User";
        }
        text += " Text Colors...";

        return new ChangeTextColorAction(text, user, defaultForeground,
                colorConfigManager);
    }
    
    /**
     * Create and return new action for changing site colors.
     * 
     * @param site
     * @param defaultForeground
     * @param colorConfigManager
     * @return
     */
    public static ChangeTextColorAction createChangeSiteTextColorAction(
            String site, RGB defaultForeground,
            FeedColorConfigManager colorConfigManager) {
        return new ChangeTextColorAction("Change Site Text Colors...", site,
                defaultForeground, colorConfigManager);
    }

    private ChangeTextColorAction(String text, String key,
            RGB defaultForeground, AbstractColorConfigManager colorConfigManager) {
        super(text);
        this.key = key;
        this.defaultForeground = defaultForeground;
        this.colorConfigManager = colorConfigManager;

        ColorInfo colorInfo = colorConfigManager.getColor(key);
        RGB foreground;
        RGB background;
        if (colorInfo != null) {
            foreground = colorInfo.getColor(SWT.FOREGROUND);
            background = colorInfo.getColor(SWT.BACKGROUND);
        } else {
            foreground = defaultForeground;
            background = new RGB(255, 255, 255);
        }
        setIconColors(foreground, background);

        CollaborationConnection.getConnection().registerEventHandler(this);
    }

    @Override
    public void run() {
        ColorInfo colorInfo = colorConfigManager.getColor(key);
        RGB foreground;
        RGB background;
        if (colorInfo != null) {
            foreground = colorInfo.getColor(SWT.FOREGROUND);
            background = colorInfo.getColor(SWT.BACKGROUND);
        } else {
            /*
             * Set dialog to display default colors
             */
            foreground = defaultForeground;
            background = new RGB(255, 255, 255);
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
                    CollaborationConnection connection = CollaborationConnection
                            .getConnection();
                    connection.postEvent(new ChangeIconEvent(key, colors[0],
                            colors[1]));
                }
            }
        });
        dialog.open();
    }

    @Subscribe
    public void changeIcon(ChangeIconEvent event) {
        if (event.key.equals(this.key)) {
            setIconColors(event.foreground, event.background);
        }
    }

    private void setIconColors(RGB foreground, RGB background) {
        Device device = Display.getCurrent();
        Color fg = new Color(device, foreground);
        Color bg = new Color(device, background);

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

    private class ChangeIconEvent {

        private String key;

        private RGB foreground;

        private RGB background;

        private ChangeIconEvent(String key, RGB foreground, RGB background) {
            this.key = key;
            this.foreground = foreground;
            this.background = background;
        }
    }

    public void dispose() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection != null) {
            connection.unregisterEventHandler(this);
        }

        if (icon != null) {
            icon.dispose();
        }
    }
}