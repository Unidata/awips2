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
package com.raytheon.uf.viz.collaboration.ui.colors;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.colors.ColorInfoMap.ColorInfo;

/**
 * Custom user coloring configuration manager for use where the user's true
 * identity is known (eg one-to-one chat)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2014 3709       mapeters    Initial creation.
 * Nov 26, 2014 3709       mapeters    Abstracted out code to {@link PersistentColorConfigManager}.
 * Dec 08, 2014 3709       mapeters    Set foreground and background colors together.
 * Jan 09, 2015 3709       bclement    made into a true singleton, moved colorInfoMap to super
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public class UserColorConfigManager extends PersistentColorConfigManager {

    private static final String FILE_PATH = CONFIG_DIR_NAME
            + IPathManager.SEPARATOR + "userColorInfo.xml";

    private static UserColorConfigManager instance;

    public static synchronized UserColorConfigManager getInstance() {
        if (instance == null) {
            instance = new UserColorConfigManager();
        }
        return instance;
    }

    protected UserColorConfigManager() {
    }

    /**
     * Set and store the given colors for the given user.
     * 
     * @param user
     * @param foreground
     * @param background
     */
    @Override
    public synchronized void setColors(String user, RGB foreground,
            RGB background) {
        super.setColors(user, foreground, background, FILE_PATH);
    }

    /**
     * Get the {@link ColorInfo} for the given user from memory.
     * 
     * @param user
     * @return
     */
    @Override
    public synchronized ColorInfo getColor(String user) {
        return super.getColor(user, FILE_PATH);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.colors.IColorConfigManager#
     * getDescription()
     */
    @Override
    public String getDescription(String key) {
        UserId id = IDConverter.convertFrom(key);
        return "Color changes will apply to one-on-one chat sessions with user "
                + id.getName() + ".";
    }

}
