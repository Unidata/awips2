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

import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.display.data.IColorManager;
import com.raytheon.uf.viz.collaboration.display.data.UserColorInfo;

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
 * Nov 26, 2014 3709       mapeters    Abstracted out code to {@link PersistentColorConfigStorage}.
 * Dec 08, 2014 3709       mapeters    Set foreground and background colors together.
 * Jan 09, 2015 3709       bclement    made into a true singleton, moved colorInfoMap to super
 * Jan 13, 2015 3709       bclement    refactored to use PersistentColorConfigStorage utility
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public class UserColorConfigManager implements IColorManager<IUser> {

    private static final String FILE_PATH = PersistentColorConfigStorage.CONFIG_DIR_NAME
            + IPathManager.SEPARATOR + "userColorInfo.xml";

    /* dark blue */
    private static final UserColorInfo DEFAULT_USER_COLORS = new UserColorInfo(
            new RGB(0, 0, 191));

    /* red */
    private static final UserColorInfo DEFAULT_PEER_COLORS = new UserColorInfo(
            new RGB(255, 0, 0));

    private static UserColorConfigManager instance;

    public static synchronized UserColorConfigManager getInstance() {
        if (instance == null) {
            instance = new UserColorConfigManager();
        }
        return instance;
    }

    private final PersistentColorConfigStorage<IUser> storage = new PersistentColorConfigStorage<IUser>() {
        @Override
        protected IUser convert(String persisted) {
            return IDConverter.convertFrom(persisted);
        }
    };

    private Map<IUser, UserColorInfo> _colors;

    protected UserColorConfigManager() {
    }

    @Override
    public String getDescription(IUser user) {
        return "Color changes will apply to one-on-one chat sessions with user "
                + user.getName() + ".";
    }

    @Override
    public UserColorInfo getColorForUser(IUser user) {
        Map<IUser, UserColorInfo> colorMap = getColorMap();
        UserColorInfo rval = colorMap.get(user);
        if (rval == null) {
            CollaborationConnection conn = CollaborationConnection
                    .getConnection();
            if (conn.getUser().isSameUser(user)) {
                rval = DEFAULT_USER_COLORS;
            } else {
                rval = DEFAULT_PEER_COLORS;
            }
        }
        return rval;
    }

    /**
     * Get color mappings, goes to storage if not initialized
     * 
     * @return
     */
    private Map<IUser, UserColorInfo> getColorMap() {
        synchronized (storage) {
            if (_colors == null) {
                _colors = storage.getColors(FILE_PATH);
            }
        }
        return _colors;
    }

    @Override
    public void setColorForUser(IUser user, UserColorInfo color) {
        synchronized (storage) {
            Map<IUser, UserColorInfo> colorMap = getColorMap();
            colorMap.put(user, color);
            storage.persistColors(colorMap, FILE_PATH);
        }
    }

    @Override
    public void clearColors() {
        synchronized (storage) {
            Map<IUser, UserColorInfo> colorMap = getColorMap();
            colorMap.clear();
            storage.persistColors(colorMap, FILE_PATH);
        }
    }

}
