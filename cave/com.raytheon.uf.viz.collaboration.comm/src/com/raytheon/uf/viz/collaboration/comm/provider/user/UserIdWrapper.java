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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;

/**
 * provides access to alias information stored in localization
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class UserIdWrapper {
    @DynamicSerializeElement
    private UserId[] userIds;

    /**
     * @param userIds
     *            the userIds to set
     */
    public void setUserIds(UserId[] userIds) {
        this.userIds = userIds;
    }

    /**
     * @return the userIds
     */
    public UserId[] getUserIds() {
        return userIds;
    }

    public static Map<String, String> readAliasMap() {
        Map<String, String> result = new HashMap<String, String>();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = PathManagerFactory.getPathManager()
                .getLocalizationFile(
                        context,
                        "collaboration" + File.separator
                                + "collaborationAliases.xml");
        if (file.exists()) {
            UserIdWrapper ids = (UserIdWrapper) JAXB.unmarshal(file.getFile(),
                    UserIdWrapper.class);
            if (ids.getUserIds() != null) {
                for (UserId id : ids.getUserIds()) {
                    result.put(id.getName() + "@" + id.getHost(), id.getAlias());
                }
            }
        }
        return result;
    }

    public static void saveAliasMap(Map<String, String> localAliases)
            throws LocalizationOpFailedException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pm.getLocalizationFile(context, "collaboration"
                + File.separator + "collaborationAliases.xml");
        Set<UserId> ids = new HashSet<UserId>();
        for (Entry<String, String> entry : localAliases.entrySet()) {
            ids.add(new UserId(Tools.parseName(entry.getKey()), Tools
                    .parseHost(entry.getKey()), null, entry.getValue()));
        }
        UserIdWrapper wrapper = new UserIdWrapper();
        wrapper.setUserIds(ids.toArray(new UserId[0]));
        JAXB.marshal(wrapper, file.getFile());
        file.save();

    }
}
