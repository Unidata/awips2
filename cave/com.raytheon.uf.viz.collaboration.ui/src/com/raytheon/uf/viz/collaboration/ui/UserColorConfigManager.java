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
package com.raytheon.uf.viz.collaboration.ui;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.swing.plaf.synth.ColorType;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.collaboration.ui.UserColorInformation.UserColor;

/**
 * User coloring configuration manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2014 3709       mapeters    Initial creation
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public class UserColorConfigManager {

    private static UserColorInformation colorInfo;

    private UserColorConfigManager() {
    }

    /**
     * Set and store the color type of the given user to be the given rgb. If
     * creating new {@link UserColor} and setting background, set foreground to
     * defaultForeground to prevent it from incorrectly defaulting.
     * 
     * @param user
     * @param type
     * @param rgb
     * @param defaultForeground
     */
    public synchronized static void setColorForUser(String user,
            ColorType type, RGB rgb, RGB defaultForeground) {
        if (colorInfo == null) {
            colorInfo = new UserColorInformation();
        }
        Map<String, UserColor> colors = colorInfo.getColors();
        if (colors == null) {
            colorInfo.setColors(new HashMap<String, UserColor>());
            colors = colorInfo.getColors();
        }

        UserColor userColor = colors.get(user);
        if (userColor != null) {
            userColor.setColor(type, rgb, null);
        } else {
            UserColor color = new UserColor();
            color.setColor(type, rgb, defaultForeground);
            colors.put(user, color);
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathMgr.getLocalizationFile(lContext,
                "collaboration" + IPathManager.SEPARATOR + "userColorInfo.xml");
        try {
            JAXBContext context = JAXBContext
                    .newInstance(UserColorInformation.class);
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            marshaller.marshal(colorInfo, file.getFile());
            file.save();
        } catch (Exception e) {
            Activator.statusHandler.error(
                    "Unable to write color information to file: "
                            + file.getName() + " in context " + lContext, e);
        }
    }

    /**
     * Get the {@link UserColor} for the given user from memory.
     * 
     * @param user
     * @return
     */
    public synchronized static UserColor getColorForUser(String user) {
        if(colorInfo == null) {
            IPathManager pm = (PathManager) PathManagerFactory.getPathManager();
            LocalizationContext locContext = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            LocalizationFile file = pm.getLocalizationFile(locContext,
                    "collaboration" + IPathManager.SEPARATOR
                            + "userColorInfo.xml");

            if (file != null && file.exists()) {
                try (InputStream in = file.openInputStream()) {
                    JAXBContext context = JAXBContext
                            .newInstance(UserColorInformation.class);
                    Unmarshaller unmarshaller = context.createUnmarshaller();
                    colorInfo = (UserColorInformation) unmarshaller
                            .unmarshal(in);
                } catch (Exception e) {
                    Activator.statusHandler.error(
                            "Unable to read color information from file: "
                                    + file.getName() + " in level "
                                    + LocalizationLevel.USER, e);
                }
            }
        }
        if(colorInfo != null) {
            Map<String, UserColor> colors = colorInfo.getColors();
            if (colors != null) {
                return colors.get(user);
            }
        }
        return null;
    }
}
