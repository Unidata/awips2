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

import java.io.File;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.collaboration.ui.role.UserInformation;

/**
 * Parse a file to grab attributes about a user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class UserInformationParser {

    /**
     * Go to the userinformation.xml file and grab the user information, for use
     * in determining what kind of user you are in collaboration
     * 
     * @return
     */
    public static UserInformation parseUserInformation() {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> files = pm
                .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                        "collaboration" + File.separator
                                + "userInformation.xml");

        LocalizationFile lFile = null;
        for (LocalizationFile file : files.values()) {
            // this is not something that the user should define
            if (file.getContext().getLocalizationLevel().isSystemLevel()
                    || file.getContext().getLocalizationLevel() == LocalizationLevel.SITE) {
                lFile = file;
            }
        }

        File toFile = lFile.getFile();
        try {
            JAXBContext context = JAXBContext
                    .newInstance(UserInformation.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();
            UserInformation info = (UserInformation) unmarshaller
                    .unmarshal(toFile);
            return info;
        } catch (JAXBException e) {
            e.printStackTrace();
        }
        return null;
    }
}
