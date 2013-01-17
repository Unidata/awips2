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
package com.raytheon.uf.edex.plugin.nwsauth;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import javax.xml.bind.JAXBException;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.plugin.nwsauth.xml.NwsRoleData;
import com.raytheon.uf.common.plugin.nwsauth.xml.UserXML;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;

/**
 * Test {@link FileManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2013 1412       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class FileManagerTest {
    private static JAXBManager jaxbManager;

    private final UserXML someUser = new UserXML("someUser");

    private FileManager manager;

    @BeforeClass
    public static void classSetup() throws JAXBException {
        jaxbManager = new JAXBManager(NwsRoleData.class);
    }

    @Before
    public void setUp() {
        TimeUtilTest.freezeTime();
        PathManagerFactoryTest.initLocalization();
        manager = new FileManager();
    }

    @After
    public void tearDown() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void fileNewerOnDiskIsReadBeforeResponse()
            throws LocalizationException {

        addUserToUserAdminFile();

        verifyUserIsFoundWhenRoleDataRetrieved();
    }

    @Test
    public void fileOlderOnDiskIsNotReadBeforeResponse()
            throws LocalizationException {

        addUserToUserAdminFile();
        setUserAdminFileModifiedTimeToOneSecondAgo();

        verifyUserIsNotFoundWhenRoleDataRetrieved();
    }

    private void verifyUserIsFoundWhenRoleDataRetrieved() {
        final Map<String, NwsRoleData> roleDataMap = manager.getRoleDataMap();
        assertTrue(
                "Did not find the user added to the role data map!",
                roleDataMap.get("TestUserRoles").getUserList()
                        .contains(someUser));
    }

    /**
     * @param someUser
     */
    private void verifyUserIsNotFoundWhenRoleDataRetrieved() {
        final Map<String, NwsRoleData> roleDataMap = manager.getRoleDataMap();
        assertFalse(
                "Should not have found the user added to the role data map!",
                roleDataMap.get("TestUserRoles").getUserList()
                        .contains(someUser));

    }

    private void addUserToUserAdminFile() throws LocalizationException {
        final LocalizationFile file = getTestUserAdminRolesLocalizationFile();
        NwsRoleData roleData = file.jaxbUnmarshal(NwsRoleData.class,
                jaxbManager);

        roleData.getUserList().add(someUser);
        file.jaxbMarshal(roleData, jaxbManager);
        file.save();
        // The file was written out 1 second after we last read it
        file.getFile().setLastModified(
                TimeUtil.currentTimeMillis() + TimeUtil.MILLIS_PER_SECOND);
    }

    private void setUserAdminFileModifiedTimeToOneSecondAgo() {
        // The file was written out 1 second before we last read it
        getTestUserAdminRolesLocalizationFile().getFile().setLastModified(
                TimeUtil.currentTimeMillis() - TimeUtil.MILLIS_PER_SECOND);
    }

    private LocalizationFile getTestUserAdminRolesLocalizationFile() {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        final LocalizationFile file = pathManager.getLocalizationFile(
                new LocalizationContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.SITE, "OAX"),
                "roles/testUserAdminRoles.xml");
        return file;
    }

}
