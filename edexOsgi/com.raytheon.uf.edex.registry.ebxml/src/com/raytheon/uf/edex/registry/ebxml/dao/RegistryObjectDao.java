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
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Data Access object for interacting with registry object types in the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/13/2013    1082       bphillip    Initial creation
 * 4/9/2013     1802       bphillip    Removed exception catching
 * 6/4/2013     2022       bphillip    Added delete objects of type method
 * 7/29/2013    2191       bphillip    Added new methods to support registry synchronization
 * 8/1/2013     1693       bphillip    Added methods to facilitate implementation of the lifecyclemanager according to the 4.0 spec
 * 2/13/2014    2769       bphillip    Added read only flags to query methods
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class RegistryObjectDao extends
        RegistryObjectTypeDao<RegistryObjectType> {

    /** Data access object for accessing slots */
    private SlotTypeDao slotDao;

    /** Delete object type parameterized statement */
    private static final String GET_IDS_BY_OBJECT_TYPE = "SELECT regObj.id FROM RegistryObjectType regObj WHERE regObj.objectType=:objectType";

    /** Query to determine if an object id exists in the registry */
    private static final String ID_EXISTS_QUERY = "select count(obj.id) from RegistryObjectType obj where id=:id";

    /** Query to determine if an object lid exists in the registry */
    private static final String LID_EXISTS_QUERY = "select count(obj.lid) from RegistryObjectType obj where lid=:lid";

    /** Query to get all sub versions beneath the given version */
    private static String GET_SUB_VERSION_QUERY = "select obj.versionInfo.versionName from RegistryObjectType obj where obj.lid=:lid and obj.versionInfo.versionName like :version";

    /**
     * Creates a new RegistryObjectDao
     */
    public RegistryObjectDao() {
    }

    /**
     * Merges the state of the new object onto the persistent object
     * 
     * @param newObject
     *            The object to get the state from
     * @param existingObject
     *            The existing persistent object to copy the state on to
     */
    public void merge(RegistryObjectType newObject,
            RegistryObjectType existingObject) {
        // Delete the existing slot to prevent orphans
        for (SlotType slot : existingObject.getSlot()) {
            slotDao.delete(slot);
        }
        newObject.setId(existingObject.getId());
        template.merge(newObject);
    }

    /**
     * Gets the next version of the given object.
     * 
     * @param objectToVersion
     *            The object to get the next version number for
     * @return The next version number
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public String getNextVersion(RegistryObjectType objectToVersion) {
        String lid = objectToVersion.getLid();
        String version = objectToVersion.getVersionInfo().getVersionName();

        // Gets all the subversion numbers of this object
        List<String> queryResult = this.executeHQLQuery(GET_SUB_VERSION_QUERY,
                "lid", lid, "version", version + ".%");
        int maxSubVersion = 0;

        // Get the maximum of the retrieved versions
        for (String ver : queryResult) {
            if (ver.matches(version + "\\.\\d{1,10}")) {
                String[] tokens = ver.split("\\.");
                int parsedVersion = Integer.parseInt(tokens[tokens.length - 1]);
                if (parsedVersion > maxSubVersion) {
                    maxSubVersion = parsedVersion;
                }
            }
        }
        // Increment the max version and append to current version
        return version + "." + String.valueOf(maxSubVersion + 1);
    }

    /**
     * Checks if the given object id exists in the registry
     * 
     * @param id
     *            The id to check
     * @return True if the id exists, else false
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public boolean idExists(String id) {
        return ((Long) this.executeHQLQuery(ID_EXISTS_QUERY, "id", id).get(0)) != 0;
    }

    /**
     * Checks if the given object lid exists in the registry
     * 
     * @param lid
     *            The lid to check
     * @return Treu if the lid exists, else false
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public boolean lidExists(String lid) {
        return ((Long) this.executeHQLQuery(LID_EXISTS_QUERY, "lid", lid)
                .get(0)) != 0;
    }

    /**
     * Gets the object ids of objects of the given object type
     * 
     * @param objectType
     *            The object type to get the ids for
     * @return The list of object ids of objects of the given type
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<String> getRegistryObjectIdsOfType(String objectType) {
        return this.executeHQLQuery(GET_IDS_BY_OBJECT_TYPE, "objectType",
                objectType);
    }

    /**
     * Deletes a persistent object
     * 
     * @param obj
     *            The persistent object to delete
     */
    public void deleteWithoutMerge(RegistryObjectType obj) {
        this.template.delete(obj);
    }

    public void deleteObjectWithoutDeletingChildren(RegistryObjectType obj)
            throws DataAccessLayerException {
        this.executeHQLStatement(
                "DELETE FROM RegistryObjectType obj where obj.id=:id", "id",
                obj.getId());
    }

    /**
     * Retrieves all registry objects from the registry
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @return All the registry objects contained in the registry
     * @throws EbxmlRegistryException
     *             If the HQL query fails
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<RegistryObjectType> getAllRegistryObjects() {
        return getAll();
    }

    @Override
    protected Class<RegistryObjectType> getEntityClass() {
        return RegistryObjectType.class;
    }

    public void setSlotDao(SlotTypeDao slotDao) {
        this.slotDao = slotDao;
    }

}
