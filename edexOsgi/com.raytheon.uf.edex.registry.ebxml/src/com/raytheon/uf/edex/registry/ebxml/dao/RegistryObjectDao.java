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

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.hibernate.SQLQuery;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.IDaoConfigFactory;
import com.raytheon.uf.edex.registry.events.DeleteSlotEvent;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Data Access object for interacting with registry object types in the registry
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ---------------------------------------------------------------
 * Mar 13, 2013  1082     bphillip   Initial creation
 * Apr 09, 2013  1802     bphillip   Removed exception catching
 * Jun 04, 2013  2022     bphillip   Added delete objects of type method
 * Jul 29, 2013  2191     bphillip   Added new methods to support registry synchronization
 * Aug 01, 2013  1693     bphillip   Added methods to facilitate implementation of the
 *                                   lifecyclemanager according to the 4.0 spec
 * Feb 13, 2014  2769     bphillip   Added read only flags to query methods
 * Apr 11, 2014  3011     bphillip   Changed merge to not delete unused slots
 * Apr 21, 2014  2992     dhladky    General list of Registry server nodes.
 * Oct 16, 2014  3454     bphillip   Upgrading to Hibernate 4
 * Jan 29, 2019  7238     skabasele  Removed the unused sloteTypeDao reference
 * Jul 25, 2019  7890     ksunil     Enhanced logging.
 * Jul 23, 2019  7839     skabasele  Implement method to retrieve with registryObject's orphaned
 *                                   slots
 * May 11, 2020  8161     bsteffen   Change slot id to a long
 * May 26, 2020  8165     bsteffen   Remove auditableevent and notification.
 * Oct 30, 2020  8170     ksunil     Removed references to deleted tables
 * Apr 14, 2021  7849     mapeters   Refactor to use TransactionTemplate.execute instead of
 *                                   Transactional annotations to support admin/non-admin instances
 *
 * </pre>
 *
 * @author bphillip
 */
public class RegistryObjectDao
        extends RegistryObjectTypeDao<RegistryObjectType> {

    protected static final Logger statusHandler = LoggerFactory
            .getLogger(RegistryObjectDao.class);

    /** Get Ids by object_types */
    private static final String GET_IDS_BY_OBJECT_TYPE = "SELECT regObj.id FROM RegistryObjectType regObj WHERE regObj.objectType=:objectType";

    /** Get the unique list of registry nodes with subscriptions **/
    private static final String QUERY_UNIQUE_REGISTRIES = "SELECT DISTINCT regObj.owner FROM RegistryObjectType regObj WHERE regObj.objectType LIKE ";

    /** Query to determine if an object id exists in the registry */
    private static final String ID_EXISTS_QUERY = "select count(obj.id) from RegistryObjectType obj where id=:id";

    /** Query to determine if an object lid exists in the registry */
    private static final String LID_EXISTS_QUERY = "select count(obj.lid) from RegistryObjectType obj where lid=:lid";

    /** Query to get all sub versions beneath the given version */
    private static String GET_SUB_VERSION_QUERY = "select obj.versionInfo.versionName from RegistryObjectType obj where obj.lid=:lid and obj.versionInfo.versionName like :version";

    /** Batch size for registry synchronization queries */
    private static final int SYNC_BATCH_SIZE = Integer
            .parseInt(System.getProperty("ebxml-notification-batch-size"));

    /**
     * List the tables that can have references in the slot table minus the
     * obvious Registryobject and Slot tables
     */
    private static final String[] validSlotParentTables = { "association",
            "classification", "classificationnode", "classificationscheme",
            "comment", "emailaddress", "externalidentifier", "externallink",
            "extrinsicobject", "federation", "organization", "parameter",
            "person", "personname", "postaladdress", "query", "querydefinition",
            "registry", "registrypackage", "role", "subscription",
            "telephonenumber" };

    public RegistryObjectDao(IDaoConfigFactory daoConfigFactory) {
        this(daoConfigFactory, false);
    }

    public RegistryObjectDao(IDaoConfigFactory daoConfigFactory,
            boolean admin) {
        super(daoConfigFactory, admin);
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
        runInTransaction(() -> {
            newObject.setId(existingObject.getId());
            getCurrentSession().merge(newObject);
        });
    }

    /**
     * Gets the next version of the given object.
     *
     * @param objectToVersion
     *            The object to get the next version number for
     * @return The next version number
     */
    public String getNextVersion(RegistryObjectType objectToVersion) {
        return supplyInTransaction(requiredReadOnlyTransactionDef, () -> {
            String lid = objectToVersion.getLid();
            String version = objectToVersion.getVersionInfo().getVersionName();

            // Gets all the subversion numbers of this object
            List<String> queryResult = this.executeHQLQuery(
                    GET_SUB_VERSION_QUERY, "lid", lid, "version",
                    version + ".%");
            int maxSubVersion = 0;

            // Get the maximum of the retrieved versions
            for (String ver : queryResult) {
                if (ver.matches(version + "\\.\\d{1,10}")) {
                    String[] tokens = ver.split("\\.");
                    int parsedVersion = Integer
                            .parseInt(tokens[tokens.length - 1]);
                    if (parsedVersion > maxSubVersion) {
                        maxSubVersion = parsedVersion;
                    }
                }
            }
            // Increment the max version and append to current version
            return version + "." + String.valueOf(maxSubVersion + 1);
        });
    }

    /**
     * Checks if the given object id exists in the registry
     *
     * @param id
     *            The id to check
     * @return True if the id exists, else false
     */
    public boolean idExists(String id) {
        return ((Long) this.executeHQLQuery(ID_EXISTS_QUERY, "id", id)
                .get(0)) != 0;
    }

    /**
     * Checks if the given object lid exists in the registry
     *
     * @param lid
     *            The lid to check
     * @return True if the lid exists, else false
     */
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
        statusHandler.debug("Deleting from getCurrentSession.");
        runInTransaction(() -> {
            getCurrentSession().delete(obj);
        });
    }

    public void deleteObjectWithoutDeletingChildren(RegistryObjectType obj)
            throws DataAccessLayerException {
        statusHandler.debug("DELETE FROM RegistryObjectType obj where obj.id="
                + obj.getId());
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
     */
    public List<RegistryObjectType> getAllRegistryObjects() {
        return getAll();
    }

    @Override
    protected Class<RegistryObjectType> getEntityClass() {
        return RegistryObjectType.class;
    }

    /**
     * Gets a list of unique registry ID's with data on this node for this
     * objectType.
     *
     * @param objectType
     * @return
     */
    public List<String> getUniqueRegistries(String objectType) {
        return this.executeHQLQuery(QUERY_UNIQUE_REGISTRIES + " " + objectType);
    }

    /**
     * Method used to purge all orphaned registry object slots
     */
    public synchronized void purgeAllSlotWithoutRegObjParent() {
        statusHandler.info(
                "Scanning the database for orphaned registry objects slots ....  ");
        runInTransaction(requiredReadOnlyTransactionDef, () -> {
            long slotPurgingStart = TimeUtil.currentTimeMillis();
            int fetchSize = 100_000;
            Set<String> allValidSlotParentIds = new HashSet<>();
            Set<Long> validRegObOprhanedSlotIds = new HashSet<>();

            // getting the valid parent ids
            for (String tablename : validSlotParentTables) {
                List<String> validSlotParentIds = getIdsFromSpecifiedTable(
                        tablename);
                allValidSlotParentIds.addAll(validSlotParentIds);

            }

            ScrollableResults scResults = getCurrentSession().createSQLQuery(
                    "select id, parent_id from ebxml.slot where parent_id not in (select id from ebxml.registryobject);")
                    .setReadOnly(true).setFetchSize(fetchSize)
                    .setCacheable(false).scroll(ScrollMode.FORWARD_ONLY);

            int rowCount = 0;
            int fetchCount = 0;
            while (scResults.next()) {
                Object[] result = scResults.get();
                Long slotId = (Long) result[0];
                String parentId = (String) result[1];
                rowCount++;

                if (rowCount < fetchSize) {
                    if (!allValidSlotParentIds.contains(parentId)) {
                        validRegObOprhanedSlotIds.add(slotId);
                    }
                } else {
                    ++fetchCount;
                    purgeAndPrintLoggingStatements(fetchCount, slotPurgingStart,
                            validRegObOprhanedSlotIds);

                    if (!validRegObOprhanedSlotIds.isEmpty()) {
                        validRegObOprhanedSlotIds = new HashSet<>();
                    }

                    rowCount = 0;

                }

            }

            // last fetch
            purgeAndPrintLoggingStatements(fetchCount + 1, slotPurgingStart,
                    validRegObOprhanedSlotIds);
            statusHandler
                    .info("Registry object's oprhaned slot purge completed in  "
                            + (TimeUtil.currentTimeMillis() - slotPurgingStart)
                            + " ms");

            scResults.close();
        });
    }

    /**
     * Method used to process the purge and print logging statements.
     *
     * @param fetchCount
     * @param slotPurgingStart
     * @param validRegObOprhanedSlotIds
     */
    private synchronized void purgeAndPrintLoggingStatements(int fetchCount,
            long slotPurgingStart, Set<Long> validRegObOprhanedSlotIds) {
        runInTransaction(() -> {
            statusHandler.info(
                    " Database fetch " + "( " + fetchCount + " ) completed in "
                            + (TimeUtil.currentTimeMillis() - slotPurgingStart)
                            + " ms");
            if (validRegObOprhanedSlotIds.isEmpty()) {

                statusHandler.info(
                        " No orphaned registry objects slots  found in fetch "
                                + "( " + fetchCount + " ) ");
            } else {
                statusHandler.info(validRegObOprhanedSlotIds.size()
                        + " orphaned registry objects slots  found in fetch "
                        + "( " + fetchCount + " ). Initiating delete... ");
                validRegObOprhanedSlotIds.remove(null);
                deleteSlotsInBatches(validRegObOprhanedSlotIds.stream()
                        .collect(Collectors.toList()));
            }
        });
    }

    /**
     * Method used to process the slot purge in batches
     *
     * @param validRegObOprhanedSlotIds
     */
    private synchronized void deleteSlotsInBatches(List<Long> ids) {
        runInTransaction(() -> {
            int batches = (int) Math
                    .ceil(((float) ids.size()) / SYNC_BATCH_SIZE);

            boolean retry = false;
            int sIndex = 0;
            int tries = 0;
            for (int currentBatch = 1; currentBatch <= batches; currentBatch++) {
                tries = 0;
                statusHandler.info("Processing slot delete for batch "
                        + currentBatch + "/" + batches);
                int eIndex = sIndex + SYNC_BATCH_SIZE;
                if (eIndex > ids.size()) {
                    eIndex = ids.size();
                }

                List<Long> batch = ids.subList(sIndex, eIndex);
                do {
                    retry = false;
                    try {

                        DeleteSlotEvent deleteSlotEvent = new DeleteSlotEvent();
                        deleteSlotEvent.setSlotsToDelete(batch);
                        EventBus.publish(deleteSlotEvent);

                    } catch (Exception e) {
                        if (tries < 1) {
                            statusHandler.error(
                                    "Error occurred purging batch for slotIds ["
                                            + batch.toString()
                                            + "], retrying...",
                                    e);
                            tries++;
                            retry = true;
                        } else {
                            statusHandler.error(
                                    "Error occurred purging batch for slotsIds ["
                                            + batch.toString()
                                            + "], skipping batch...",
                                    e);
                        }
                    }
                } while (retry);
                sIndex = eIndex;
            }
        });
    }

    /**
     * Method used to the ids of the specified table name.
     *
     * @param tablename
     * @return
     */
    public synchronized List<String> getIdsFromSpecifiedTable(
            String tablename) {
        return supplyInTransaction(() -> {
            String queryString = " select id from ebxml.%s ";

            queryString = String.format(queryString, tablename);

            SQLQuery query = getCurrentSession().createSQLQuery(queryString);
            @SuppressWarnings("unchecked")
            List<String> list = query.list();
            return list;

        });
    }

}
