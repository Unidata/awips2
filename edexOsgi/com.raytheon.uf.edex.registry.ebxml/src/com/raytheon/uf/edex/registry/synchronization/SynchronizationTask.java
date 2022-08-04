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
package com.raytheon.uf.edex.registry.synchronization;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.hibernate.Session;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.common.registry.services.rest.response.RegObjectSubset;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.registry.ebxml.dao.DbInit;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.RegistryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.util.RegistryIdUtil;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;

/**
 * Process synchronization events
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------
 * Apr 29,  2016  5386     tjensen   Initial creation
 * Jun 20,  2017  6186     rjpeter   Fixed batch numbering.
 * Aug 02,  2017  6186     rjpeter   Cache soap service lookup and add retry.
 * Sep 04,  2018  7238     skabasele added logic for automatic update on out of synch objects.
 *
 * </pre>
 *
 * @author tjensen
 */
@Transactional(propagation = Propagation.REQUIRED, readOnly = true)
public class SynchronizationTask implements Runnable {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    /** Batch size for registry synchronization queries */
    protected static final int SYNC_BATCH_SIZE = Integer
            .parseInt(System.getProperty("ebxml-notification-batch-size"));

    private final RegistrySOAPServices soapService;

    private final String objectType;

    private final String syncUrl;

    private final TransactionTemplate txTemplate;

    private final LifecycleManager remoteLifeCycleManager;

    private final RegistryObjectDao registryObjectDao;

    private final Date synchDelayTime;

    private final CoreDao coreDao;

    /**
     * This Map is designed to contain the id and updateTime of local
     * RegistryObjects where the owner is the local registry
     */
    private HashMap<String, Date> localMapForObjectOwnedByLocalRegistry;

    /**
     * This Map is designed to contain the id and updateTime of local
     * RegistryObjects where the owner is not the local registry
     */
    private HashMap<String, Date> localMapForObjectNotOwnedByLocalRegistry;

    /**
     * This Map is designed to contain the id and updateTime of remote
     * RegistryObjects where the owner is the local registry
     */
    private HashMap<String, Date> remoteMapForObjectOwnedByLocalRegistry;

    /**
     * This Map is designed to contain the id and updateTime of remote
     * RegistryObjects where the owner is not the local registry
     */
    private HashMap<String, Date> remoteMapForObjectNotOwnedByLocalRegistry;

    /**
     * Set contains the initial object Ids that are automatically created during
     * the initialization of the ebxml schema tables
     */
    private static final Set<String> initialSiteDbOjectIdsSet = DbInit
            .getInitialDbOjectIdsSet();

    /**
     * Initial list of the local registryObjects for the current object type.
     * This list will be used for comparison with the list remote objects for
     * the same object type
     */
    private List<RegObjectSubset> localRegObjectSubsetList;

    private final RegistryRESTServices restClient;

    public SynchronizationTask(final String objectType, final String syncUrl,
            final TransactionTemplate txTemplate,
            final RegistrySOAPServices soapService,
            final RegistryRESTServices restClient,
            final RegistryObjectDao registryObjectDao,
            final Date synchDelayTime, final CoreDao coreDao) {
        this.objectType = objectType;
        this.syncUrl = syncUrl;
        this.txTemplate = txTemplate;
        this.soapService = soapService;
        this.restClient = restClient;
        this.registryObjectDao = registryObjectDao;
        this.localRegObjectSubsetList = new ArrayList<>();
        this.synchDelayTime = synchDelayTime;
        this.localMapForObjectOwnedByLocalRegistry = new HashMap<>();
        this.localMapForObjectNotOwnedByLocalRegistry = new HashMap<>();
        this.remoteMapForObjectOwnedByLocalRegistry = new HashMap<>();
        this.remoteMapForObjectNotOwnedByLocalRegistry = new HashMap<>();
        this.localRegObjectSubsetList = new ArrayList<>();
        this.remoteLifeCycleManager = soapService
                .getLifecycleManagerServiceForHost(syncUrl);
        this.coreDao = coreDao;

    }


    @Override
    public void run() {
        /*
         * Create a new session. Once the thread is done, the session is always
         * closed in the finally block
         */
        Session session = coreDao.getSession();

        try {
            logger.info("------ Starting synchronization for objectType  "
                    + objectType);

            /*
             * Get the list of remote object ids, owner and updateTime for the
             * specific object type
             */
            List<RegObjectSubset> remoteRegObjectSubsetList = restClient
                    .getRegistryDataAccessService(syncUrl)
                    .getRegistryObjectIdsOwnerUpdateTimeOfType(objectType);

            /*
             * The query manager used to query the remote registry
             */
            QueryManager queryManager = soapService
                    .getQueryServiceForHost(syncUrl);
            QueryRemoteRegistryHelper queryRemoteRegistryHelper = new QueryRemoteRegistryHelper(
                    objectType, queryManager);

            /*
             * local list of ids, owners, updateTimes per ObjectType
             */
            localRegObjectSubsetList = registryObjectDao
                    .getIdsOwnerUpdateTimeOfType(objectType);

            QueryLocalRegistryHelper queryLocalRegistryHelper = new QueryLocalRegistryHelper(
                    session);

            /*
             * Building the hashMaps
             */
            loadLocalandRemoteMaps(localRegObjectSubsetList,
                    remoteRegObjectSubsetList);

            /*
             * Get id lists only
             */
            List<String> localRegistryObjectSubsetIdsOnly = getIdsList(
                    localRegObjectSubsetList);
            List<String> remoteRegistryObjectSubsetIdsOnly = getIdsList(
                    remoteRegObjectSubsetList);

            SynchronizeInBatches synchronizeInBatches = new SynchronizeInBatches(
                    remoteLifeCycleManager, registryObjectDao, txTemplate,
                    queryRemoteRegistryHelper, objectType,
                    queryLocalRegistryHelper);
            /*
             * Check for purge
             */

            if (remoteRegObjectSubsetList == null
                    || remoteRegObjectSubsetList.isEmpty()) {
                logger.info("0 objects of type [" + objectType
                        + "] present on remote registry. Purging the local database of those specific object types");

                /*
                 * purging
                 * 
                 */
                // ensure that we do not remove the initial db objects
                localRegistryObjectSubsetIdsOnly
                        .removeAll(initialSiteDbOjectIdsSet);

                synchronizeInBatches
                        .localDelete(localRegistryObjectSubsetIdsOnly);
            } else {

                /*
                 * Ensure that an administrator is still able to delete the
                 * ebxml schema tables and re-download objects from central
                 * (remote). However, This occurs *only* when the initial ebxml
                 * schema tables are completely empty or if the ebxml schema
                 * tables are populated solely by the initial objects
                 * automatically created in the database
                 * 
                 * @see DbInit.java {addToInitialDbOjectIdsSet()}
                 * 
                 * Else proceed to synch local and remote by doing all the steps
                 * ( add, update, delete )
                 * 
                 */

                if (localRegObjectSubsetList.isEmpty()
                        || initialSiteDbOjectIdsSet.containsAll(
                                localRegistryObjectSubsetIdsOnly)) {

                    logger.info("Downloading "
                            + remoteRegistryObjectSubsetIdsOnly.size()
                            + " objects to local ");

                    synchronizeInBatches.localInsertOrUpdate(
                            remoteRegistryObjectSubsetIdsOnly);

                } else {

                    /*
                     * compute the synchronization objects
                     */

                    List<String> remoteObjectIdsToAddToLocal = computeObjectsToAddToSite();
                    List<String> remoteObjectobjectIdsToUpdateLocal = computeObjectsToUpdateSite();
                    List<String> remoteObjectsIdsToRemoveFromLocal = computeObjectsToRemoveFromSite();

                    List<String> localObjectIdsToAddToRemote = computeObjectsToAddToRemote();
                    List<String> localobjectIdsToUpdateRemote = computeObjectsToUpdateRemote();
                    List<String> localobjectIdsToRemoveFromRemote = computeObjectsToRemoveFromRemote();

                    if (remoteObjectIdsToAddToLocal.isEmpty()) {
                        logger.info("No object to add to local. Skipping ...");
                    } else {
                        logger.info("Adding to local: "
                                + remoteObjectIdsToAddToLocal.size()
                                + " objects ");
                        synchronizeInBatches.localInsertOrUpdate(
                                remoteObjectIdsToAddToLocal);
                    }

                    if (remoteObjectobjectIdsToUpdateLocal.isEmpty()) {
                        logger.info(
                                "No object to update to local. Skipping ...");

                    } else {
                        logger.info("Updating to local: "
                                + remoteObjectobjectIdsToUpdateLocal.size()
                                + " objects ");
                        synchronizeInBatches.localInsertOrUpdate(
                                remoteObjectobjectIdsToUpdateLocal);

                    }

                    if (remoteObjectsIdsToRemoveFromLocal.isEmpty()) {
                        logger.info(
                                "No object to remove from local. Skipping ...");
                    } else {
                        logger.info("Removing from local: "
                                + remoteObjectsIdsToRemoveFromLocal.size()
                                + " objects ");
                        synchronizeInBatches
                                .localDelete(remoteObjectsIdsToRemoveFromLocal);
                    }

                    if (localObjectIdsToAddToRemote.isEmpty()) {
                        logger.info("No object to add to remote. Skipping ...");
                    } else {
                        logger.info("Adding to remote: "
                                + localObjectIdsToAddToRemote.size()
                                + " objects ");
                        synchronizeInBatches.remoteInsertOrUpdate(
                                localObjectIdsToAddToRemote);
                    }

                    if (localobjectIdsToUpdateRemote.isEmpty()) {
                        logger.info(
                                "No object to update to remote. Skipping ...");
                    } else {
                        logger.info("Updating to remote: "
                                + localobjectIdsToUpdateRemote.size()
                                + " objects ");
                        synchronizeInBatches.remoteInsertOrUpdate(
                                localobjectIdsToUpdateRemote);
                    }

                    if (localobjectIdsToRemoveFromRemote.isEmpty()) {
                        logger.info(
                                "No object to remove from remote. Skipping ...");
                    } else {
                        logger.info("Removing from remote: "
                                + localobjectIdsToRemoveFromRemote.size()
                                + " objects ");
                        synchronizeInBatches
                                .remoteDelete(localobjectIdsToRemoveFromRemote);
                    }

                }

            }

            logger.info("***** Successfully synchronized objectType: "
                    + objectType + "   ");
        } catch (Exception e) {
            logger.error("***** Failed to synchronize objectType: " + objectType
                    + "  *", e);
        } finally {
            // ensuring the session is always closed by the thread
            if (session != null) {
                session.close();
            }
        }
    }

    /**
     * Helper class used to load the instance map based on ownership by the site
     * or not.
     * 
     * @param localRegistryObjectsSubset
     * @param remoteRegistriObjectSubset
     */
    private void loadLocalandRemoteMaps(
            List<RegObjectSubset> localRegistryObjectsSubset,
            List<RegObjectSubset> remoteRegistriObjectSubset) {

        for (int i = 0; i < localRegistryObjectsSubset.size(); i++) {
            RegObjectSubset localSubset = localRegistryObjectsSubset.get(i);
            if (isSiteObjectCreator(localSubset)) {

                this.localMapForObjectOwnedByLocalRegistry
                        .put(localSubset.getId(), localSubset.getUpdateTime());
            } else {
                this.localMapForObjectNotOwnedByLocalRegistry
                        .put(localSubset.getId(), localSubset.getUpdateTime());
            }
        }
        if (remoteRegistriObjectSubset != null) {
            for (int i = 0; i < remoteRegistriObjectSubset.size(); i++) {
                RegObjectSubset remoteSubset = remoteRegistriObjectSubset
                        .get(i);
                if (isSiteObjectCreator(remoteSubset)) {
                    this.remoteMapForObjectOwnedByLocalRegistry.put(
                            remoteSubset.getId(), remoteSubset.getUpdateTime());
                } else {
                    this.remoteMapForObjectNotOwnedByLocalRegistry.put(
                            remoteSubset.getId(), remoteSubset.getUpdateTime());
                }

            }
        }

    }

    /**
     * Method used to get the list of Ids corresponding to the list of
     * RegistryObjects
     * 
     * @param RegistryObjectList
     * @return
     */
    private List<String> getIdsList(List<RegObjectSubset> registryObjectList) {
        List<String> list = new ArrayList<>();
        for (int i = 0; i < registryObjectList.size(); i++) {
            list.add(registryObjectList.get(i).getId());
        }
        return list;
    }

    /**
     * Helper method to find out if the site is the object creator,
     *
     * @param registryObj
     * @return
     */
    private boolean isSiteObjectCreator(RegObjectSubset registryObj) {
        if (RegistryIdUtil.getId().equals(registryObj.getOwner())) {
            return true;
        }
        return false;
    }

    /**
     * Method used to compute the list of objects from the *remote* registry to
     * *add* to the *site* registry. This occurs when an object exists in remote
     * registry and not in the site Registry and only if the site Registry is
     * not the creator of the object.
     * 
     * @param listRemote
     * @param siteExistingIdsSet
     * @return
     */
    private List<String> computeObjectsToAddToSite() {
        List<String> idsToAddToSiteList = new ArrayList<>();
        Iterator<String> remoteIterator = this.remoteMapForObjectNotOwnedByLocalRegistry
                .keySet().iterator();
        while (remoteIterator.hasNext()) {
            String remoteId = remoteIterator.next();
            if (!this.localMapForObjectNotOwnedByLocalRegistry
                    .containsKey(remoteId)) {
                Date remoteUpdaTime = remoteMapForObjectNotOwnedByLocalRegistry
                        .get(remoteId);
                // ensure that the remote time is before the synch delay time
                if (remoteUpdaTime.compareTo(synchDelayTime) < 0) {
                    idsToAddToSiteList.add(remoteId);
                }
            }

        }

        return idsToAddToSiteList;
    }

    /**
     * Method used to compute the list of objects to *remove* from *site*
     * registry. This occurs when an object exists in site registry and not in
     * the remote Registry AND only if the site Registry is *not* the creator of
     * the object.
     * 
     * @param listSite
     * @param remoteExistingIdsSet
     * @return
     */

    private List<String> computeObjectsToRemoveFromSite() {
        List<String> idsToRemoveFromSiteList = new ArrayList<>();
        Iterator<String> it = this.localMapForObjectNotOwnedByLocalRegistry
                .keySet().iterator();

        while (it.hasNext()) {
            String id = it.next();
            if (!this.remoteMapForObjectNotOwnedByLocalRegistry
                    .containsKey(id)) {
                idsToRemoveFromSiteList.add(id);
            }

        }

        return idsToRemoveFromSiteList;
    }

    /**
     * Method used to compute the list of objects from the *site* registry to
     * *add* to the *remote* registry. This occurs when an object exists in site
     * registry and not in the remote Registry and only if the site Registry is
     * the creator of the object.
     * 
     * @param listSite
     * @param remoteExistingIdsSet
     * @return
     */

    private List<String> computeObjectsToAddToRemote() {
        List<String> idsToAddToRemoteList = new ArrayList<>();
        Iterator<String> localIterator = this.localMapForObjectOwnedByLocalRegistry
                .keySet().iterator();
        while (localIterator.hasNext()) {
            String localId = localIterator.next();
            if (!this.remoteMapForObjectOwnedByLocalRegistry
                    .containsKey(localId)) {

                idsToAddToRemoteList.add(localId);

            }

        }

        return idsToAddToRemoteList;
    }

    /**
     * Method used to compute the list of objects to *remove* from *remote*
     * registry. This occurs when an object exists in Remote registry and not in
     * the site Registry and only if the site Registry is the creator of the
     * object.
     *
     * @param listRemote
     * @param siteExistingIdsSet
     * @return
     */

    private List<String> computeObjectsToRemoveFromRemote() {
        List<String> idsToRemoveFromRemoteList = new ArrayList<>();
        Iterator<String> remoteIterator = this.remoteMapForObjectOwnedByLocalRegistry
                .keySet().iterator();
        while (remoteIterator.hasNext()) {
            String remoteId = remoteIterator.next();
            if (!this.localMapForObjectOwnedByLocalRegistry
                    .containsKey(remoteId)) {
                idsToRemoveFromRemoteList.add(remoteId);

            }

        }

        return idsToRemoveFromRemoteList;
    }

    /**
     * Method used to compute the list of objects from the *remote* registry to
     * *update* to the *site* registry. This occurs when an object exists in
     * remote registry and in the site registry AND only if the site Registry is
     * not the creator of the object and the site has an earlier version
     * 
     * 
     * @param listRemote
     * @param listSite
     * @return
     */

    private List<String> computeObjectsToUpdateSite() {
        List<String> idsToUpdateSiteList = new ArrayList<>();
        Iterator<String> remoteIterator = this.remoteMapForObjectNotOwnedByLocalRegistry
                .keySet().iterator();
        while (remoteIterator.hasNext()) {
            String remoteId = remoteIterator.next();
            Date remoteUpdateTime = this.remoteMapForObjectNotOwnedByLocalRegistry
                    .get(remoteId);
            Date localUpdateTime = this.localMapForObjectNotOwnedByLocalRegistry
                    .get(remoteId);

            if (localUpdateTime != null && remoteUpdateTime != null) {
                if (localUpdateTime.compareTo(remoteUpdateTime) < 0) {
                    // only if the remoteObject updatetime is before the
                    // synchDelayTime.
                    if (remoteUpdateTime.compareTo(synchDelayTime) < 0) {
                        idsToUpdateSiteList.add(remoteId);
                    }
                }
            }

        }

        return idsToUpdateSiteList;

    }

    /**
     * Method used to compute the list of objects from the *site* registry to
     * *update* to the *remote* registry. This occurs when an object exists in
     * remote registry and in the site registry AND only if the site registry is
     * the creator of the object and the site has the latest version
     * 
     * 
     * @param listRemote
     * @param listSite
     * @return
     */

    private List<String> computeObjectsToUpdateRemote() {
        List<String> idsToUpdateRemoteList = new ArrayList<>();
        Iterator<String> remoteIterator = this.remoteMapForObjectOwnedByLocalRegistry
                .keySet().iterator();
        while (remoteIterator.hasNext()) {
            String remoteId = remoteIterator.next();
            Date remoteUpdateTime = this.remoteMapForObjectOwnedByLocalRegistry
                    .get(remoteId);
            Date localUpdateTime = this.localMapForObjectOwnedByLocalRegistry
                    .get(remoteId);
            if (localUpdateTime != null && remoteUpdateTime != null) {
                if (remoteUpdateTime.compareTo(localUpdateTime) < 0) {

                    idsToUpdateRemoteList.add(remoteId);

                }
            }

        }

        return idsToUpdateRemoteList;

    }

}
