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
package com.raytheon.uf.edex.datadelivery.registry.web;

import java.io.File;
import java.util.List;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlRootElement;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ParameterType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.reflections.Reflections;
import org.reflections.scanners.TypeAnnotationsScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.datadelivery.registry.web.IRegistryDataAccessService;
import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.registry.ebxml.dao.QueryDefinitionDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil;

/**
 * 
 * Implementation of the registry data access service interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/29/2013    2191        bphillip    Initial implementation
 * 9/20/2013    2385        bphillip    Added subscription backup functions
 * 10/2/2013    2385        bphillip    Fixed subscription backup queries
 * 10/8/2013    1682        bphillip    Added query queries
 * 11/7/2013    1678        bphillip    Added getCustomQueries method
 * Mar 31, 2014 2889        dhladky     Added username for notification center tracking.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
@Path(IRegistryDataAccessService.DATA_ACCESS_PATH_PREFIX)
public class RegistryDataAccessService implements IRegistryDataAccessService {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryDataAccessService.class);

    private static final File SUBSCRIPTION_BACKUP_DIR = new File(
            System.getProperty("edex.home")
                    + "/data/registrySubscriptionBackup");

    private static final String GET_SINGLE_SUBSCRIPTIONS_QUERY = "FROM RegistryObjectType obj "
            + "where (obj.objectType like '%SiteSubscription' "
            + "OR obj.objectType like '%SharedSubscription') "
            + "AND obj.id=:id";

    private static final String GET_SUBSCRIPTIONS_QUERY = "FROM RegistryObjectType obj "
            + "where obj.objectType like '%SiteSubscription' "
            + "OR obj.objectType like '%SharedSubscription' order by obj.id asc";

    private static final JAXBManager subscriptionJaxbManager = initJaxbManager();

    /** Data access object for registry objects */
    private RegistryObjectDao registryObjectDao;

    /** Data access object for query definitions */
    private QueryDefinitionDao queryDefinitionDao;

    /** Lifecyclemanager */
    private LifecycleManager lcm;

    public RegistryDataAccessService() {

    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .getRegistryObjectIdsOfType(String)
     */
    @Override
    @GET
    @Path("getRegistryObjectIds/{objectType}")
    public RestCollectionResponse<String> getRegistryObjectIdsOfType(
            @PathParam("objectType") String objectType) {
        statusHandler.info("Getting registry object ids of type [" + objectType
                + "]...");
        RestCollectionResponse<String> response = new RestCollectionResponse<String>();
        response.setPayload(registryObjectDao
                .getRegistryObjectIdsOfType(objectType));
        return response;
    }

    @Override
    @GET
    @Path("getQueries")
    public String getValidQueries() {
        statusHandler.debug("Getting valid queries...");
        List<String> ids = queryDefinitionDao.getQueryIds();
        StringBuilder builder = new StringBuilder();
        for (String id : ids) {
            builder.append(id).append(StringUtil.NEWLINE);
        }
        return builder.toString();
    }

    @Override
    @GET
    @Path("getCustomQueries")
    public String getCustomQueries() {
        List<String> ids = queryDefinitionDao.getQueryIds();
        ids.removeAll(CanonicalQueryTypes.CANONICAL_QUERY_TYPES);
        return RegistryQueryUtil.formatArrayString(ids.toArray());
    }

    @Override
    @GET
    @Path("getParametersForQuery/{queryId}")
    public String getParametersForQuery(@PathParam("queryId") String queryId) {
        statusHandler.debug("Getting query parameters for query: " + queryId
                + "...");
        List<ParameterType> parameters = queryDefinitionDao
                .getParametersForQuery(queryId);
        StringBuilder retVal = new StringBuilder();
        for (ParameterType param : parameters) {
            retVal.append(param.getParameterName()).append(StringUtil.NEWLINE);
            retVal.append(param.getDataType()).append(StringUtil.NEWLINE);
            retVal.append(param.getDefaultValue()).append(StringUtil.NEWLINE);
        }
        return retVal.toString();
    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .removeSubscriptionsForSite(String)
     */
    @Override
    @GET
    @Path("removeSubscriptionsFor/{siteId}")
    public void removeSubscriptionsForSite(@PathParam("siteId") String siteId) {
        statusHandler.info("Removing subscriptions for: " + siteId);
        List<SubscriptionType> subscriptions = registryObjectDao
                .executeHQLQuery(
                        "from SubscriptionType sub where sub.owner=:siteId",
                        "siteId", siteId);
        if (subscriptions.isEmpty()) {
            statusHandler.info("No subscriptions present for site: " + siteId);
        } else {
            ObjectRefListType refList = new ObjectRefListType();
            for (SubscriptionType sub : subscriptions) {
                refList.getObjectRef().add(new ObjectRefType(sub.getId()));
            }
            RemoveObjectsRequest removeRequest = new RemoveObjectsRequest();
            removeRequest.setDeleteChildren(true);
            removeRequest.setId("Remote subscription removal request for "
                    + siteId);
            removeRequest.setComment("Removal of remote subscriptions for "
                    + siteId);
            removeRequest.setObjectRefList(refList);
            try {
                lcm.removeObjects(removeRequest);
            } catch (Exception e) {
                throw new RegistryException(
                        "Error removing subscriptions for site " + siteId, e);
            }
        }
    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .getSubscriptions()
     */
    @Override
    @GET
    @Path("getSubscriptions")
    public String getSubscriptions() {
        String[] slotNames = new String[] { "name", "owner", "dataSetName",
                "provider", "dataSetType", "route", "active", "groupName",
                "valid", "fullDataSet" };

        StringBuilder response = new StringBuilder();
        response.append("<table border=\"1\" style=\"border-style:solid; border-width:thin\"> ");
        response.append("<tr>");
        response.append("<th>ID</th>");
        for (String slotName : slotNames) {
            response.append("<th>").append(slotName).append("</th>");
        }
        response.append("</tr>");

        List<RegistryObjectType> subs = registryObjectDao
                .executeHQLQuery(GET_SUBSCRIPTIONS_QUERY);
        for (RegistryObjectType obj : subs) {
            String[] values = new String[slotNames.length + 1];
            values[0] = obj.getId();
            for (int i = 0; i < slotNames.length; i++) {
                values[i + 1] = String.valueOf(obj.getSlotValue(slotNames[i]));
            }
            response.append("<tr>");
            for (String val : values) {
                response.append("<td>").append(val).append("</td>");
            }
            response.append("</tr>");
        }
        response.append("</table>");
        return response.toString();
    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .backupSubscription(String)
     */
    @Override
    @GET
    @Path("backupSubscription/{subscriptionName}")
    public String backupSubscription(
            @PathParam("subscriptionName") String subscriptionName) {
        StringBuilder response = new StringBuilder();
        List<RegistryObjectType> result = registryObjectDao.executeHQLQuery(
                GET_SINGLE_SUBSCRIPTIONS_QUERY, "id", subscriptionName);

        if (CollectionUtil.isNullOrEmpty(result)) {
            response.append("Subscription with ID [").append(subscriptionName)
                    .append("] not found in registry");
        } else {
            RegistryObjectType sub = result.get(0);
            if (!SUBSCRIPTION_BACKUP_DIR.exists()) {
                SUBSCRIPTION_BACKUP_DIR.mkdirs();
            }
            String subId = sub.getId();
            File backupFile = new File(SUBSCRIPTION_BACKUP_DIR.getPath()
                    + File.separator + subId);
            SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest();
            submitObjectsRequest.setCheckReferences(false);
            submitObjectsRequest
                    .setComment("Restoring backed up subscriptions");
            submitObjectsRequest.setId("Restore subscription [" + subId + "]");
            submitObjectsRequest.setMode(Mode.CREATE_OR_REPLACE);
            submitObjectsRequest.setUsername(RegistryUtil.registryUser);
            submitObjectsRequest
                    .setRegistryObjectList(new RegistryObjectListType());
            submitObjectsRequest.getRegistryObjects().add(sub);

            JAXB.marshal(submitObjectsRequest, backupFile);
            response.append("Subscription [").append(subId)
                    .append("] successfully backed up to [")
                    .append(backupFile.getPath()).append("]<br>");
        }
        return response.toString();
    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .backupAllSubscriptions()
     */
    @Override
    @GET
    @Path("backupAllSubscriptions/")
    public String backupAllSubscriptions() {
        StringBuilder response = new StringBuilder();
        List<RegistryObjectType> subs = registryObjectDao
                .executeHQLQuery(GET_SUBSCRIPTIONS_QUERY);
        if (subs.isEmpty()) {
            response.append("No subscriptions present to backup!");
        } else {
            for (RegistryObjectType sub : subs) {
                response.append(backupSubscription(sub.getId()));
            }
        }
        return response.toString();
    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .restoreSubscription(String)
     */
    @Override
    @GET
    @Path("restoreSubscription/{subscriptionName}")
    public String restoreSubscription(
            @PathParam("subscriptionName") String subscriptionName) {
        StringBuilder response = new StringBuilder();
        File subscriptionFile = new File(SUBSCRIPTION_BACKUP_DIR
                + File.separator + subscriptionName);
        if (subscriptionFile.exists()) {
            SubmitObjectsRequest submitRequest = JAXB.unmarshal(
                    subscriptionFile, SubmitObjectsRequest.class);
            String subscriptionXML = submitRequest.getRegistryObjects().get(0)
                    .getSlotByName("content").getSlotValue().getValue();

            try {
                Object subObj = subscriptionJaxbManager
                        .unmarshalFromXml(subscriptionXML);
                EDEXUtil.getMessageProducer().sendSync("scheduleSubscription",
                        subObj);
                lcm.submitObjects(submitRequest);
                subscriptionFile.delete();
                response.append(
                        "Subscription successfully restored from file [")
                        .append(subscriptionFile).append("]<br>");
            } catch (EdexException e1) {
                statusHandler.error("Error submitting subscription", e1);
                response.append("Subscription from file [")
                        .append(subscriptionFile)
                        .append("] failed to be restored: ")
                        .append(e1.getLocalizedMessage()).append("<br>");
            } catch (MsgRegistryException e) {
                response.append("Error restoring subscription from file [")
                        .append(subscriptionFile).append("] ")
                        .append(e.getMessage()).append("<br>");
                statusHandler.error("Error restoring subscription from file ["
                        + subscriptionFile + "]", e);
                return response.toString();
            } catch (JAXBException e) {
                response.append("Error restoring subscription from file [")
                        .append(subscriptionFile).append("] ")
                        .append(e.getMessage()).append("<br>");
                statusHandler.error("Error restoring subscription from file ["
                        + subscriptionFile + "]", e);
            }
        } else {
            response.append("No backup file exists for subscription[")
                    .append(subscriptionName).append("]<br>");
        }

        return response.toString();
    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .restoreSubscriptions()
     */
    @Override
    @GET
    @Path("restoreSubscriptions/")
    public String restoreSubscriptions() {
        StringBuilder response = new StringBuilder();
        if (SUBSCRIPTION_BACKUP_DIR.exists()) {
            File[] filesToRestore = SUBSCRIPTION_BACKUP_DIR.listFiles();
            if (filesToRestore.length == 0) {
                response.append("No subscriptions found to restore<br>");
            } else {
                for (File subscription : filesToRestore) {
                    response.append(restoreSubscription(subscription.getName()));
                }
            }
        } else {
            response.append("No subscriptions found to restore<br>");
        }
        return response.toString();
    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .clearSubscriptionBackupFiles()
     */
    @GET
    @Path("clearSubscriptionBackupFiles/")
    public String clearSubscriptionBackupFiles() {
        StringBuilder response = new StringBuilder();
        if (SUBSCRIPTION_BACKUP_DIR.exists()) {
            File[] filesToDelete = SUBSCRIPTION_BACKUP_DIR.listFiles();
            if (filesToDelete.length == 0) {
                response.append("No backup files to delete");
            }
            for (File file : filesToDelete) {
                if (file.delete()) {
                    response.append("Deleted backup file [")
                            .append(file.getPath()).append("]<br>");
                } else {
                    response.append("Error deleting backup file [")
                            .append(file.getPath()).append("]<br>");
                }
            }
        } else {
            response.append("No backup files to delete");
        }
        return response.toString();
    }

    /**
     * Initializes the JAXBManager for datadelivery classes.
     * 
     * @return JAXBManager for datadelivery classes
     */
    private static JAXBManager initJaxbManager() {
        ConfigurationBuilder cb = new ConfigurationBuilder();
        cb.addUrls(ClasspathHelper
                .forPackage("com.raytheon.uf.common.datadelivery.registry"));
        cb.setScanners(new TypeAnnotationsScanner());
        Reflections reflecs = cb.build();
        Set<Class<?>> classes = reflecs
                .getTypesAnnotatedWith(XmlRootElement.class);
        try {
            return new JAXBManager(
                    classes.toArray(new Class<?>[classes.size()]));
        } catch (JAXBException e) {
            throw new RuntimeException(
                    "Error initializing subscription jaxb Manager!", e);
        }
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setLcm(LifecycleManager lcm) {
        this.lcm = lcm;
    }

    public void setQueryDefinitionDao(QueryDefinitionDao queryDefinitionDao) {
        this.queryDefinitionDao = queryDefinitionDao;
    }
}
