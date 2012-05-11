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
package com.raytheon.uf.viz.collaboration.ui.role.dataprovider;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.entity.ByteArrayEntity;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.comm.NetworkStatistics;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.compression.CompressionUtil;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.IPersistedEvent;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;

/**
 * Class responsible for object event storage. Will persist/retrieve objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationObjectEventStorage implements
        IObjectEventPersistance, IObjectEventRetrieval {

    private static final String SESSION_DATA_URL_FORMAT_STRING = "http://%s:%d/session_data/";

    private static final int SESSION_DATA_PORT = 80;

    private static NetworkStatistics stats = com.raytheon.uf.viz.collaboration.comm.Activator
            .getDefault().getNetworkStats();

    public static IObjectEventPersistance createPersistanceObject(
            ISharedDisplaySession session) throws CollaborationException {
        CollaborationObjectEventStorage persistance = new CollaborationObjectEventStorage(
                session);
        persistance.createFolder(URI.create(persistance.sessionDataURL));
        return persistance;
    }

    public static IObjectEventRetrieval createRetrievalObject(
            ISharedDisplaySession session) {
        return new CollaborationObjectEventStorage(session);
    }

    private String sessionDataURL;

    private HttpClient client;

    private CollaborationObjectEventStorage(ISharedDisplaySession session) {
        this.client = HttpClient.getInstance();
        String collaborationServer = Activator.getDefault()
                .getPreferenceStore().getString(CollabPrefConstants.P_SERVER);
        if (collaborationServer != null) {
            sessionDataURL = String.format(SESSION_DATA_URL_FORMAT_STRING,
                    collaborationServer, SESSION_DATA_PORT);
            sessionDataURL += session.getSessionId() + "/";
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IObjectEventPersistance
     * #persistEvent
     * (com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent)
     */
    @Override
    public IPersistedEvent persistEvent(AbstractDispatchingObjectEvent event)
            throws CollaborationException {
        if (event instanceof ICreationEvent) {
            createFolder(String.valueOf(event.getObjectId()));
        } else if (event instanceof DisposeObjectEvent) {
            deleteResource(URI.create(sessionDataURL + event.getObjectId()
                    + "/"));
            CollaborationWrappedEvent wrapped = new CollaborationWrappedEvent();
            wrapped.setEvent(event);
            return wrapped;
        }

        try {
            CollaborationHttpPersistedEvent wrapped = new CollaborationHttpPersistedEvent();
            String objectPath = event.getObjectId() + "/"
                    + event.getClass().getName() + ".obj";
            String eventObjectURL = sessionDataURL + objectPath;
            HttpPut put = new HttpPut(eventObjectURL);

            CollaborationHttpPersistedObject persistObject = new CollaborationHttpPersistedObject();
            persistObject.persistTime = System.currentTimeMillis();
            persistObject.event = event;
            byte[] toPersist = CompressionUtil.compress(SerializationUtil
                    .transformToThrift(persistObject));
            stats.log(event.getClass().getSimpleName(), toPersist.length, 0);
            put.setEntity(new ByteArrayEntity(toPersist));
            HttpClientResponse response = executeRequest(put);
            if (isSuccess(response.code) == false) {
                throw new CollaborationException(
                        "Error uploading event object (" + event.getObjectId()
                                + ") to server @ " + eventObjectURL + " : "
                                + new String(response.data));
            }
            wrapped.setResourcePath(objectPath);
            return wrapped;
        } catch (CollaborationException e) {
            throw e;
        } catch (Exception e) {
            throw new CollaborationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IObjectEventPersistance
     * #dispose()
     */
    @Override
    public void dispose() throws CollaborationException {
        deleteResource(URI.create(sessionDataURL));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IObjectEventRetrieval
     * #retrieveEvent
     * (com.raytheon.uf.viz.collaboration.ui.role.event.IPersistedEvent)
     */
    @Override
    public AbstractDispatchingObjectEvent retrieveEvent(IPersistedEvent event)
            throws CollaborationException {
        if (event instanceof CollaborationHttpPersistedEvent) {
            CollaborationHttpPersistedObject object = retreiveStoredObject((CollaborationHttpPersistedEvent) event);
            if (object == null) {
                // No object available
                return null;
            } else if (object.event != null) {
                stats.log(object.event.getClass().getSimpleName(), 0,
                        object.dataSize);
                return object.event;
            } else {
                throw new CollaborationException(
                        "Unable to deserialize persisted event into event object");
            }
        } else if (event instanceof CollaborationWrappedEvent) {
            return ((CollaborationWrappedEvent) event).getEvent();
        } else {
            throw new CollaborationException(
                    "Unable to retreieve event for object: " + event);
        }
    }

    private CollaborationHttpPersistedObject retreiveStoredObject(
            CollaborationHttpPersistedEvent event)
            throws CollaborationException {
        String objectPath = event.getResourcePath();
        HttpGet get = new HttpGet(sessionDataURL + objectPath);
        HttpClientResponse response = executeRequest(get);
        if (isSuccess(response.code)) {
            try {
                CollaborationHttpPersistedObject dataObject = (CollaborationHttpPersistedObject) SerializationUtil
                        .transformFromThrift(CompressionUtil
                                .uncompress(response.data));
                if (dataObject != null) {
                    dataObject.dataSize = response.data.length;
                }
                return dataObject;
            } catch (SerializationException e) {
                throw new CollaborationException(e);
            }
        } else if (isNotExists(response.code)) {
            // Object was deleted
            return null;
        } else {
            throw new CollaborationException("Error retrieving object from "
                    + objectPath + " : " + new String(response.data));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IObjectEventRetrieval
     * #retrieveObjectEvents(int)
     */
    @Override
    public AbstractDispatchingObjectEvent[] retrieveObjectEvents(int objectId)
            throws CollaborationException {
        String objectPath = objectId + "/";
        HttpGet get = new HttpGet(sessionDataURL + objectPath);
        HttpClientResponse response = executeRequest(get);
        if (isSuccess(response.code) == false) {
            if (isNotExists(response.code)) {
                return new AbstractDispatchingObjectEvent[0];
            }
            throw new CollaborationException("Error retrieving object ("
                    + objectId + ") events, received code: " + response.code);
        }
        CollaborationHttpPersistedEvent event = new CollaborationHttpPersistedEvent();
        List<CollaborationHttpPersistedObject> objectEvents = new ArrayList<CollaborationHttpPersistedObject>();
        // parse out links of objects
        String htmlStr = new String(response.data);
        int searchIdx = 0;
        String searchStrStart = "<a href=\"";
        String objectEnding = ".obj";
        String searchStrEnd = "\">";
        int searchStrLen = searchStrStart.length();
        while (searchIdx > -1) {
            int previousIdx = searchIdx;
            int foundAt = htmlStr.indexOf(searchStrStart, searchIdx);
            // reset searchIdx to -1 until found
            searchIdx = -1;
            if (foundAt > previousIdx) {
                foundAt += searchStrLen;
                int endsAt = htmlStr.indexOf(searchStrEnd, foundAt);
                if (endsAt > foundAt) {
                    String object = htmlStr.substring(foundAt, endsAt);
                    if (object.endsWith(objectEnding)) {
                        event.setResourcePath(objectPath + object);
                        CollaborationHttpPersistedObject eventObject = retreiveStoredObject(event);
                        if (eventObject != null) {
                            objectEvents.add(eventObject);
                        } else {
                            // Object was deleted, abort
                            return new AbstractDispatchingObjectEvent[0];
                        }
                    }
                    searchIdx = endsAt + 1;
                }
            }
        }

        // Sort by creation time
        Collections.sort(objectEvents,
                new Comparator<CollaborationHttpPersistedObject>() {
                    @Override
                    public int compare(CollaborationHttpPersistedObject o1,
                            CollaborationHttpPersistedObject o2) {
                        return (int) (o1.persistTime - o2.persistTime);
                    }
                });

        AbstractDispatchingObjectEvent[] events = new AbstractDispatchingObjectEvent[objectEvents
                .size()];
        int i = 0;
        for (CollaborationHttpPersistedObject object : objectEvents) {
            events[i++] = object.event;
        }

        return events;
    }

    private void createFolder(String folderPath) throws CollaborationException {
        createFolder(URI.create(sessionDataURL + folderPath));
    }

    private void createFolder(URI folderPath) throws CollaborationException {
        HttpRequestBase mkcol = new HttpRequestBase() {
            @Override
            public String getMethod() {
                return "MKCOL";
            }
        };
        mkcol.setURI(folderPath);
        HttpClientResponse rsp = executeRequest(mkcol);
        if (isSuccess(rsp.code) == false) {
            throw new CollaborationException("Folder creation failed for "
                    + folderPath + ": " + new String(rsp.data));
        }
    }

    private void deleteResource(URI uri) throws CollaborationException {
        HttpClientResponse rsp = executeRequest(new HttpDelete(uri));
        // If request was success or resource doesn't exist, we are good
        if (isSuccess(rsp.code) == false && isNotExists(rsp.code) == false) {
            throw new CollaborationException("Folder creation failed for "
                    + uri + ": " + new String(rsp.data));
        }
    }

    private HttpClientResponse executeRequest(HttpUriRequest request)
            throws CollaborationException {
        try {
            return client.executeRequest(request);
        } catch (Exception e) {
            throw new CollaborationException(e);
        }
    }

    private boolean isSuccess(int code) {
        return code >= 200 && code < 300;
    }

    private boolean isNotExists(int code) {
        return code == 404 || code == 410;
    }

    @DynamicSerialize
    public static class CollaborationHttpPersistedObject {

        @DynamicSerializeElement
        private long persistTime;

        @DynamicSerializeElement
        private AbstractDispatchingObjectEvent event;

        private long dataSize;

        /**
         * @return the persistTime
         */
        public long getPersistTime() {
            return persistTime;
        }

        /**
         * @param persistTime
         *            the persistTime to set
         */
        public void setPersistTime(long persistTime) {
            this.persistTime = persistTime;
        }

        /**
         * @return the event
         */
        public AbstractDispatchingObjectEvent getEvent() {
            return event;
        }

        /**
         * @param event
         *            the event to set
         */
        public void setEvent(AbstractDispatchingObjectEvent event) {
            this.event = event;
        }

    }

    @DynamicSerialize
    public static class CollaborationHttpPersistedEvent implements
            IPersistedEvent {

        @DynamicSerializeElement
        private String resourcePath;

        /**
         * @return the resourceURL
         */
        public String getResourcePath() {
            return resourcePath;
        }

        /**
         * @param resourceURL
         *            the resourceURL to set
         */
        public void setResourcePath(String resourceURL) {
            this.resourcePath = resourceURL;
        }
    }

    @DynamicSerialize
    public static class CollaborationWrappedEvent implements IPersistedEvent {

        @DynamicSerializeElement
        private AbstractDispatchingObjectEvent event;

        /**
         * @return the event
         */
        public AbstractDispatchingObjectEvent getEvent() {
            return event;
        }

        /**
         * @param event
         *            the event to set
         */
        public void setEvent(AbstractDispatchingObjectEvent event) {
            this.event = event;
        }

    }
}
