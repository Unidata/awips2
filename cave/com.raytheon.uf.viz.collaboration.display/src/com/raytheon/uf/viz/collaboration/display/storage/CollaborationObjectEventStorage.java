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
package com.raytheon.uf.viz.collaboration.display.storage;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.message.BasicHeader;
import org.apache.http.protocol.HTTP;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.comm.NetworkStatistics;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.compression.CompressionUtil;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.provider.session.ClientAuthManager;
import com.raytheon.uf.viz.collaboration.comm.provider.session.PeerToPeerCommHelper;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;

/**
 * Class responsible for object event storage. Will persist/retrieve objects.
 * When listing remote directories, this client uses the HTTP Accepts header to
 * prefer XML format, but also accepts HTML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2012            mschenke     Initial creation
 * Feb 17, 2014 2756       bclement     added xml parsing for HTTP directory listing
 * Feb 24, 2014 2751       bclement     added separate paths for each provider under session id
 * Feb 25, 2014 2751       bclement     fixed provider id path for webDAV
 * Feb 28, 2014 2756       bclement     added auth, moved response code checks to response object
 * Mar 06, 2014 2826       njensen      Fix spelling mistake
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationObjectEventStorage implements
        IObjectEventPersistance, IObjectEventRetrieval {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(CollaborationObjectEventStorage.class);

    private static NetworkStatistics stats = com.raytheon.uf.viz.collaboration.comm.Activator
            .getDefault().getNetworkStats();

    private static final String SLASH = "/";

    public static IObjectEventPersistance createPersistanceObject(
            ISharedDisplaySession session, Dispatcher dispatcher)
            throws CollaborationException {
        CollaborationObjectEventStorage persistance = new CollaborationObjectEventStorage(
                session, dispatcher.getDispatcherId());
        persistance.createFolder(URI.create(persistance.sessionDataURL));
        appendProviderIdToPath(persistance);
        persistance.createFolder(URI.create(persistance.sessionDataURL));
        persistance.sessionDataURL += persistance.displayId + SLASH;
        persistance.createFolder(URI.create(persistance.sessionDataURL));
        return persistance;
    }

    /**
     * appends URL encoded provider id to persistence URL path
     * 
     * @param persistance
     */
    @SuppressWarnings("deprecation")
    private static void appendProviderIdToPath(
            CollaborationObjectEventStorage persistance) {
        try {
            persistance.sessionDataURL += URLEncoder.encode(
                    persistance.providerid, HTTP.UTF_8) + SLASH;
        } catch (UnsupportedEncodingException e) {
            log.warn("URL encoding failed, retrying with default encoding: "
                    + e.getLocalizedMessage());
            // unlikely that utf8 isn't supported, but just go again with the
            // default encoding
            persistance.sessionDataURL += URLEncoder
                    .encode(persistance.providerid);
        }
    }

    public static IObjectEventRetrieval createRetrievalObject(
            ISharedDisplaySession session, int displayId) {
        CollaborationObjectEventStorage persistance = new CollaborationObjectEventStorage(
                session, displayId);
        appendProviderIdToPath(persistance);
        persistance.sessionDataURL += persistance.displayId + SLASH;
        return persistance;
    }

    public static final String ACCEPTS_HEADER = "Accepts";

    public static final String XML_CONTENT_TYPE = "text/xml";

    public static final String HTML_CONTENT_TYPE = "text/html";

    private String sessionDataURL;

    private HttpClient client;

    private int displayId;

    private final XMLInputFactory staxFactory = XMLInputFactory.newInstance();

    private final String providerid;

    private final ClientAuthManager authManager;

    private CollaborationObjectEventStorage(ISharedDisplaySession session,
            int displayId) {
        this.displayId = displayId;
        this.client = HttpClient.getInstance();
        this.authManager = session.getConnection().getAuthManager();
        VenueParticipant dataProvider = session.getCurrentDataProvider();
        this.providerid = dataProvider.getUserid().getNormalizedId();
        this.sessionDataURL = PeerToPeerCommHelper.getCollaborationHttpServer();
        this.sessionDataURL += session.getSessionId() + SLASH;
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
            // TODO this is for pre 14.3 compatibility
            createFolder(String.valueOf(event.getObjectId()));
        } else if (event instanceof DisposeObjectEvent) {
            // Do not delete anything off the server, users may still be
            // retrieving data off the server if they have not yet processed all
            // the messages and everything gets deleted when the session is
            // closed anyway.
            // deleteResource(URI.create(sessionDataURL + event.getObjectId()
            // + "/"));
            CollaborationWrappedEvent wrapped = new CollaborationWrappedEvent();
            wrapped.setDisplayId(displayId);
            wrapped.setEvent(event);
            return wrapped;
        }

        try {
            CollaborationHttpPersistedEvent wrapped = new CollaborationHttpPersistedEvent();
            String objectPath = event.getObjectId() + SLASH
                    + event.getClass().getName() + ".obj";
            String eventObjectURL = sessionDataURL + objectPath;

            CollaborationHttpPersistedObject persistObject = new CollaborationHttpPersistedObject();
            persistObject.persistTime = System.currentTimeMillis();
            persistObject.event = event;
            byte[] toPersist = CompressionUtil.compress(SerializationUtil
                    .transformToThrift(persistObject));
            stats.log(event.getClass().getSimpleName(), toPersist.length, 0);
            HttpPut put = createPut(eventObjectURL, toPersist);
            HttpClientResponse response = executeRequest(put);
            if (response.isSuccess() == false) {
                throw new CollaborationException(
                        "Error uploading event object (" + event.getObjectId()
                                + ") to server @ " + eventObjectURL + " : "
                                + new String(response.data));
            }
            wrapped.setDisplayId(displayId);
            wrapped.setResourcePath(objectPath);
            return wrapped;
        } catch (CollaborationException e) {
            throw e;
        } catch (Exception e) {
            throw new CollaborationException(e);
        }
    }

    /**
     * Create http put method object. Adds auth header if needed.
     * 
     * @param url
     * @param body
     * @return
     * @throws CollaborationException
     */
    private HttpPut createPut(String url, byte[] body)
            throws CollaborationException {
        URI uri = URI.create(url);
        HttpPut put = new HttpPut(uri);
        authManager.signRequest(put, providerid, uri, body);
        put.setEntity(new ByteArrayEntity(body));
        return put;
    }

    /**
     * Create http delete method object. Adds auth header if needed.
     * 
     * @param uri
     * @return
     * @throws CollaborationException
     */
    private HttpDelete createDelete(URI uri) throws CollaborationException {
        HttpDelete delete = new HttpDelete(uri);
        authManager.signRequest(delete, providerid, uri);
        return delete;
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
            CollaborationHttpPersistedObject object = retrieveStoredObject((CollaborationHttpPersistedEvent) event);
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

    /**
     * Execute get request for object stored at event's resource path
     * 
     * @param event
     * @return null if object was deleted
     * @throws CollaborationException
     */
    private CollaborationHttpPersistedObject retrieveStoredObject(
            CollaborationHttpPersistedEvent event)
            throws CollaborationException {
        String objectPath = event.getResourcePath();
        HttpGet get = new HttpGet(sessionDataURL + objectPath);
        HttpClientResponse response = executeRequest(get);
        if (response.isSuccess()) {
            try {
                CollaborationHttpPersistedObject dataObject = SerializationUtil
                        .transformFromThrift(
                                CollaborationHttpPersistedObject.class,
                                CompressionUtil.uncompress(response.data));
                if (dataObject != null) {
                    dataObject.dataSize = response.data.length;
                }
                return dataObject;
            } catch (SerializationException e) {
                throw new CollaborationException(e);
            }
        } else if (response.isNotExists()) {
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
        get.addHeader(new BasicHeader(ACCEPTS_HEADER, XML_CONTENT_TYPE + ","
                + HTML_CONTENT_TYPE));
        HttpClientResponse response = executeRequest(get);
        if (response.isSuccess() == false) {
            if (response.isNotExists()) {
                return new AbstractDispatchingObjectEvent[0];
            }
            throw new CollaborationException("Error retrieving object ("
                    + objectId + ") events, received code: " + response.code);
        }
        // parse out links of objects
        List<CollaborationHttpPersistedObject> objectEvents;
        try {
            if (response.contentType != null
                    && response.contentType.toLowerCase().contains("xml")) {
                objectEvents = parseXmlResponseData(objectPath, response.data);
            } else {
                objectEvents = parseHtmlResponseData(objectPath, response.data);
            }
        } catch (XMLStreamException e) {
            throw new CollaborationException("Error parsing response data", e);
        }
        if (objectEvents == null) {
            // Object was deleted, abort
            return new AbstractDispatchingObjectEvent[0];
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

    /**
     * Parse response from HTTP directory listing in xml format
     * 
     * @param objectPath
     * @param responseData
     * @return null if object was deleted
     * @throws XMLStreamException
     * @throws CollaborationException
     */
    private List<CollaborationHttpPersistedObject> parseXmlResponseData(
            String objectPath, byte[] responseData) throws XMLStreamException,
            CollaborationException {

        CollaborationHttpPersistedEvent event = new CollaborationHttpPersistedEvent();
        List<CollaborationHttpPersistedObject> objectEvents = new ArrayList<CollaborationHttpPersistedObject>();
        String text;
        boolean inFileTag = false;
        XMLStreamReader reader = staxFactory
                .createXMLStreamReader(new ByteArrayInputStream(responseData));
        while (reader.hasNext()) {
            int staxEvent = reader.next();
            switch (staxEvent) {
            case XMLStreamConstants.START_ELEMENT:
                String localName = reader.getLocalName();
                inFileTag = localName.equalsIgnoreCase("file");
                break;
            case XMLStreamConstants.CHARACTERS:
                text = reader.getText().trim();
                if (inFileTag && text.endsWith(".obj")) {
                    event.setResourcePath(objectPath + text);
                    CollaborationHttpPersistedObject eventObject = retrieveStoredObject(event);
                    if (eventObject != null) {
                        objectEvents.add(eventObject);
                    } else {
                        // Object was deleted, abort
                        return null;
                    }
                }
            }
        }
        return objectEvents;
    }

    /**
     * Parse httpd HTML directory listing response
     * 
     * @param objectPath
     * @param responseData
     * @return null if object was deleted
     * @throws XMLStreamException
     * @throws CollaborationException
     */
    private List<CollaborationHttpPersistedObject> parseHtmlResponseData(
            String objectPath, byte[] responseData) throws XMLStreamException,
            CollaborationException {
        CollaborationHttpPersistedEvent event = new CollaborationHttpPersistedEvent();
        List<CollaborationHttpPersistedObject> objectEvents = new ArrayList<CollaborationHttpPersistedObject>();
        String htmlStr = new String(responseData);
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
                        CollaborationHttpPersistedObject eventObject = retrieveStoredObject(event);
                        if (eventObject != null) {
                            objectEvents.add(eventObject);
                        } else {
                            // Object was deleted, abort
                            return null;
                        }
                    }
                    searchIdx = endsAt + 1;
                }
            }
        }
        return objectEvents;
    }

    /**
     * Perform a MKCOL operation on httpd with DAV module.
     * 
     * @deprecated MKCOL is only required for DAV (pre 14.3)
     * 
     * @param folderPath
     * @throws CollaborationException
     */
    @Deprecated
    private void createFolder(String folderPath) throws CollaborationException {
        createFolder(URI.create(sessionDataURL + folderPath));
    }

    /**
     * Perform a MKCOL operation on httpd with DAV module.
     * 
     * @deprecated MKCOL is only required for DAV (pre 14.3)
     * 
     * @param folderPath
     * @throws CollaborationException
     */
    @Deprecated
    private void createFolder(URI folderPath) throws CollaborationException {
        HttpRequestBase mkcol = new HttpRequestBase() {
            @Override
            public String getMethod() {
                return "MKCOL";
            }
        };
        mkcol.setURI(folderPath);
        HttpClientResponse rsp = executeRequest(mkcol);
        if (rsp.isSuccess() == false && isDirExists(rsp.code) == false) {
            throw new CollaborationException("Folder creation failed for "
                    + folderPath + ": " + new String(rsp.data));
        }
    }

    /**
     * Delete all files at and below uri
     * 
     * @param uri
     * @throws CollaborationException
     */
    private void deleteResource(URI uri) throws CollaborationException {
        HttpClientResponse rsp = executeRequest(createDelete(uri));
        // If request was success or resource doesn't exist, we are good
        if (rsp.isSuccess() == false && rsp.isNotExists() == false) {
            throw new CollaborationException("Folder creation failed for "
                    + uri + ": " + new String(rsp.data));
        }
    }

    /**
     * Execute HTTP request
     * 
     * @param request
     * @return
     * @throws CollaborationException
     */
    private HttpClientResponse executeRequest(HttpUriRequest request)
            throws CollaborationException {
        try {
            return client.executeRequest(request);
        } catch (Exception e) {
            throw new CollaborationException(e);
        }
    }

    /**
     * @deprecated this error is related to MKCOL. MKCOL is only required for
     *             DAV (pre 14.3)
     * 
     * @param code
     * @return true if directory alread exists on server
     */
    @Deprecated
    private boolean isDirExists(int code) {
        return code == 405 || code == 301;
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
        private int displayId;

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

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.
         * IPersistedEvent#getDisplayId()
         */
        @Override
        public int getDisplayId() {
            return displayId;
        }

        /**
         * @param displayId
         *            the displayId to set
         */
        public void setDisplayId(int displayId) {
            this.displayId = displayId;
        }

    }

    @DynamicSerialize
    public static class CollaborationWrappedEvent implements IPersistedEvent {

        @DynamicSerializeElement
        private int displayId;

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

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.
         * IPersistedEvent#getDisplayId()
         */
        @Override
        public int getDisplayId() {
            return displayId;
        }

        /**
         * @param displayId
         *            the displayId to set
         */
        public void setDisplayId(int displayId) {
            this.displayId = displayId;
        }

    }
}
