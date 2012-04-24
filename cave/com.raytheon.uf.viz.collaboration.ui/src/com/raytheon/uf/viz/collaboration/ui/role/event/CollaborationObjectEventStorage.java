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
package com.raytheon.uf.viz.collaboration.ui.role.event;

import java.net.URI;

import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.entity.ByteArrayEntity;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
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

    private volatile long EVENT_ID_COUNTER = 0;

    public static IObjectEventPersistance createPersistanceObject(
            ISharedDisplaySession session) throws CollaborationException {
        CollaborationObjectEventStorage persistance = new CollaborationObjectEventStorage(
                session);
        persistance.setupStorage();
        return persistance;
    }

    public static IObjectEventRetrieval createRetrievalObject(
            ISharedDisplaySession session) {
        return new CollaborationObjectEventStorage(session);
    }

    private ISharedDisplaySession session;

    // TODO: Get Collaboration server to build URL
    private String baseUrl = "http://edexproxy:9582/collab/";

    private HttpClient client;

    private CollaborationObjectEventStorage(ISharedDisplaySession session) {
        this.session = session;
        this.client = HttpClient.getInstance();
    }

    private void setupStorage() throws CollaborationException {
        createFolder(session.getSessionId());
        // Successful creation of session folder, add to base url
        baseUrl += session.getSessionId() + "/";
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
            deleteResource(URI.create(baseUrl + event.getObjectId() + "/"));
            CollaborationWrappedEvent wrapped = new CollaborationWrappedEvent();
            wrapped.setEvent(event);
            return wrapped;
        }

        try {
            CollaborationHttpPersistedEvent wrapped = new CollaborationHttpPersistedEvent();
            String eventObjectURL = baseUrl + event.getObjectId() + "/"
                    + (++EVENT_ID_COUNTER) + ".obj";
            HttpPut put = new HttpPut(eventObjectURL);
            put.setEntity(new ByteArrayEntity(Tools.compress(SerializationUtil
                    .transformToThrift(event))));
            HttpClientResponse response = executeRequest(put);
            if (response.code != 201) {
                throw new CollaborationException(
                        "Error uploading event object to server @ "
                                + eventObjectURL + " : "
                                + new String(response.data));
            }
            wrapped.setResourceURL(eventObjectURL);
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
        deleteResource(URI.create(baseUrl));
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
            String objectURL = ((CollaborationHttpPersistedEvent) event)
                    .getResourceURL();
            HttpGet get = new HttpGet(objectURL);
            HttpClientResponse response = executeRequest(get);
            if (response.code == 200) {
                try {
                    return (AbstractDispatchingObjectEvent) SerializationUtil
                            .transformFromThrift(Tools
                                    .uncompress(response.data));
                } catch (SerializationException e) {
                    throw new CollaborationException(e);
                }
            } else {
                throw new CollaborationException(
                        "Error retrieving object from url " + objectURL + " : "
                                + new String(response.data));
            }
        } else if (event instanceof CollaborationWrappedEvent) {
            return ((CollaborationWrappedEvent) event).getEvent();
        } else {
            throw new CollaborationException(
                    "Unable to retreieve event for object: " + event);
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
        // TODO: Retrieve all events for the given objectId
        throw new CollaborationException(
                "Retrieving objects for creation is not supported yet!");
    }

    private void createFolder(String folderPath) throws CollaborationException {
        HttpRequestBase mkcol = new HttpRequestBase() {
            @Override
            public String getMethod() {
                return "MKCOL";
            }
        };
        mkcol.setURI(URI.create(baseUrl + folderPath));
        HttpClientResponse rsp = executeRequest(mkcol);
        if (rsp.code != 201) {
            throw new CollaborationException("Folder creation failed for "
                    + folderPath + ": " + new String(rsp.data));
        }
    }

    private void deleteResource(URI uri) throws CollaborationException {
        HttpClientResponse rsp = executeRequest(new HttpDelete(uri));
        // Valid DELETE return codes are 200, 202, and 204
        if (rsp.code != 200 && rsp.code != 202 && rsp.code != 204) {
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

    @DynamicSerialize
    public static class CollaborationHttpPersistedEvent implements
            IPersistedEvent {

        @DynamicSerializeElement
        private String resourceURL;

        /**
         * @return the resourceURL
         */
        public String getResourceURL() {
            return resourceURL;
        }

        /**
         * @param resourceURL
         *            the resourceURL to set
         */
        public void setResourceURL(String resourceURL) {
            this.resourceURL = resourceURL;
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
