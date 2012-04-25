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

    private String sessionDataURL;

    private HttpClient client;

    private CollaborationObjectEventStorage(ISharedDisplaySession session) {
        this.session = session;
        this.client = HttpClient.getInstance();
    }

    private void setupStorage() throws CollaborationException {
        String collaborationServer = Activator.getDefault()
                .getPreferenceStore().getString(CollabPrefConstants.P_SERVER);
        if (collaborationServer != null) {
            sessionDataURL = String.format(SESSION_DATA_URL_FORMAT_STRING,
                    collaborationServer, SESSION_DATA_PORT);
            createFolder(session.getSessionId());
            // Successful creation of session folder, add to base url
            sessionDataURL += session.getSessionId() + "/";
        } else {
            throw new CollaborationException(
                    "Could not retrieve collaboration server from preferences");
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
            String eventObjectURL = sessionDataURL + event.getObjectId() + "/"
                    + event.getClass().getName() + ".obj";
            HttpPut put = new HttpPut(eventObjectURL);

            put.setEntity(new ByteArrayEntity(Tools.compress(SerializationUtil
                    .transformToThrift(event))));
            HttpClientResponse response = executeRequest(put);
            if (isSuccess(response.code) == false) {
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
            String objectURL = ((CollaborationHttpPersistedEvent) event)
                    .getResourceURL();
            HttpGet get = new HttpGet(objectURL);
            HttpClientResponse response = executeRequest(get);
            if (isSuccess(response.code)) {
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
        mkcol.setURI(URI.create(sessionDataURL + folderPath));
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
