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
package com.raytheon.openfire.plugin.configuration.collaboration.iq;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.tree.DefaultElement;
import org.jivesoftware.openfire.IQHandlerInfo;
import org.jivesoftware.openfire.PrivateStorage;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.auth.UnauthorizedException;
import org.jivesoftware.openfire.disco.ServerFeaturesProvider;
import org.jivesoftware.openfire.handler.IQHandler;
import org.jivesoftware.util.JiveGlobals;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;

/**
 * Base IQ handler for configuration over XMPP
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2014 2756       bclement     Initial creation
 * Feb 28, 2014 2756       bclement     reordered retrieve method operations for clarity
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractConfigHandler extends IQHandler implements
        ServerFeaturesProvider {

    public static final String DATASERVER_USERS_KEY = "plugin.collaboration.dataserver.users";

    public static final String QUERY_ELEMENT_NAME = "query";

    public static final String COLLAB_XMLNS = "urn:uf:viz:collaboration";

    private static final Logger log = LoggerFactory
            .getLogger(AbstractConfigHandler.class);

    protected final IQHandlerInfo info;

    protected final List<String> features;

    protected final PrivateStorage storage;


    /**
     * @param moduleName
     *            display name for handler
     * @param features
     *            xmlns strings for features
     * @param info
     *            qualified name registration information
     */
    public AbstractConfigHandler(String moduleName, List<String> features,
            IQHandlerInfo info) {
        super(moduleName);
        this.info = info;
        this.features = features;
        this.storage = XMPPServer.getInstance().getPrivateStorage();
    }

    /* (non-Javadoc)
     * @see org.jivesoftware.openfire.handler.IQHandler#getInfo()
     */
    @Override
    public IQHandlerInfo getInfo() {
        return info;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.openfire.handler.IQHandler#handleIQ(org.xmpp.packet.IQ)
     */
    @Override
    public IQ handleIQ(IQ packet) throws UnauthorizedException {
        Type type = packet.getType();
        IQ rval;
        switch (type) {
        case get:
            rval = handleGet(packet);
            break;
        case set:
            rval = handleSet(packet);
            break;
        default:
            rval = createError(packet, new PacketError(Condition.bad_request));
        }
        return rval;
    }

    /**
     * Process get request
     * 
     * @param packet
     *            request packet
     * @return
     * @throws UnauthorizedException
     */
    abstract protected IQ handleGet(IQ packet) throws UnauthorizedException;

    /**
     * Process set request
     * 
     * @param packet
     *            request packet
     * @return
     * @throws UnauthorizedException
     */
    abstract protected IQ handleSet(IQ packet) throws UnauthorizedException;

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.openfire.disco.ServerFeaturesProvider#getFeatures()
     */
    @Override
    public Iterator<String> getFeatures() {
        return features.iterator();
    }

    /**
     * Create error response packet
     * 
     * @param request
     * @param error
     * @return
     */
    protected static IQ createError(IQ request, PacketError error) {
        Element child = request.getChildElement();
        child.setParent(null);
        IQ rval = IQ.createResultIQ(request);
        rval.setType(Type.error);
        rval.setChildElement(child);
        rval.setError(error);
        return rval;
    }

    /**
     * Get first child of parent found with qualified name
     * 
     * @param parent
     * @param name
     * @param namespace
     * @return
     * @throws Exception
     *             if no child can be found
     */
    @SuppressWarnings("unchecked")
    protected static Element getChildElement(Element parent, String name,
            String namespace) throws Exception {
        Element rval = null;
        Iterator<Element> iter = parent.elementIterator();
        while (iter.hasNext()) {
            Element child = iter.next();
            if (child != null && child.getName().equals(name)
                    && child.getNamespace().getURI().equals(namespace)) {
                rval = child;
                break;
            }
        }
        if (rval == null) {
            log.debug("Missing Data Element: " + parent.asXML());
            String msg = "Missing Required Child Element: " + namespace + " "
                    + name;
            throw new Exception(msg);
        }
        return rval;
    }

    /**
     * Remove all child elements from e
     * 
     * @param e
     */
    @SuppressWarnings("unchecked")
    protected static void removeChildren(Element e) {
        Iterator<Element> iter = e.elementIterator();
        while (iter.hasNext()) {
            e.remove(iter.next());
        }
    }

    /**
     * @param id
     * @return true if id is in the list of dataserver users
     */
    protected static boolean isDataServerUser(JID id) {
        String usersStr = JiveGlobals.getProperty(DATASERVER_USERS_KEY, "");
        // this will work if the dataserver user is set to a single account or a
        // comma separated list or is blank
        for (String user : StringUtils.split(usersStr, ',')) {
            if (StringUtils.isBlank(user)) {
                continue;
            }
            user = user.trim();
            if (id.toBareJID().equals(user)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Return the primary dataserver which is the first item in the list of
     * dataserver users
     * 
     * @return null if none found
     */
    public static String getPrimaryDataServer() {
        String usersStr = JiveGlobals.getProperty(DATASERVER_USERS_KEY, "");
        String[] users = StringUtils.split(usersStr, ',');
        if (users.length < 1) {
            return null;
        }
        // primary server is the only one or first in list
        return users[0];
    }

    /**
     * Retrieve data from private storage
     * 
     * @param requestPacket
     *            request IQ for data
     * @param id
     *            user id that data is stored under
     * @param key
     *            empty xml element with qualified name
     * @return
     */
    protected IQ retrieve(IQ requestPacket, String id, Element key) {
        Element queryElem = requestPacket.getChildElement();
        IQ rval = IQ.createResultIQ(requestPacket);
        Element respElem = storage.get(id, key);
        respElem.setParent(null);

        removeChildren(queryElem);
        queryElem.setParent(null);
        rval.setChildElement(queryElem);
        queryElem.add(respElem);
        log.info(rval.toXML());
        return rval;
    }

    /**
     * Store to private data. NOTE: only child XML elements of the root data
     * element will be retrievable. No attributes in the root element will be
     * retrievable.
     * 
     * @param id
     *            user id to store data under
     * @param data
     *            xml element with data
     */
    protected void store(String id, Element data) {
        storage.add(id, data);
    }

    /**
     * Create empty xml element using qualified name
     * 
     * @param name
     * @param namespace
     * @return
     */
    protected static Element createElement(String name, String namespace) {
        return new DefaultElement(name, new Namespace(null, namespace));
    }

}
