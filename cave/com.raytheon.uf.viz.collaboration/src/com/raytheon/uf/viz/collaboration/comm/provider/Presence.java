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
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class Presence implements IPresence {

    private static Map<org.eclipse.ecf.presence.IPresence.Type,IPresence.Type> TYPE_MAP = new HashMap<org.eclipse.ecf.presence.IPresence.Type,IPresence.Type>();
    static {
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.AVAILABLE, IPresence.Type.AVAILABLE);
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.ERROR, IPresence.Type.ERROR);
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.SUBSCRIBE, IPresence.Type.SUBSCRIBE);
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.SUBSCRIBED, IPresence.Type.SUBSCRIBED);
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.UNAVAILABLE, IPresence.Type.UNAVAILABLE);
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.UNSUBSCRIBE, IPresence.Type.UNSUBSCRIBE);
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.UNSUBSCRIBED, IPresence.Type.UNSUBSCRIBED);
        TYPE_MAP.put(org.eclipse.ecf.presence.IPresence.Type.UNKNOWN, IPresence.Type.UNKNOWN);
    }
    
    private static Map<org.eclipse.ecf.presence.IPresence.Mode,IPresence.Mode> MODE_MAP = new HashMap<org.eclipse.ecf.presence.IPresence.Mode,IPresence.Mode>();
    static {
        MODE_MAP.put(org.eclipse.ecf.presence.IPresence.Mode.AVAILABLE, IPresence.Mode.AVAILABLE);
        MODE_MAP.put(org.eclipse.ecf.presence.IPresence.Mode.AWAY, IPresence.Mode.AWAY);
        MODE_MAP.put(org.eclipse.ecf.presence.IPresence.Mode.CHAT, IPresence.Mode.CHAT);
        MODE_MAP.put(org.eclipse.ecf.presence.IPresence.Mode.DND, IPresence.Mode.DND);
        MODE_MAP.put(org.eclipse.ecf.presence.IPresence.Mode.EXTENDED_AWAY, IPresence.Mode.EXTENDED_AWAY);
        MODE_MAP.put(org.eclipse.ecf.presence.IPresence.Mode.INVISIBLE, IPresence.Mode.INVISIBLE);
    }

    private Map<String,Property> properties = null;
    
    private Mode mode;
    
    private Type type;
    
    /**
     * 
     */
    public Presence() {
        mode = Mode.AVAILABLE;
        type = Type.AVAILABLE;
    }

    /**
     * 
     */
    @Override
    public Mode getMode() {
        return mode;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IPresence#setMode(com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Mode)
     */
    @Override
    public void setMode(Mode mode) {
        this.mode = mode;
    }

    /**
     * 
     */
    @Override
    public Type getType() {
        return type;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IPresence#setType(com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Type)
     */
    @Override
    public void setType(Type type) {
        this.type = type;
    }

    private void ensureProperties() {
        if(properties == null) {
            properties = new HashMap<String,Property>();
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#setProperty(java.lang.String, java.lang.String)
     */
    @Override
    public void setProperty(String key, String value) {
        ensureProperties();
        properties.put(key, new Property(key,value));
    }
    
    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getProperty(java.lang.String, java.lang.String)
     */
    @Override
    public String getProperty(String key, String defaultValue) {
        String retValue = defaultValue;
        if(properties != null) {
            Property property = properties.get(key);
            retValue = (property != null) ? property.getValue() : defaultValue;
        }
        return retValue;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getProperties()
     */
    @Override
    public Collection<Property> getProperties() {
        return properties.values();
    }
    
    /**
     * 
     * @param presence
     * @return
     */
    public static IPresence convertPresence(org.eclipse.ecf.presence.IPresence presence) {
        IPresence newPresence = null;
        if(presence != null) {
            newPresence = new Presence();
            
            
            newPresence.setType(TYPE_MAP.get(presence.getType()));
            newPresence.setMode(MODE_MAP.get(presence.getMode()));
            
            System.out.println(presence.getStatus());
        }
        return newPresence;
    }

    
    /**
     * 
     * @param presence
     * @return
     */
    public static org.eclipse.ecf.presence.IPresence convertPresence(IPresence presence) {
        org.eclipse.ecf.presence.IPresence newPresence = null;
        if(presence != null) {
            
        
        }
        return newPresence;
    }
    
    
    
    
    
    
    
}
