package com.raytheon.uf.common.registry.event;

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

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Registry event
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            jsanchez     Initial creation
 * Nov 08, 2013 2506       bgonzale     Added constructors.
 * Mar 31, 2014 2889       dhladky      Added username for notification center tracking.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@DynamicSerialize
public abstract class RegistryEvent extends Event {

    private static final long serialVersionUID = 987676907;

    public enum Action {
        UPDATE, DELETE, INSERT
    }

    /**
     * Default Constructor.
     */
    public RegistryEvent() {
    }

    /**
     * Initialization Constructor.
     * 
     * @param id
     * @param lid
     * @param objectType
     * @param action
     */
    public RegistryEvent(String id, String lid, String objectType, String username, Action action) {
        super(id);
        this.lid = lid;
        this.objectType = objectType;
        this.username = username;
        this.action = action;
    }

    @DynamicSerializeElement
    private String lid;

    @DynamicSerializeElement
    private String objectType;

    @DynamicSerializeElement
    private Action action;
    
    @DynamicSerializeElement
    private String username;

    public void setObjectType(String objectType) {
        this.objectType = objectType;
    }

    public String getObjectType() {
        return objectType;
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    @Override
    public String toString() {
        return "Registry Event: " + super.toString() + " Lid: " + lid
                + " Action: " + action + " objectType: " + objectType;
    }

    public Action getAction() {
        return action;
    }

    public void setAction(Action action) {
        this.action = action;
    }
    
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

}
