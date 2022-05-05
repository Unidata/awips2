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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Registry Event for removing objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            jsanchez     Initial creation
 * Nov 08, 2013 2506       bgonzale     Added constructors.  Added object deleted field.
 *                                      Added RegistryObjectType field.
 * Mar 31, 2014 2889       dhladky      Added username for notification center tracking.
 * Apr 05, 2016 5488       tjensen      Added DynamicSerializeElement to removedObject
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@DynamicSerialize
public class RemoveRegistryEvent extends RegistryEvent {

    private static final long serialVersionUID = -5854149256576746509L;

    @DynamicSerializeElement
    private RegistryObjectType removedObject;

    public RemoveRegistryEvent() {

    }

    public RemoveRegistryEvent(String username, String id,
            RegistryObjectType removedObject) {
        super(id, null, null, username, Action.DELETE);
        this.removedObject = removedObject;
    }

    /**
     * @return the removedObject
     */
    public RegistryObjectType getRemovedObject() {
        return removedObject;
    }

    /**
     * @param removedObject
     *            the removedObject to set
     */
    public void setRemovedObject(RegistryObjectType removedObject) {
        this.removedObject = removedObject;
    }

    @Override
    public String toString() {
        return "Remove " + super.toString();
    }

}
