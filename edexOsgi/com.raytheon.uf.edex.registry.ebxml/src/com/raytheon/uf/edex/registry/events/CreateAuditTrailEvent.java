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
package com.raytheon.uf.edex.registry.events;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import com.raytheon.uf.common.event.Event;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/2/2013    1829        bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class CreateAuditTrailEvent extends Event {

    private static final long serialVersionUID = 8587255848647808543L;

    private RegistryRequestType request;

    private String actionType;

    private List<RegistryObjectType> objectsAffected;

    public CreateAuditTrailEvent(String id, RegistryRequestType request,
            String actionType, List<RegistryObjectType> objectsAffected) {
        super(id);
        this.request = request;
        this.actionType = actionType;
        this.objectsAffected = objectsAffected;
    }

    public RegistryRequestType getRequest() {
        return request;
    }

    public String getActionType() {
        return actionType;
    }

    public List<RegistryObjectType> getObjectsAffected() {
        return objectsAffected;
    }

}
