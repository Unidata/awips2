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

package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1802       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class AuditableEventTypeDao extends
        RegistryObjectTypeDao<AuditableEventType> {

    /**
     * Get events of interest for registry subscriptions
     * 
     * @param objectsOfInterest
     * @return
     * @throws EbxmlRegistryException
     */
    public List<AuditableEventType> getEventsOfInterest(
            List<RegistryObjectType> objectsOfInterest)
            throws EbxmlRegistryException {
        String query = "from AuditableEventType event inner join event.action as action inner join action.affectedObjects as AffectedObjects inner join AffectedObjects.registryObject as RegistryObjects where RegistryObjects.id = '";
        for (int i = 0; i < objectsOfInterest.size(); i++) {
            query += "'" + objectsOfInterest.get(i).getId() + "'";
            if (i != objectsOfInterest.size() - 1) {
                query += ",";
            }
        }
        query += "'";

        try {
            return this.executeHQLQuery(query);
        } catch (DataAccessLayerException e) {
            throw new EbxmlRegistryException("Data Access Error", e);
        }
    }

    @Override
    protected Class<AuditableEventType> getEntityClass() {
        return AuditableEventType.class;
    }

}
