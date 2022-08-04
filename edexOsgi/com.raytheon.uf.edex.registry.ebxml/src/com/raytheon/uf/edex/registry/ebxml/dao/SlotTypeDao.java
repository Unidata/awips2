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

import java.util.Collection;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import org.hibernate.SQLQuery;
import org.hibernate.StaleStateException;
import org.hibernate.criterion.Property;

import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * 
 * Data Access object for interacting with slot objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/11/2013    1707        bphillip    Initial implementation
 * 7/29/2013    2191        bphillip    Modified method to get orphaned slots
 * 12/2/2013    1829        bphillip    Changed how orphans are purged
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * Nov 18, 2015  5006       dhladky     Deletion of duplicate slots causes errors.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SlotTypeDao extends SessionManagedDao<String, SlotType> {

    @Override
    protected Class<SlotType> getEntityClass() {
        return SlotType.class;
    }

    /**
     * Gets orphaned slot ids
     * 
     * @param limit
     *            The maximum number of results to return
     * @return List of orphaned ids of size limit
     */
    @SuppressWarnings("unchecked")
    public void purgeOrphans() {
        SQLQuery query = this.getSessionFactory().getCurrentSession()
                .createSQLQuery("select id, parent_id FROM ebxml.slot");
        List<Object[]> results = query.list();

        for (Object[] result : results) {
            String slotId = (String) result[0];
            String parentId = (String) result[1];
            statusHandler.info("Checking [" + slotId + "]");
            if (this.executeHQLQuery(
                    "FROM ExtensibleObjectType obj where obj.id=:id", "id",
                    parentId).isEmpty()) {
                deleteBySlotId(slotId);
            }
        }
    }

    /**
     * Delete an individual slot by ID
     * @param id
     */
    public void deleteBySlotId(String id) {
        SlotType slot = this.getById(id);
        if (slot != null) {
            try {
                getCurrentSession().delete(slot);
            } catch (StaleStateException sse) {
                statusHandler.warn("Registry Object to delete: " + id
                        + " no longer exists in registry.");
            }
        }
    }

    /**
     * Deletes a list of registry objects (SLOTS) associated with given ids.
     * 
     * @param ids
     */
    @SuppressWarnings("unchecked")
    public void deleteBySlotId(Collection<String> ids) {

        List<SlotType> objs = createCriteria()
                .add(Property.forName(QueryConstants.ID).in(ids)).list();
     
        for (SlotType slot: objs) {
            
            try {
                getCurrentSession().delete(slot);
            } catch (StaleStateException sse) {
                statusHandler.warn("Registry Object to delete: " + slot.getId()
                        + " no longer exists in registry.");
            }
        }
    }
}
