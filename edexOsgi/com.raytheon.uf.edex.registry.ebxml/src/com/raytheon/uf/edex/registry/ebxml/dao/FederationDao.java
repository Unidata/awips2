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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Data access object for FederationType objects
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/21/2013    2022        bphillip    Initial implementation
 * 2/13/2014    2769       bphillip    Added read only flags to query methods
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class FederationDao extends RegistryObjectTypeDao<FederationType> {

    private static final String COUNT_QUERY = "select count(*) FROM FederationType";

    @Override
    protected Class<FederationType> getEntityClass() {
        return FederationType.class;
    }

    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public long getFederationCount() {
        return (Long) this.executeHQLQuery(COUNT_QUERY).get(0);
    }

    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public boolean federationsExist() {
        return getFederationCount() > 0;
    }

}
