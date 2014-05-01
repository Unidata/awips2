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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;

import com.raytheon.uf.edex.database.dao.SessionManagedDao;

/**
 * <pre>
 * 
 * Data access object for interactions with ExtensibleObjectType objects
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
public class ExtensibleObjectTypeDao<ENTITY extends ExtensibleObjectType>
        extends SessionManagedDao<String, ENTITY> {

    @SuppressWarnings("unchecked")
    @Override
    protected Class<ENTITY> getEntityClass() {
        return (Class<ENTITY>) ExtensibleObjectType.class;
    }

}
