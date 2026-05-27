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

import com.raytheon.uf.edex.database.dao.IDaoConfigFactory;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DynamicObjectRefType;

/**
 * Data access object for handling {@link DynamicObjectRefType}s
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------------
 * Aug 06, 2013  1693     bphillip  Initial creation
 * Apr 14, 2021  7849     mapeters  Add {@link IDaoConfigFactory} constructor arg
 *
 * </pre>
 *
 * @author mapeters
 */
public class DynamicObjectRefDao
        extends SessionManagedDao<String, DynamicObjectRefType> {

    public DynamicObjectRefDao(IDaoConfigFactory daoConfigFactory) {
        super(daoConfigFactory);
    }

    @Override
    protected Class<DynamicObjectRefType> getEntityClass() {
        return DynamicObjectRefType.class;
    }
}
