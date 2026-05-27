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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryPackageType;

/**
 * Data Access object for interacting with registry object types in the registry
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------------------
 * Mar 13, 2013  1082     bphillip  Initial creation
 * Apr 14, 2021  7849     mapeters  Add {@link IDaoConfigFactory} constructor arg
 *
 * </pre>
 *
 * @author bphillip
 */
public class RegistryPackageDao
        extends RegistryObjectTypeDao<RegistryPackageType> {

    public RegistryPackageDao(IDaoConfigFactory daoConfigFactory) {
        super(daoConfigFactory);
    }

    @Override
    protected Class<RegistryPackageType> getEntityClass() {
        return RegistryPackageType.class;
    }

}
