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

import org.springframework.transaction.TransactionDefinition;

import com.raytheon.uf.edex.database.dao.IDaoConfigFactory;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;

/**
 *
 * Data access object for RegistryType objects
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------------------------------
 * May 21, 2013  2022     bphillip  Initial implementation
 * Apr 14, 2021  7849     mapeters  Refactor to use TransactionTemplate.execute instead of
 *                                  Transactional annotations
 *
 * </pre>
 *
 * @author bphillip
 */
public class RegistryDao extends RegistryObjectTypeDao<RegistryType> {

    private static final String QUERY_BY_BASE_URL = "FROM RegistryType reg where reg.baseURL=:baseURL";

    public RegistryDao(IDaoConfigFactory daoConfigFactory) {
        super(daoConfigFactory);
    }

    public RegistryType getRegistryByBaseURL(String baseURL) {
        TransactionDefinition transactionDef = getTransactionDef(true);
        return supplyInTransaction(transactionDef, () -> {
            return this.uniqueResult(QUERY_BY_BASE_URL, "baseURL", baseURL);
        });
    }

    @Override
    protected Class<RegistryType> getEntityClass() {
        return RegistryType.class;
    }

}
