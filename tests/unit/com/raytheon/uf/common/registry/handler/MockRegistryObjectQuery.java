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
package com.raytheon.uf.common.registry.handler;

import com.raytheon.uf.common.registry.MockRegistryObject;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012            djohnson     Initial creation
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0	
 */
public class MockRegistryObjectQuery extends
        AdhocRegistryQuery<MockRegistryObject> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<?> getObjectType() {
        return MockRegistryObject.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<MockRegistryObject> getResultType() {
        return MockRegistryObject.class;
    }

}
