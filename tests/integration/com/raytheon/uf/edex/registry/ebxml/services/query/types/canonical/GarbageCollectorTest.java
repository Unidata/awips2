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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.GarbageCollectorQueryPlugin;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical GarbageCollector query defined by the EBXML 4.0 spec
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2013    1682        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GarbageCollectorTest extends QueryTest {

    @Autowired
    private GarbageCollectorQueryPlugin garbageCollector;

    @Before
    public void insertTestObjects() throws MsgRegistryException {

        List<RegistryObjectType> objsToSubmit = new ArrayList<RegistryObjectType>();
        for (int i = 0; i < 5; i++) {
            RegistryObjectType obj = new RegistryObjectType("Source Object "
                    + i, "Source Object " + i);
            objsToSubmit.add(obj);
        }

        for (int i = 1; i < 4; i++) {
            RegistryObjectType obj = new RegistryObjectType("Target Object "
                    + i, "Target Object " + i);
            objsToSubmit.add(obj);
        }

        for (int i = 0; i < 5; i++) {
            AssociationType association = new AssociationType();
            association.setId("Affiliated With Association " + i);
            association.setLid(association.getId());
            association.setObjectType(RegistryObjectTypes.ASSOCIATION);
            association.setType(AssociationTypes.AFFILIATED_WITH);
            association.setSourceObject("Source Object " + i);
            association.setTargetObject("Target Object " + (i + 1));
            objsToSubmit.add(association);
        }
        submitRegistryObjectsToRegistry(objsToSubmit);
    }

    @Test
    public void testGarbageCollect() throws MsgRegistryException {

        QueryRequest query = createQuery(CanonicalQueryTypes.GARBAGE_COLLECTOR);

        List<RegistryObjectType> result = executeQuery(garbageCollector, query);
        assertEquals(2, result.size());

    }
}
