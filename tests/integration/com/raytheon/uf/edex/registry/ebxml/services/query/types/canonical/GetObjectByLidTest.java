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

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.GetObjectsByLid;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical GetObjectByLid query defined by the EBXML 4.0 spec
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
public class GetObjectByLidTest extends QueryTest {

    @Autowired
    private GetObjectsByLid getObjectsByLid;

    @Before
    public void insertTestObjects() throws MsgRegistryException {
        for (int i = 0; i < 11; i++) {
            RegistryObjectType obj = new RegistryObjectType(
                    "GetByLid Test Object " + i, "GetByLid Test Object " + i);
            submitRegistryObjectToRegistry(obj);
        }

    }

    @Test
    public void getObjectByLid() throws MsgRegistryException,
            MsgRegistryException {
        List<RegistryObjectType> result = executeQuery(
                getObjectsByLid,
                createQuery(CanonicalQueryTypes.GET_OBJECTS_BY_LID, "lid",
                        "GetByLid Test Object 1"));
        assertTrue(result.size() == 1);
        assertTrue(result.get(0).getLid().equals("GetByLid Test Object 1"));
    }

    @Test
    public void getObjectByLidUsingWildcards() throws MsgRegistryException {
        List<RegistryObjectType> result = executeQuery(
                getObjectsByLid,
                createQuery(CanonicalQueryTypes.GET_OBJECTS_BY_LID, "lid",
                        "GetByLid Test Object %"));
        assertTrue(result.size() == 11);
        result = executeQuery(
                getObjectsByLid,
                createQuery(CanonicalQueryTypes.GET_OBJECTS_BY_LID, "lid",
                        "GetByLid Test Object ??"));
        assertTrue(result.size() == 1);
    }

    @Test
    public void getObjectByLidObjectDoesntExist() throws MsgRegistryException {
        List<RegistryObjectType> result = executeQuery(
                getObjectsByLid,
                createQuery(CanonicalQueryTypes.GET_OBJECTS_BY_LID, "lid",
                        "GetByLid Test Object 20"));
        assertTrue(result.isEmpty());
    }

    @Test
    public void getObjectByLidWhenLidNotSpecified() {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_OBJECTS_BY_LID, "lid",
                "GetByLid Test Object 20");
        request.getQuery().getSlot().clear();
        try {
            executeQuery(getObjectsByLid, request);
        } catch (MsgRegistryException e) {
            return;
        }
        fail();
    }
}
