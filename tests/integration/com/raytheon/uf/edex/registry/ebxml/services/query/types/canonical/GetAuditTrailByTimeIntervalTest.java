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

import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.GetAuditTrailByTimeInterval;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Test for the Canonical GetAuditTrailByTimeInterval defined by the EBXML 4.0
 * spec
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
public class GetAuditTrailByTimeIntervalTest extends QueryTest {

    private XMLGregorianCalendar currentTime;

    private XMLGregorianCalendar fiveMinsAgo;

    @Autowired
    private GetAuditTrailByTimeInterval getAuditTrail;

    @Before
    public void insertTestObjects() throws MsgRegistryException,
            EbxmlRegistryException {

        List<RegistryObjectType> objsToSubmit = new ArrayList<RegistryObjectType>();
        for (int i = 0; i < 5; i++) {
            RegistryObjectType obj = new RegistryObjectType("Test Object " + i,
                    "Test Object " + i);
            objsToSubmit.add(obj);
        }
        submitRegistryObjectsToRegistry(objsToSubmit);

        long currentMillis = TimeUtil.currentTimeMillis();
        currentTime = EbxmlObjectUtil
                .getTimeAsXMLGregorianCalendar(currentMillis);
        fiveMinsAgo = EbxmlObjectUtil
                .getTimeAsXMLGregorianCalendar(currentMillis - 5
                        * TimeUtil.MILLIS_PER_MINUTE);
    }

    @Test
    public void getByStartAndEndTime() throws MsgRegistryException {

        List<RegistryObjectType> result = executeQuery(
                getAuditTrail,
                createQuery(
                        CanonicalQueryTypes.GET_AUDIT_TRAIL_BY_TIME_INTERVAL,
                        QueryConstants.START_TIME, fiveMinsAgo,
                        QueryConstants.END_TIME, currentTime));
        assertEquals(33, result.size());
    }

    @Test
    public void getWhenStartAndEndAreNotSpecified() throws MsgRegistryException {
        List<RegistryObjectType> result = executeQuery(
                getAuditTrail,
                createQuery(CanonicalQueryTypes.GET_AUDIT_TRAIL_BY_TIME_INTERVAL));
        assertEquals(0, result.size());
    }
}
