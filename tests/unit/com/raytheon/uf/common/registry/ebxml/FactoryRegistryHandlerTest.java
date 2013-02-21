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
package com.raytheon.uf.common.registry.ebxml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import javax.xml.ws.WebServiceException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders.Type;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.time.util.TimeUtilTest;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Test {@link FactoryRegistryHandler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2012 740        djohnson    Initial creation
 * Aug 15, 2012 0743       djohnson    Type-safe result formatters.
 * Sep 07, 2012 1102       djohnson    Setup the registry encoder.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class FactoryRegistryHandlerTest {

    private static final WebServiceException WEB_SERVICE_EXCEPTION = new WebServiceException(
            "Thrown on purpose");

    private static final CommunicationException COMMUNICATION_EXCEPTION = new CommunicationException(
            RegistryUtil.DATABASE_ERROR_MESSAGE);

    @BeforeClass
    public static void classSetup() {
        RegistryUtilTest.setEncoderStrategy(RegistryEncoders
                .ofType(Type.DYNAMIC_SERIALIZE));
    }

    @After
    public void cleanUp() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void testGetObjectsReturnsFailedStatusIfWebServiceExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).getObjects(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsUnableToConnectMessageIfWebServiceExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).getObjects(null);

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithQueryReturnsUnableToConnectMessageIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).removeObjects(null);

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithObjectsReturnsUnableToConnectMessageIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).removeObjects("someUsername",
                Collections.emptyList());

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testStoreObjectReturnsUnableToConnectMessageIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).storeObject(null);

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithQueryReturnsFailedStatusIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).removeObjects(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testRemoveObjectsWithObjectsReturnsFailedStatusIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).removeObjects("someUsername",
                Collections.emptyList());

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testStoreObjectReturnsFailedStatusIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                WEB_SERVICE_EXCEPTION).storeObject(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsFailedStatusIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).getObjects(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsFailedToConnectToTheDatabaseIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).getObjects(null);

        assertEquals(RegistryUtil.FAILED_TO_CONNECT_TO_DATABASE, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithQueryReturnsUnableToConnectMessageIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).removeObjects(null);

        assertEquals(RegistryUtil.FAILED_TO_CONNECT_TO_DATABASE, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithObjectsReturnsUnableToConnectMessageIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).removeObjects("someUsername",
                Collections.emptyList());

        assertEquals(RegistryUtil.FAILED_TO_CONNECT_TO_DATABASE, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testStoreObjectReturnsUnableToConnectMessageIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).storeObject(null);

        assertEquals(RegistryUtil.FAILED_TO_CONNECT_TO_DATABASE, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithQueryReturnsFailedStatusIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).removeObjects(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testRemoveObjectsWithObjectsReturnsFailedStatusIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).removeObjects("someUsername",
                Collections.emptyList());

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testStoreObjectReturnsFailedStatusIfCommunicationExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).storeObject(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testNonResultFormatterWillReturnDecodedRegistryObjects()
            throws SerializationException {
        final String[] results = new String[] { "one", "two", "three" };
        final List<RegistryObjectType> registryObjectTypes = java.util.Arrays
                .asList(new RegistryObjectType(), new RegistryObjectType(),
                        new RegistryObjectType());

        // Setup the encoded objects as if they were coming from the registry
        for (int i = 0; i < registryObjectTypes.size(); i++) {
            Set<SlotType> slotType = CollectionUtil.asSet(RegistryUtil
                    .encodeObject(results[i]));
            registryObjectTypes.get(i).setSlot(slotType);
        }

        List<String> filteredResults = FactoryRegistryHandler.filterResults(
                new StringQueryNonFormatter(), registryObjectTypes);
        assertEquals("Incorrect number of results were returned!",
                results.length, filteredResults.size());

        for (String result : results) {
            assertTrue("The filtered results should have contained result ["
                    + result + "]", filteredResults.contains(result));
        }
    }

    @Test
    public void testResultFormatterThatReturnsSingleResultWillReturnsAllSingleResults() {
        final String[] results = new String[]{"one", "two", "three"};
        final List<RegistryObjectType> registryObjectTypes = java.util.Arrays.asList(new RegistryObjectType(), new RegistryObjectType(), new RegistryObjectType());

        StringResultsQuery query = new StringResultsQuery(results);

        List<String> filteredResults = FactoryRegistryHandler.filterResults(
                query, registryObjectTypes);
        assertEquals("Incorrect number of results were returned!", results.length, filteredResults.size());

        for (String result : results) {
            assertTrue("The filtered results should have contained result ["
                    + result + "]", filteredResults.contains(result));
        }
    }

    @Test
    public void testResultFormatterThatReturnsMultipleResultsWillReturnAllMultipleResults() {
        final String[][] results = new String[][] {
                new String[] { "one", "two" },
                new String[] { "three", "four" },
                new String[] { "five", "six" } };

        final List<RegistryObjectType> registryObjectTypes = java.util.Arrays
                .asList(new RegistryObjectType(), new RegistryObjectType(),
                        new RegistryObjectType());

        StringArrayResultsQuery query = new StringArrayResultsQuery(results);

        List<String> filteredResults = FactoryRegistryHandler.filterResults(
                query, registryObjectTypes);
        assertEquals("Incorrect number of results were returned!", 6,
                filteredResults.size());

        for (String[] resultArray : results) {
            for (String result : resultArray) {
                assertTrue(
                        "The filtered results should have contained result ["
                                + result + "]",
                        filteredResults.contains(result));
            }
        }
    }

    @Test
    public void testNormalQueryWillReturnAllDecodedObjects() {
        final String[][] results = new String[][] {
                new String[] { "one", "two" },
                new String[] { "three", "four" },
                new String[] { "five", "six" } };

        final List<RegistryObjectType> registryObjectTypes = java.util.Arrays
                .asList(new RegistryObjectType(), new RegistryObjectType(),
                        new RegistryObjectType());

        StringArrayResultsQuery query = new StringArrayResultsQuery(results);

        List<String> filteredResults = FactoryRegistryHandler.filterResults(
                query, registryObjectTypes);
        assertEquals("Incorrect number of results were returned!", 6,
                filteredResults.size());

        for (String[] resultArray : results) {
            for (String result : resultArray) {
                assertTrue(
                        "The filtered results should have contained result ["
                                + result + "]",
                        filteredResults.contains(result));
            }
        }
    }

    @Test
    public void testWarningIsLoggedForExcessiveTimedQuery()
            throws MsgRegistryException {
        TimeUtilTest.installMockTimes(
                FactoryRegistryHandler.QUERY_DURATION_WARN_LEVEL + 1,
                TimeUnit.MILLISECONDS);

        TestableFactoryRegistryHandler handler = new TestableFactoryRegistryHandler();

        IUFStatusHandler statusHandler = mock(IUFStatusHandler.class);
        IUFStatusHandler oldHandler = FactoryRegistryHandler.statusHandler;
        try {
            FactoryRegistryHandler.statusHandler = statusHandler;

            IdQuery<String> registryQuery = new IdQuery<String>(String.class);
            registryQuery.setID("someId");

            handler.getObjects(registryQuery);

            verify(statusHandler, never()).warn(anyString());
        } finally {
            FactoryRegistryHandler.statusHandler = oldHandler;
        }
    }

    @Test
    public void testWarningIsNotLoggedForNonExcessiveTimedQuery()
            throws MsgRegistryException {
        TimeUtilTest.installMockTimes(
                FactoryRegistryHandler.QUERY_DURATION_WARN_LEVEL - 1,
                TimeUnit.MILLISECONDS);

        TestableFactoryRegistryHandler handler = new TestableFactoryRegistryHandler();

        IUFStatusHandler statusHandler = mock(IUFStatusHandler.class);
        IUFStatusHandler oldHandler = FactoryRegistryHandler.statusHandler;
        try {
            FactoryRegistryHandler.statusHandler = statusHandler;

            IdQuery<String> registryQuery = new IdQuery<String>(String.class);
            registryQuery.setID("someId");

            handler.getObjects(registryQuery);

            verify(statusHandler, never()).warn(anyString());
        } finally {
            FactoryRegistryHandler.statusHandler = oldHandler;
        }
    }

    private static <T extends Exception> FactoryRegistryHandler getExceptionThrowingHandler(
            final T t) {
        FactoryRegistryHandler handler = new FactoryRegistryHandler();
        handler.setTxManager(new RegistryTxManager() {
            @Override
            public TxManager getTxManager() {
                return new TxManager() {
                    @Override
                    public void startTransaction() throws Exception {
                        throw t;
                    }

                    @Override
                    public void closeTransaction() {
                    }
                };
            }
        });

        return handler;
    }
}
