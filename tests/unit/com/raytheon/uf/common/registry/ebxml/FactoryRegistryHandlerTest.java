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

import java.util.List;
import java.util.concurrent.Callable;
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
import com.raytheon.uf.common.registry.constants.RegistryErrorMessage;
import com.raytheon.uf.common.registry.ebxml.encoder.IRegistryEncoder;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders.Type;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.time.util.TimeUtilTest;

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
 * Jun 24, 2013 2106       djohnson    FactoryRegistryHandler now takes the encoder.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class FactoryRegistryHandlerTest {

    private static final WebServiceException WEB_SERVICE_EXCEPTION = new WebServiceException(
            "Thrown on purpose");

    private static final CommunicationException COMMUNICATION_EXCEPTION = new CommunicationException(
            RegistryErrorMessage.DATABASE_ERROR_MESSAGE);

    private static final IRegistryEncoder encoderStrategy = RegistryEncoders
            .ofType(Type.JAXB);

    private static final FactoryRegistryHandler registryHandler = new FactoryRegistryHandler();

    @BeforeClass
    public static void classSetUp() {
        registryHandler.setEncoderStrategy(encoderStrategy);
    }

    @After
    public void cleanUp() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void testGetObjectsReturnsFailedStatusIfWebServiceExceptionThrown() {
        Callable<RegistryResponse<Object>> callable = new Callable<RegistryResponse<Object>>() {
            @Override
            public RegistryResponse<Object> call() throws Exception {
                throw WEB_SERVICE_EXCEPTION;
            }
        };
        RegistryResponse<Object> response = registryHandler.processRequest(
                callable, new RegistryResponse<Object>());
        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsUnableToConnectMessageIfWebServiceExceptionThrown() {
        Callable<RegistryResponse<Object>> callable = new Callable<RegistryResponse<Object>>() {
            @Override
            public RegistryResponse<Object> call() throws Exception {
                throw WEB_SERVICE_EXCEPTION;
            }
        };
        RegistryResponse<Object> response = registryHandler.processRequest(
                callable, new RegistryResponse<Object>());
        assertEquals(RegistryErrorMessage.UNABLE_TO_CONNECT_TO_REGISTRY,
                response.getErrors().iterator().next().getMessage());
    }

    @Test
    public void testGetObjectsReturnsFailedStatusIfCommunicationExceptionThrown() {
        Callable<RegistryResponse<Object>> callable = new Callable<RegistryResponse<Object>>() {
            @Override
            public RegistryResponse<Object> call() throws Exception {
                throw COMMUNICATION_EXCEPTION;
            }
        };
        RegistryResponse<Object> response = registryHandler.processRequest(
                callable, new RegistryResponse<Object>());
        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsFailedToConnectToTheDatabaseIfCommunicationExceptionThrown() {
        Callable<RegistryResponse<Object>> callable = new Callable<RegistryResponse<Object>>() {
            @Override
            public RegistryResponse<Object> call() throws Exception {
                throw COMMUNICATION_EXCEPTION;
            }
        };
        RegistryResponse<Object> response = registryHandler.processRequest(
                callable, new RegistryResponse<Object>());
        assertEquals(RegistryErrorMessage.FAILED_TO_CONNECT_TO_DATABASE,
                response.getErrors().iterator().next().getMessage());
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
            SlotType slotType = encoderStrategy.encodeObject(results[i]);
            registryObjectTypes.get(i).getSlot().add(slotType);
        }

        List<String> filteredResults = registryHandler.filterResults(
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
        final String[] results = new String[] { "one", "two", "three" };
        final List<RegistryObjectType> registryObjectTypes = java.util.Arrays
                .asList(new RegistryObjectType(), new RegistryObjectType(),
                        new RegistryObjectType());

        StringResultsQuery query = new StringResultsQuery(results);

        List<String> filteredResults = registryHandler.filterResults(query,
                registryObjectTypes);
        assertEquals("Incorrect number of results were returned!",
                results.length, filteredResults.size());

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

        List<String> filteredResults = registryHandler.filterResults(query,
                registryObjectTypes);
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

        List<String> filteredResults = registryHandler.filterResults(query,
                registryObjectTypes);
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
}
