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

import java.net.ConnectException;
import java.rmi.RemoteException;
import java.util.Collections;

import org.apache.http.HttpHost;
import org.apache.http.conn.HttpHostConnectException;
import org.junit.Test;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.registry.IRegistryRequest;
import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Test {@link ThriftRegistryHandler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2012 740        djohnson     Initial creation
 * Sep 12, 2012 1167       djohnson     Use localization.
 * Nov 15, 2012 1286       djohnson     No constructor arguments for ThriftRegistryHandler.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class ThriftRegistryHandlerTest {
    

    private static final HttpHostConnectException HTTP_HOST_CONNECT_EXCEPTION = new HttpHostConnectException(
            new HttpHost("someHost"), new ConnectException(
                    "'cause I done thrown it."));

    private static final CommunicationException COMMUNICATION_EXCEPTION = new CommunicationException(
            RegistryUtil.DATABASE_ERROR_MESSAGE);

    @Test
    public void testGetObjectsReturnsFailedStatusIfHttpHostConnectExceptionThrown() {
        RegistryQueryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).getObjects(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsUnableToConnectMessageIfHttpHostConnectExceptionThrown() {
        RegistryQueryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).getObjects(null);

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithQueryReturnsUnableToConnectMessageIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).removeObjects(null);

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithObjectsReturnsUnableToConnectMessageIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).removeObjects("someUsername",
                Collections.emptyList());

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testStoreObjectReturnsUnableToConnectMessageIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).storeObject(null);

        assertEquals(RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, response
                .getErrors().iterator().next().getMessage());
    }

    @Test
    public void testRemoveObjectsWithQueryReturnsFailedStatusIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).removeObjects(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testRemoveObjectsWithObjectsReturnsFailedStatusIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).removeObjects("someUsername",
                Collections.emptyList());

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testStoreObjectReturnsFailedStatusIfHttpHostConnectExceptionThrown() {
        RegistryResponse<Object> response = getExceptionThrowingHandler(
                HTTP_HOST_CONNECT_EXCEPTION).storeObject(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsFailedStatusIfCommunicationExceptionThrown() {
        RegistryQueryResponse<Object> response = getExceptionThrowingHandler(
                COMMUNICATION_EXCEPTION).getObjects(null);

        assertEquals(OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testGetObjectsReturnsFailedToConnectToTheDatabaseIfCommunicationExceptionThrown() {
        RegistryQueryResponse<Object> response = getExceptionThrowingHandler(
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

    private static <T extends Exception> ThriftRegistryHandler getExceptionThrowingHandler(
            final T t) {
        ThriftRegistryHandler handler = new ThriftRegistryHandler() {
            @Override
            <U> RegistryResponse<U> sendRequestViaThrift(
                    IRegistryRequest<U> request) throws SerializationException,
                    RemoteException {
                throw new RemoteException(
                        "Error communicating with the server", t);
            }
        };
        return handler;
    }
}
