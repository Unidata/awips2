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
package com.raytheon.uf.edex.datadelivery.harvester.purge;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpUriRequest;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaDataFixture;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderFixture;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;

/**
 * Test {@link OpenDapGriddedPurgeImpl}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 5, 2012  1102      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class OpenDapGriddedPurgeImplTest {

    private final HttpClient mockHttpClient = mock(HttpClient.class);

    private final OpenDapGriddedPurgeImpl purge = new OpenDapGriddedPurgeImpl() {
        // Inject the mock http client
        @Override
        HttpClient getHttpClient() {
            return mockHttpClient;
        }
    };

    private final OpenDapGriddedDataSetMetaData metaData = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
            .get();

    private final HarvesterConfig harvesterConfig = new HarvesterConfig();
    {
        Provider provider = ProviderFixture.INSTANCE.get();

        harvesterConfig.setProvider(provider);
    }

    /**
     * Sets up the Http status return code on the mock objects.
     * 
     * @param statusCode
     *            the status code to return
     * @param entityBody
     */
    private void setupHttpResponse(int statusCode, String entityBody) {
        final HttpResponse response = mock(HttpResponse.class);
        final StatusLine statusLine = mock(StatusLine.class);
        final HttpEntity entity = mock(HttpEntity.class);

        try {
            when(mockHttpClient.execute(any(HttpUriRequest.class))).thenReturn(
                    response);
            when(response.getStatusLine()).thenReturn(statusLine);
            when(statusLine.getStatusCode()).thenReturn(statusCode);
            when(response.getEntity()).thenReturn(entity);
            when(entity.getContent()).thenReturn(
                    new ByteArrayInputStream(entityBody.getBytes()));
        } catch (Exception e) {
            throw new RuntimeException(
                    "An exception that never should have occurred did!", e);
        }
    }

    @Test
    public void testReturnsFalseIfUrlReturnsSuccessfulStatusWithoutErrorPage() {
        setupHttpResponse(HttpStatus.SC_OK, "good response");

        assertFalse(
                "Expected to return false when a successful status is returned!",
                purge.isTimeToPurge(metaData, harvesterConfig));
    }

    @Test
    public void testReturnsFalseIfUrlReturnsUnsuccessfulStatus() {
        setupHttpResponse(HttpStatus.SC_BAD_GATEWAY, "good response");

        assertFalse(
                "Expected to return false when a non-successful status is returned!  "
                        + "We don't want to purge all of the metadata if the server is down!",
                purge.isTimeToPurge(metaData, harvesterConfig));
    }

    @Test
    public void testReturnsTrueIfUrlIsUnparseable() {
        metaData.setUrl("::");

        assertTrue(purge.isTimeToPurge(metaData, harvesterConfig));
    }

    @Test
    public void testReturnsTrueIfUrlReturnsSuccessfulStatusWithErrorPage() {
        setupHttpResponse(HttpStatus.SC_OK, "GrADS Data Server - error");

        assertTrue(
                "Expected to return true when a successful status is returned!",
                purge.isTimeToPurge(metaData, harvesterConfig));
    }

}
