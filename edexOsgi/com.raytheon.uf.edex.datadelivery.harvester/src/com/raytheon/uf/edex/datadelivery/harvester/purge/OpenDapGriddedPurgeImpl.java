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

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.conn.params.ConnRoutePNames;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.EntityUtils;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ConnectionUtil;

/**
 * Purges {@link OpenDapGriddedDataSetMetaData} instances that are no longer
 * retrievable. Intentionally package-private as it should not be part of the
 * public API.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2012  1102      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class OpenDapGriddedPurgeImpl implements IOpenDapGriddedPurge {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenDapGriddedPurgeImpl.class);

    /**
     * Retrieves the error response pattern from the provider for the specified
     * metadata instance.
     * 
     * @param metaData
     *            the metadata
     * @param harvesterConfig
     *            the harvester configuration
     * @return the pattern or null if the applicable provider cannot be found
     */
    private static Pattern getProviderErrorResponsePattern(
            DataSetMetaData metaData, HarvesterConfig harvesterConfig) {

        Provider provider = harvesterConfig.getProvider();
        final String providerName = provider.getName();
        final String metadataProviderName = metaData.getProviderName();

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug("Checking provider [" + providerName
                    + "] to match metadata provider name ["
                    + metadataProviderName + "].");
        }

        if (metadataProviderName.equals(providerName)) {
            return Pattern.compile(provider.getErrorResponsePattern());
        }

        return null;
    }

    /**
     * Retrieves the HttpClient implementation to use.
     * 
     * @return the {@link HttpClient} implementation
     */
    @VisibleForTesting
    HttpClient getHttpClient() {
        HttpClient httpClient = new DefaultHttpClient();
        String[] proxyParameters = ConnectionUtil.getProxyParameters();
        if (proxyParameters != null) {
            HttpHost proxy = new HttpHost(proxyParameters[0],
                    Integer.parseInt(proxyParameters[1]));
            httpClient.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY,
                    proxy);
        }
        return httpClient;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isTimeToPurge(OpenDapGriddedDataSetMetaData metaData,
            HarvesterConfig harvesterConfig) {
        HttpGet request = new HttpGet();
        HttpResponse response = null;
        try {
            final String url = metaData.getUrl();
            request.setURI(new URI(url));

            HttpClient httpClient = getHttpClient();

            response = httpClient.execute(request);
            int code = response.getStatusLine().getStatusCode();

            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug(String.format(
                        "Received status code [%s] from url [%s]", code, url));
            }

            if (code == HttpStatus.SC_OK) {
                String entityContent = EntityUtils.toString(response
                        .getEntity());
                Pattern providerErrorResponsePattern = getProviderErrorResponsePattern(
                        metaData, harvesterConfig);
                if (providerErrorResponsePattern != null) {
                    Matcher matcher = providerErrorResponsePattern
                            .matcher(entityContent);
                    if (matcher.find()) {
                        return true;
                    }
                } else {
                    statusHandler
                            .warn(String
                                    .format("Unable to find a configured provider by name [%s], removing obsolete DataSetMetaData.",
                                            metaData.getProviderName()));
                }
            }
            return false;
        } catch (URISyntaxException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Unable to parse URL into a URI, purging metadata since it would otherwise remain unusable!",
                            e);
            return true;
        } catch (IOException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Unable to contact the host, not purging metadata since it may still be valid!",
                            e);
            request.abort();
            return false;
        } finally {
            if (response != null) {
                try {
                    EntityUtils.consume(response.getEntity());
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
    }
}
