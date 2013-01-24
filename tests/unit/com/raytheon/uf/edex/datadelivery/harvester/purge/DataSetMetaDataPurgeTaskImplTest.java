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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Multimap;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaDataFixture;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderFixture;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;

/**
 * Test {@link DataSetMetaDataPurgeImpl).
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2012  1102       djohnson     Initial creation
 * Dec 12, 2012 1041       dhladky      Updated for multi provider purging.
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0	
 */

public class DataSetMetaDataPurgeTaskImplTest {

    private final IOpenDapGriddedPurge openDapGriddedPurge = mock(IOpenDapGriddedPurge.class);

    private final OpenDapGriddedDataSetMetaData[] dataSetMetaDatas = new OpenDapGriddedDataSetMetaData[] {
            OpenDapGriddedDataSetMetaDataFixture.INSTANCE.get(1),
            OpenDapGriddedDataSetMetaDataFixture.INSTANCE.get(2),
            OpenDapGriddedDataSetMetaDataFixture.INSTANCE.get(3) };
    {
        // Give each one a unique date
        for (int i = 0; i < dataSetMetaDatas.length; i++) {
            dataSetMetaDatas[i].setDate(new ImmutableDate(i));
        }
    }

    private final List<HarvesterConfig> harvesterConfigs = new ArrayList<HarvesterConfig>();
    {
        Provider provider = ProviderFixture.INSTANCE.get();
        HarvesterConfig config = new HarvesterConfig();
        config.setProvider(provider);
        harvesterConfigs.add(config);
        // We only need to test one to make sure it works
    }

    private final DataSetMetaDataPurgeTaskImpl purge = new DataSetMetaDataPurgeTaskImpl(
            openDapGriddedPurge) {

        @Override
        List<DataSetMetaData> getDataSetMetaDatas() {
            return java.util.Arrays.<DataSetMetaData> asList(dataSetMetaDatas);
        }

        @Override
        List<HarvesterConfig> getHarvesterConfigs() {
            return harvesterConfigs;
        }
    };

    @Before
    public void setUp() {
        RegistryObjectHandlersUtil.initMocks();

        purge.initializeState();
    }

    @Test(expected = NullPointerException.class)
    public void testExceptionThrownWhenMetaDataIsNull() {
        DataSetMetaDataPurgeTaskImpl.purgeMetaData(null);
    }

    @Test
    public void testMetaDatasAreOrderedByDate() {
        // Give all DataSetMetaDatas the same dataset name and provider name so
        // they go in the same set
        for (DataSetMetaData metaData : dataSetMetaDatas) {
            metaData.setDataSetName("dataSetName");
            metaData.setProviderName("providerName");
        }

        Multimap<String, DataSetMetaData> dataSetKeyedMap = purge
                .getDataSetNameKeyedInstanceMap();

        Set<String> keySet = dataSetKeyedMap.keySet();
        assertEquals("Incorrect number of dataset name keys!", 1, keySet.size());

        DataSetMetaData old = null;
        for (DataSetMetaData metaData : dataSetKeyedMap.values()) {
            if (old != null) {
                assertTrue(
                        "The DataSetMetaData instances should have been ordered by date!",
                        metaData.getDate().after(old.getDate()));
            }
            old = metaData;
        }
    }

    @Test
    public void testMetaDatasAreSeparatedIntoMapSetsKeyedByDataSetName() {
        Multimap<String, DataSetMetaData> dataSetKeyedMap = purge
                .getDataSetNameKeyedInstanceMap();

        Set<String> keySet = dataSetKeyedMap.keySet();
        assertEquals("Incorrect number of dataset name keys!",
                dataSetMetaDatas.length, keySet.size());

        for (DataSetMetaData metaData : dataSetMetaDatas) {
            assertTrue(
                    "Did not find the dataset name and provider combination as a key a in the map!",
                    keySet.contains(DataSetMetaDataPurgeTaskImpl
                            .getDatasetMetaDataMapKey(metaData)));
        }
    }

    @Test
    public void testOpenDapGriddedDataSetMetaDataWillInvokeOpenDapGriddedPurge() {
        OpenDapGriddedDataSetMetaData metaData = dataSetMetaDatas[0];

        purge.visit(metaData);

        verify(openDapGriddedPurge).isTimeToPurge(metaData,
                harvesterConfigs.get(0));
    }

    @Test
    public void testRunWillCallVisitOnAllDataSetMetaDataInstances() {
        purge.run();

        for (OpenDapGriddedDataSetMetaData metaData : dataSetMetaDatas) {
            verify(openDapGriddedPurge)
.isTimeToPurge(metaData,
                    harvesterConfigs.get(0));
        }
    }

    @Test
    public void testWhenDataSetMetadataReturnsFalseForPurgeNextDataSetIsChecked() {
        // Give all but the last DataSetMetaData the same dataset name and
        // provider name so they go in the same set, the last will be for a
        // separate data set that we verify is still checked
        final int lastIndex = dataSetMetaDatas.length - 1;
        for (int i = 0; i < lastIndex; i++) {
            dataSetMetaDatas[i].setDataSetName("sameDataSetName");
            dataSetMetaDatas[i].setProviderName("sameProviderName");
        }

        when(
                openDapGriddedPurge.isTimeToPurge(dataSetMetaDatas[0],
                        harvesterConfigs.get(0))).thenReturn(false);

        purge.run();

        // Make sure the other dataset is still checked
        verify(openDapGriddedPurge).isTimeToPurge(dataSetMetaDatas[lastIndex],
                harvesterConfigs.get(0));
    }

    @Test
    public void testWhenDataSetMetadataReturnsFalseForPurgeSetIsNoLongerChecked() {
        // Give all DataSetMetaDatas the same dataset name and provider name so
        // they go in the same set
        for (DataSetMetaData metaData : dataSetMetaDatas) {
            metaData.setDataSetName("dataSetName");
            metaData.setProviderName("providerName");
        }

        when(
                openDapGriddedPurge.isTimeToPurge(dataSetMetaDatas[0],
                        harvesterConfigs.get(0))).thenReturn(true);
        when(
                openDapGriddedPurge.isTimeToPurge(dataSetMetaDatas[1],
                        harvesterConfigs.get(0))).thenReturn(false);

        purge.run();

        // This one should not be checked since the previous returned false
        verify(openDapGriddedPurge, never()).isTimeToPurge(dataSetMetaDatas[2],
                harvesterConfigs.get(0));
    }

    @Test
    public void testWhenDataSetMetadataReturnsTrueForPurgeSetIsContinuouslyChecked() {
        // Give all DataSetMetaDatas the same dataset name and provider name so
        // they go in the same set
        for (OpenDapGriddedDataSetMetaData metaData : dataSetMetaDatas) {
            metaData.setDataSetName("dataSetName");
            metaData.setProviderName("providerName");
            when(
                    openDapGriddedPurge.isTimeToPurge(metaData,
                            harvesterConfigs.get(0)))
                    .thenReturn(true);
        }

        purge.run();

        for (OpenDapGriddedDataSetMetaData metaData : dataSetMetaDatas) {
            verify(openDapGriddedPurge)
.isTimeToPurge(metaData,
                    harvesterConfigs.get(0));
        }
    }

    @Test
    public void testWhenTimeToPurgeReturnsFalseDataSetMetaDataIsNotRemoved() {
        when(
                openDapGriddedPurge.isTimeToPurge(dataSetMetaDatas[0],
                        harvesterConfigs.get(0))).thenReturn(false);

        purge.visit(dataSetMetaDatas[0]);

        verifyZeroInteractions(DataDeliveryHandlers.getDataSetMetaDataHandler());
    }

    @Test
    public void testWhenTimeToPurgeReturnsTrueDataSetMetaDataIsRemoved()
            throws RegistryHandlerException {
        when(
                openDapGriddedPurge.isTimeToPurge(dataSetMetaDatas[0],
                        harvesterConfigs.get(0))).thenReturn(true);

        purge.visit(dataSetMetaDatas[0]);

        verify(DataDeliveryHandlers.getDataSetMetaDataHandler()).delete(
                dataSetMetaDatas[0]);
    }
}
