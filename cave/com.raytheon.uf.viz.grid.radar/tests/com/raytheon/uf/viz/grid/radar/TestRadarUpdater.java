/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.grid.radar;

import static org.junit.Assert.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_MOCKS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.MockedStatic;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.grid.radar.RadarUpdater.CacheKey;
import com.raytheon.uf.viz.grid.radar.RadarUpdater.TimeAndSpaceCacheEntry;
import com.raytheon.uf.viz.grid.radar.RadarUpdater.TimeCacheEntry;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * Unit tests for {@link RadarUpdater}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2024 2037092    mapeters    Initial creation
 * Sep 16, 2024 2037941    mapeters    Virtual availability is no longer cached
 * Oct 14, 2024 2037939    mapeters    Test getStationTimes()
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarUpdater {

    private static final String KOAX = "koax";

    private static final DataTime dt30 = new DataTime("2024-01-01_12:30:00.0");

    private static final DataTime dt35 = new DataTime("2024-01-01_12:35:00.0");

    private static final DataTime dt40 = new DataTime("2024-01-01_12:40:00.0");

    private RadarUpdater updater;

    @BeforeEach
    void setup() {
        try (MockedStatic<ProductAlertObserver> paoMockedStatic = mockStatic(
                ProductAlertObserver.class);
                MockedStatic<RadarDisplayManager> rdmMockedStatic = mockStatic(
                        RadarDisplayManager.class)) {
            // These just disable things that would throw exceptions
            paoMockedStatic
                    .when(() -> ProductAlertObserver.addObserver(any(), any()))
                    .then(invocation -> null);
            rdmMockedStatic.when(RadarDisplayManager::getInstance)
                    .then(RETURNS_MOCKS);

            updater = RadarUpdater.getInstance();
        }

    }

    @ParameterizedTest
    @CsvSource({ "koax,153,1.5,true", "kdmx,154,3.4,false" })
    void testGetCacheKey(String icao, int productCode, double tilt,
            boolean virtualVolume) {
        RadarRequestableLevelNode levelNode = mock(
                RadarRequestableLevelNode.class);
        when(levelNode.getIcao()).thenReturn(icao);
        when(levelNode.getProductCode()).thenReturn(productCode);
        when(levelNode.getTilt()).thenReturn(tilt);
        when(levelNode.isVirtualVolume()).thenReturn(virtualVolume);

        CacheKey cacheKey = updater.getCacheKey(levelNode);

        assertEquals(icao, cacheKey.icao);
        assertEquals(productCode, cacheKey.productCode);
        assertEquals(tilt, cacheKey.elevationAngle);
        assertEquals(virtualVolume, cacheKey.normalScansOnly);
    }

    @Test
    void testClearCache() {
        // Verify cached values are reset
        updater.cache.put(new CacheKey(KOAX, 153, 1.5, true),
                new TimeAndSpaceCacheEntry(Set.of()));
        updater.stationTimesCache.put(KOAX, new TimeCacheEntry(Set.of()));

        updater.clearCache();

        assertTrue(updater.cache.isEmpty());
        assertTrue(updater.stationTimesCache.isEmpty());
    }

    @ParameterizedTest
    @CsvSource({
            // All match
            "koax,153,1.5,true,koax,153,1.5,true,true",
            // Different icao
            "koax,153,1.5,true,kdmx,153,1.5,true,false",
            // Different product code
            "koax,153,1.5,true,koax,159,1.5,true,false",
            // Different tilt
            "koax,153,1.5,true,koax,153,2.5,true,false",
            // Different virtual volume flag
            "koax,153,1.5,true,koax,153,1.5,false,false" })
    void testCacheKeyEquals(String icao1, int pc1, double tilt1, boolean vv1,
            String icao2, int pc2, double tilt2, boolean vv2,
            boolean expectedEquals) {
        CacheKey key1 = new CacheKey(icao1, pc1, tilt1, vv1);
        CacheKey key2 = new CacheKey(icao2, pc2, tilt2, vv2);

        boolean actualEquals = key1.equals(key2);

        assertEquals(expectedEquals, actualEquals);
    }

    @ParameterizedTest
    @CsvSource({
            // All match
            "koax,153,1.5,true,koax,153,1.5,true,true",
            // Different icao
            "koax,153,1.5,true,kdmx,153,1.5,true,false",
            // Different product code
            "koax,153,1.5,true,koax,159,1.5,true,false",
            // Different tilt
            "koax,153,1.5,true,koax,153,2.5,true,false",
            // Different virtual volume flag
            "koax,153,1.5,true,koax,153,1.5,false,false" })
    void testCacheKeyHashCode(String icao1, int pc1, double tilt1, boolean vv1,
            String icao2, int pc2, double tilt2, boolean vv2,
            boolean expectedEquals) {
        CacheKey key1 = new CacheKey(icao1, pc1, tilt1, vv1);
        CacheKey key2 = new CacheKey(icao2, pc2, tilt2, vv2);

        int hashCode1 = key1.hashCode();
        int hashCode2 = key2.hashCode();
        boolean actualEquals = hashCode1 == hashCode2;

        assertEquals(expectedEquals, actualEquals);
    }

    @Test
    void testGetStationTimes1() {
        // No cache entry -> DB is queried and result is cached
        Set<DataTime> actualTimes;
        try (MockedStatic<CatalogQuery> catalogQueryStaticMock = mockStatic(
                CatalogQuery.class)) {
            Map<String, RequestConstraint> query = Map.of(
                    RadarAdapter.PLUGIN_NAME_QUERY,
                    new RequestConstraint(RadarAdapter.RADAR_SOURCE),
                    RadarAdapter.ICAO_QUERY, new RequestConstraint(KOAX));
            catalogQueryStaticMock.when(
                    () -> CatalogQuery.performTimeQuery(query, false, null))
                    .thenReturn(new DataTime[] { dt30, dt35 });

            actualTimes = updater.getStationTimes(KOAX);
        }

        Set<DataTime> expectedTimes = Set.of(dt30, dt35);
        assertEquals(expectedTimes, actualTimes);
        assertEquals(expectedTimes, updater.stationTimesCache.get(KOAX).times);
        assertThrows(UnsupportedOperationException.class, actualTimes::clear);
    }

    @Test
    void testGetStationTimes2() {
        // Non-expired cache entry is used
        TimeCacheEntry cacheEntry = new TimeCacheEntry(Set.of(dt30, dt35));
        updater.stationTimesCache.put(KOAX, cacheEntry);

        Set<DataTime> actualTimes;
        try (MockedStatic<CatalogQuery> catalogQueryStaticMock = mockStatic(
                CatalogQuery.class)) {
            actualTimes = updater.getStationTimes(KOAX);

            catalogQueryStaticMock.verifyNoInteractions();
        }

        Set<DataTime> expectedTimes = Set.of(dt30, dt35);
        assertEquals(expectedTimes, actualTimes);
        assertSame(cacheEntry, updater.stationTimesCache.get(KOAX));
        assertThrows(UnsupportedOperationException.class, actualTimes::clear);
    }

    @Test
    void testGetStationTimes3() {
        /*
         * Expired cache entry -> entry is not used, DB is queried and new
         * result is cached
         */
        TimeCacheEntry cacheEntry;
        /*
         * Cache entry constructor grabs current time, mock it to be an older,
         * expired time
         */
        Instant frozenInstant = Instant.now(Clock.fixed(
                Instant.ofEpochMilli(Instant.now().toEpochMilli()
                        - RadarUpdater.CACHE_TIME - 1),
                ZoneId.systemDefault()));
        try (MockedStatic<Instant> instantStaticMock = mockStatic(
                Instant.class)) {
            instantStaticMock.when(Instant::now).thenReturn(frozenInstant);
            cacheEntry = new TimeCacheEntry(Set.of(dt30, dt35));
            updater.stationTimesCache.put(KOAX, cacheEntry);
        }

        Set<DataTime> actualTimes;
        try (MockedStatic<CatalogQuery> catalogQueryStaticMock = mockStatic(
                CatalogQuery.class)) {
            Map<String, RequestConstraint> query = Map.of(
                    RadarAdapter.PLUGIN_NAME_QUERY,
                    new RequestConstraint(RadarAdapter.RADAR_SOURCE),
                    RadarAdapter.ICAO_QUERY, new RequestConstraint(KOAX));
            catalogQueryStaticMock.when(
                    () -> CatalogQuery.performTimeQuery(query, false, null))
                    .thenReturn(new DataTime[] { dt30, dt35, dt40 });

            actualTimes = updater.getStationTimes(KOAX);
        }

        Set<DataTime> expectedTimes = Set.of(dt30, dt35, dt40);
        assertEquals(expectedTimes, actualTimes);
        assertNotSame(cacheEntry, updater.stationTimesCache.get(KOAX));
        assertEquals(expectedTimes, updater.stationTimesCache.get(KOAX).times);
        assertThrows(UnsupportedOperationException.class, actualTimes::clear);
    }
}
