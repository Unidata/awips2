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
package com.raytheon.uf.common.dataplugin.radar.util;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;

/**
 *
 * Unit tests for {@link RadarUtil}
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2024  2036516    bines       Initial creation
 *
 * </pre>
 *
 * @author bines
 */
class TestRadarUtil {

    private DbQueryResponse dbQueryResponse;

    private MockedStatic<RadarUtil> radarUtilMock;

    private RadarStation radar1;

    private RadarStation radar2;

    @BeforeEach
    public void setupBeforeEach() throws Exception {
        radar1 = new RadarStation();
        radar1.setRdaId("KABR");
        radar1.setElevMeter((float) 421.44208);
        radar1.setEqpElv((float) 1382.684);
        radar1.setImmutablEx((float) 154);
        radar1.setLat((float) 45.45583);
        radar1.setLon((float) -98.41306);
        radar1.setName("Aberdeen");
        radar1.setRpgIdDec("309");

        radar2 = new RadarStation();
        radar2.setRdaId("KABX");
        radar2.setElevMeter((float) 1813.4156);
        radar1.setEqpElv((float) 5949.5264);
        radar2.setImmutablEx((float) 93);
        radar2.setLat((float) 35.14972);
        radar2.setLon((float) -106.82389);
        radar2.setName("La Mesita Negra");
        radar2.setRpgIdDec("311");

        radarUtilMock = mockStatic(RadarUtil.class);
    }

    @AfterEach
    public void setupAfterEach() throws Exception {
        radarUtilMock.close();
    }

    @Test
    void testGetAllRadarsRdaId() {
        // Test getAllRadarsRdaId function
        String requestField = "rdaId";
        Map<String, Object> data1 = new HashMap<>();
        data1.put(requestField, radar1.getRdaId());
        Map<String, Object> data2 = new HashMap<>();
        data2.put(requestField, radar2.getRdaId());
        List<Map<String, Object>> results = new ArrayList<>();
        results.add(data1);
        results.add(data2);

        Set<String> expectedResult = new HashSet<>();
        expectedResult.add(radar1.getRdaId().toLowerCase());
        expectedResult.add(radar2.getRdaId().toLowerCase());

        dbQueryResponse = new DbQueryResponse();
        dbQueryResponse.setResults(results);
        radarUtilMock.when(() -> RadarUtil.sendRequest(any()))
                .thenReturn(dbQueryResponse);
        when(RadarUtil.getAllRadarsRdaId()).thenCallRealMethod();
        Set<String> allRadarsRdaId = RadarUtil.getAllRadarsRdaId();
        assertEquals(expectedResult, allRadarsRdaId);
    }

    @Test
    void testGetAllRadars() {
        // Test getAllRadars function
        String requestField = null;
        Map<String, Object> data1 = new HashMap<>();
        data1.put(requestField, radar1);
        Map<String, Object> data2 = new HashMap<>();
        data2.put(requestField, radar2);
        List<Map<String, Object>> results = new ArrayList<>();
        results.add(data1);
        results.add(data2);

        Set<RadarStation> expectedResult = new HashSet<>();
        expectedResult.add(radar1);
        expectedResult.add(radar2);

        dbQueryResponse = new DbQueryResponse();
        dbQueryResponse.setResults(results);
        radarUtilMock.when(() -> RadarUtil.sendRequest(any()))
                .thenReturn(dbQueryResponse);
        when(RadarUtil.getAllRadars()).thenCallRealMethod();
        Set<RadarStation> allRadars = RadarUtil.getAllRadars();
        assertEquals(expectedResult, allRadars);
    }

}
