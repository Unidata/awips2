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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import java.text.ParsePosition;
import java.util.stream.Stream;

import javax.measure.Unit;

import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.units.RadarUnits;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.DefaultVizRadarRecord;

import tech.units.indriya.format.SimpleUnitFormat;

/**
 * Unit tests for {@link RadarRequestableData}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2024 2037092    mapeters    Initial creation
 * Jul 17, 2024 2037624    mapeters    Remove unused DBZ field
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
public class TestRadarRequestableData {

    private static final String TILT = "TILT";

    private static final String RR = "RR";

    private static final String DT_STR = "2024-01-01_12:00:00.0";

    @Mock
    private GridCoverage coverage;

    private static Stream<Arguments> provideArgsForBuildGridRecord() {
        DataTime dt = new DataTime(DT_STR);
        DataTime dtSpatial = new DataTime(DT_STR);
        dtSpatial.setLevel(1.5, TILT);

        /*
         * Test that a non-spatial time is set on the grid record, regardless of
         * if the input time is spatial or not
         */
        return Stream.of(Arguments.of(dt, dt), Arguments.of(dtSpatial, dt));
    }

    @ParameterizedTest
    @MethodSource("provideArgsForBuildGridRecord")
    void testBuildGridRecord(DataTime inputTime, DataTime expectedOutputTime)
            throws VizException {
        RadarRecord record = new RadarRecord();
        record.setIcao("KOAX");
        record.setPrimaryElevationAngle(1.5);
        RadarAdapter radarAdapter = mock(RadarAdapter.class);
        when(radarAdapter.getCoverage(any())).thenReturn(coverage);
        ColorMapParameters cmapParams = mock(ColorMapParameters.class);
        doReturn(RadarUnits.DBZ).when(cmapParams).getDisplayUnit();

        LevelFactory levelFactory = mock(LevelFactory.class);
        when(levelFactory.getLevel(eq(TILT), anyDouble())).then(invocation -> {
            double tiltVal = invocation.getArgument(1);

            return getLevel(tiltVal);
        });

        GridRecord gridRecord;
        try (MockedStatic<RadarAdapter> radarAdapterStaticMock = mockStatic(
                RadarAdapter.class);
                MockedStatic<LevelFactory> levelFactoryStaticMock = mockStatic(
                        LevelFactory.class)) {
            radarAdapterStaticMock.when(RadarAdapter::getInstance)
                    .thenReturn(radarAdapter);
            radarAdapterStaticMock.when(() -> RadarAdapter.getColorMap(any()))
                    .thenReturn(cmapParams);
            levelFactoryStaticMock.when(LevelFactory::getInstance)
                    .thenReturn(levelFactory);
            gridRecord = RadarRequestableData.buildGridRecord(record, RR,
                    inputTime);
        }

        assertEquals("radar-koax", gridRecord.getDatasetId());
        assertEquals(coverage, gridRecord.getLocation());
        assertEquals(new Parameter(RR, "", RadarUnits.DBZ),
                gridRecord.getParameter());
        assertEquals(expectedOutputTime, gridRecord.getDataTime());
        assertEquals(getLevel(1.5), gridRecord.getLevel());
    }

    @ParameterizedTest
    // Just a couple different sets of values
    @CsvSource({ "radar-koax,RR,dBZ,3.4", "radar-kdmx,RRV,kts,19.5" })
    void testConstructor(String datasetId, String paramAbbrev, String unitStr,
            double tilt) throws VizException {
        /*
         * Verify radar record source gets wrapped in a DefaultVizRadarRecord
         * and saved as a field, and that values from grid record get pulled out
         * and set as fields on the RadarRequestableData.
         */
        // Setup DBZ unit
        RadarUnits.register();

        DataTime time = new DataTime(DT_STR);
        time.setLevel(tilt, TILT);

        RadarRecord record = new RadarRecord();
        record.setDataTime(time);
        RadarRequestableData rrd;
        try (MockedStatic<RadarRequestableData> radarAdapterStaticMock = getMockedStatic(
                datasetId, unitStr, tilt, coverage)) {
            rrd = new RadarRequestableData(record, paramAbbrev);
        }

        Unit<?> unit = SimpleUnitFormat
                .getInstance(SimpleUnitFormat.Flavor.ASCII)
                .parseProductUnit(unitStr, new ParsePosition(0));

        assertTrue(rrd.radarSource instanceof DefaultVizRadarRecord);
        assertEquals(new DefaultVizRadarRecord(record), rrd.radarSource);
        assertEquals(datasetId, rrd.getSource());
        assertSame(coverage, rrd.getSpace());
        assertEquals(paramAbbrev, rrd.getParameter());
        assertEquals(unit, rrd.getUnit());
        assertEquals(new DataTime(DT_STR), rrd.getDataTime());
        assertEquals(getLevel(tilt), rrd.getLevel());
    }

    private static Level getLevel(double tilt) {
        Level level = new Level();
        level.setMasterLevel(new MasterLevel(TILT));
        level.setLevelonevalue(tilt);
        return level;
    }

    /**
     * Get a mocked static that mocks buildGridRecord to build a grid record
     * using the given values.
     *
     * @return mocked static
     */
    public static MockedStatic<RadarRequestableData> getMockedStatic(
            String datasetId, String unitStr, double tilt,
            GridCoverage coverage) {
        MockedStatic<RadarRequestableData> mockedStatic = mockStatic(
                RadarRequestableData.class);
        /*
         * Mock RadarRequestableData.buildGridRecord to return a grid record for
         * the paramAbbrev/time that are passed into it.
         */
        mockedStatic.when(
                () -> RadarRequestableData.buildGridRecord(any(), any(), any()))
                .then(invocation -> {
                    String paramAbbrev = invocation.getArgument(1);
                    DataTime time = invocation.getArgument(2);

                    GridRecord gridRecord = new GridRecord();
                    gridRecord.setDatasetId(datasetId);
                    gridRecord.setLocation(coverage);
                    gridRecord.setParameter(
                            new Parameter(paramAbbrev, "", unitStr));
                    gridRecord.setDataTime(time);
                    gridRecord.setLevel(getLevel(tilt));

                    return gridRecord;
                });
        return mockedStatic;
    }
}
