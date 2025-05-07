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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.util.List;

import javax.measure.UnitConverter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionImage;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;

import tech.units.indriya.function.MultiplyConverter;

/**
 *
 * Unit tests for {@link CrossSectionImageResource}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2024 2037476    bines       Initial creation
 * Aug 06, 2024 2037698    bines       dataMappingConversion test
 * Aug 20, 2024 2037631    mapeters    Test getFrameRenderable()
 *
 * </pre>
 *
 * @author bines
 */
@ExtendWith(MockitoExtension.class)
class TestCrossSectionImageResource {

    private static final float[] data = { -32.5f, -20.0f, 75.0f, 94.5f };

    private static final List<float[]> dataList = List.of(data);

    private CrossSectionImageResource csir;

    private CrossSectionImageResource.ImageDataCallback idc;

    private UnitConverter converter = MultiplyConverter.of(2);

    private DataTime dt1 = new DataTime("2024-03-26_12:00:00.0");

    private DataTime dt2 = new DataTime("2024-03-26_12:06:00.0");

    @Mock
    private CrossSectionImage image1;

    @Mock
    private CrossSectionImage image2;

    @BeforeEach
    public void setupBefore() throws VizException {
        DataTime dt = new DataTime("2024-03-26_12:00:00.0");

        CrossSectionResourceData rscData = mock(CrossSectionResourceData.class);
        LoadProperties props = new LoadProperties();
        AbstractCrossSectionAdapter<?> adapter = mock(
                AbstractCrossSectionAdapter.class);

        csir = spy(new CrossSectionImageResource(rscData, props, adapter));
        idc = csir.new ImageDataCallback(dt, dataList);
    }

    @Test
    public void testConvertDataToColorMap() throws VizException {
        // Check if converter returns correct converted data

        ColorMapCapability cmCap = mock(ColorMapCapability.class);
        ColorMapParameters cmapp = mock(ColorMapParameters.class);
        doReturn(true).when(csir).hasCapability(ColorMapCapability.class);
        doReturn(cmCap).when(csir).getCapability(any());
        doReturn(cmapp).when(cmCap).getColorMapParameters();
        doReturn(converter).when(cmapp).getDisplayToColorMapConverter();

        float[] expectedResult = new float[data.length];
        for (int i = 0; i < data.length; i++) {
            expectedResult[i] = (float) converter.convert(data[i]);
        }

        float[] convertedData = idc.convertDataToColorMap(data);

        assertArrayEquals(expectedResult, convertedData);
    }

    @Test
    public void testDataMappingConversion() {
        // Check if converter calls the correct functions and converts val

        String actualResult = "Rain";
        double testVal = 2.0;
        double convertedTestVal = converter.convert(testVal);

        ColorMapCapability cmCap = mock(ColorMapCapability.class);
        ColorMapParameters cmapp = mock(ColorMapParameters.class);
        DataMappingPreferences dmapp = mock(DataMappingPreferences.class);
        doReturn(true).when(csir).hasCapability(ColorMapCapability.class);
        doReturn(cmCap).when(csir).getCapability(any());
        doReturn(cmapp).when(cmCap).getColorMapParameters();
        doReturn(converter).when(cmapp).getDisplayToColorMapConverter();
        doReturn(dmapp).when(cmapp).getDataMapping();
        doReturn(actualResult).when(dmapp)
                .getSampleOrLabelValueForDataValue(convertedTestVal);

        String convertedResult = csir.dataMappingConversion(testVal);

        assertTrue(actualResult.equals(convertedResult));
    }

    @Test
    public void testGetFrameRenderable1() throws VizException {
        // No images for time -> null returned
        CrossSectionImage actualResult = csir.getFrameRenderable(dt1);

        assertNull(actualResult);
    }

    @Test
    public void testGetFrameRenderable2() throws VizException {
        // Verify that corresponding image mappings are returned
        csir.imageMap.put(dt1, image1);
        csir.imageMap.put(dt2, image2);

        CrossSectionImage actualImage1 = csir.getFrameRenderable(dt1);
        CrossSectionImage actualImage2 = csir.getFrameRenderable(dt2);

        assertSame(image1, actualImage1);
        assertSame(image2, actualImage2);
    }
}
