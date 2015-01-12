package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;

import org.junit.Ignore;
import org.junit.Test;

//TODO fix?
@Ignore
public class NcUairPressureHeightGroupTest {

    public void setUp() throws Exception {
    }

    @Test
    public void testPressHeightField() {

        String presgroup = "00104";
        Boolean above = false;
        int level = 1;
        String stationNumber = "72403";
        String dataType = "TTAA";
        NcUairRecord record = null;
        NcUairPressureHeightGroup.PressureHeightField(presgroup, above, level,
                stationNumber, dataType, record);
        float height = NcUairPressureHeightGroup.getHeight();
        float pres = NcUairPressureHeightGroup.getPressure();
        assertEquals(104.0, height);
        assertEquals(1000.0, pres);
    }

}