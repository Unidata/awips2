package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import static org.junit.Assert.assertEquals;

import org.junit.Ignore;
import org.junit.Test;

//TODO fix?
@Ignore
public class NcUairWindGroupTest {

    public void setUp() throws Exception {
    }

    @Test
    public void testWindField() {
        String windgroup = "27049";
        NcUairWindGroup.WindField(windgroup, false);
        float windSpeed = NcUairWindGroup.getSped();
        float winddir = NcUairWindGroup.getDrct();
        assertEquals(49.0, windSpeed);
        assertEquals(270.0, winddir);
    }
}