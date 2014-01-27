package gov.noaa.nws.ncep.ui.pgen.gfa;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

//TODO fix?
@Ignore
public class GfaInfoTest {

    @Before
    public void setUp() throws Exception {
        GfaFormatTest.configure();
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testCycle() {
        assertEquals(GfaFormatTest.CYCLE_DAY, PgenCycleTool.getCycleDay());
        assertEquals(GfaFormatTest.CYCLE_HOUR, PgenCycleTool.getCycleHour());
    }

    @Test
    public void getDocument() {
        // reads the gfa configuration file
        assertNotNull(GfaInfo.getDocument());
    }
}
