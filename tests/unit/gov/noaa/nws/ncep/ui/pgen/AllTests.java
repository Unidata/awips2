package gov.noaa.nws.ncep.ui.pgen;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
 
// suites ONLY

@RunWith(Suite.class)
@Suite.SuiteClasses({
	gov.noaa.nws.ncep.ui.pgen.display.DisplaySuite.class,
	gov.noaa.nws.ncep.ui.pgen.elements.ElementsSuite.class,
	gov.noaa.nws.ncep.ui.pgen.gfa.GfaSuite.class
})
public class AllTests {
    // the class remains completely empty, 
    // being used only as a holder for the above annotations
}