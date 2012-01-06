package gov.noaa.nws.ncep.viz.localization.filter;

import java.io.File;
import java.io.FilenameFilter;

public class DummyFilenameFilter implements FilenameFilter {

	@Override
	public boolean accept(File dir, String name) {
		return true;
	}

}
