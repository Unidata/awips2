package gov.noaa.nws.ncep.viz.localization.filter;

import gov.noaa.nws.ncep.viz.localization.StringUtil;

import java.io.File;
import java.io.FilenameFilter;

public class DefaultFilenameFilter implements FilenameFilter {

	@Override
	public boolean accept(File dir, String name) {
		boolean filterResult = false; 
		if(!StringUtil.isStringEmpty(name) && !name.startsWith(".")) {
			filterResult = true; 
		}
		return filterResult;
	}

}
