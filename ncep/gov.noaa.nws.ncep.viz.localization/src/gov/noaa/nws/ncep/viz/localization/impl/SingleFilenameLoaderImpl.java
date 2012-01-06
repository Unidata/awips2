package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.impl.AbstractSingleFileInfoLoader;

import java.io.File;

public class SingleFilenameLoaderImpl extends AbstractSingleFileInfoLoader<String> {

	@Override
	protected String loadAvailableSingleFileInfo(String filePathValue) {
		String filename = null; 
		if(filePathValue != null) {
			File file = new File(filePathValue); 
			if(file != null && file.exists())
				filename = filePathValue; 
		}
		return filename; 
	}

}
