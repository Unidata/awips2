package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.impl.AbstractSingleFileInfoLoader;

import java.io.File; 

public class SingleFileLoaderImpl extends AbstractSingleFileInfoLoader<File> {
	@Override
	protected File loadAvailableSingleFileInfo(String filePathValue) {
		if(filePathValue != null) {
			File file = new File(filePathValue); 
			if(file != null && file.exists())
				return file; 
		}
		return null; 
	}

}
