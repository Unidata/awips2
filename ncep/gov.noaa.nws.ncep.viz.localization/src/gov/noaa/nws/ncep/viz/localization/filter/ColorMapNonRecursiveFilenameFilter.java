package gov.noaa.nws.ncep.viz.localization.filter;

import java.io.File;
import java.io.FilenameFilter;

public class ColorMapNonRecursiveFilenameFilter implements FilenameFilter {

	@Override
	public boolean accept(File dir, String name) {
		boolean filterResult = false; 
		if(dir != null) {
			if(name != null && name.trim().length() != 0) {
				if(!name.startsWith(".") && hasCorrectFileExtension(name, ".cmap"))
					filterResult = true; 
			}
		}
		return filterResult;
	}

	private boolean hasCorrectFileExtension(String filename, String fileExtension) {
		boolean checkResult = false; 
		if(filename.endsWith(fileExtension))
			checkResult = true; 
		return checkResult;
	}

}
