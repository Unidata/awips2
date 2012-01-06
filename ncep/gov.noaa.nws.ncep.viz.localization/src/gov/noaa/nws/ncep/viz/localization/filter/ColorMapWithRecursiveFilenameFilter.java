package gov.noaa.nws.ncep.viz.localization.filter;

import java.io.File;
import java.io.FilenameFilter;

public class ColorMapWithRecursiveFilenameFilter implements FilenameFilter {
	@Override
	public boolean accept(File dir, String name) {
		boolean filterResult = false; 
		if(dir != null && !isStringEmpty(name)) {
//			if(dir.isDirectory() && !name.startsWith("."))
//				filterResult = true; 
//			else if(!name.startsWith(".") && hasCorrectFileExtension(name, ".cmap"))
//					filterResult = true; 
			if(!name.startsWith(".")) {
				
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

	private boolean isStringEmpty(String str) {
		boolean result = false; 
		if(str == null || str.trim().length() == 0)
			result = true; 
		return result; 
	}

}
