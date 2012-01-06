package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.LocalizationUtil;
import gov.noaa.nws.ncep.viz.localization.StringUtil;
import gov.noaa.nws.ncep.viz.localization.impl.AbstractMultiFileInfoLoader;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class MultiFilenameLoaderImpl extends AbstractMultiFileInfoLoader<String> {

	@Override
	protected void loadAvailableFiles(String fileParentPathValue, String resourceNamePattern, 
			int patternMatchStyle, 
			FilenameFilter filenameFilter, Map<String, String> fileMap) {
		String[] fileNameArray = null; 
		File file = new File(fileParentPathValue); 
		if(file != null) {
			fileNameArray = file.list(filenameFilter); 
		}
		
		if(fileNameArray == null)
			return; 
		List<String> directoryList = new ArrayList<String>(); 

		for(String currentFilename : fileNameArray) {
			String currentFileFullPath = fileParentPathValue + File.separator + currentFilename; 
			File currentFile = new File(currentFileFullPath);
			if(currentFile != null) {
				if(currentFile.isDirectory()) {
					directoryList.add(currentFileFullPath); 
//					loadAvailableFiles(currentFileFullPath, resourceNamePattern, filenameFilter, fileMap);
				} else if(LocalizationUtil.isMatched(currentFilename, resourceNamePattern, patternMatchStyle)) {
					if(!fileMap.containsKey(currentFilename)) 
						fileMap.put(currentFilename, currentFileFullPath); 
				}
			}
		}
		/*
		 * Now start recursive searching
		 */
		for(String eachDirFullPath : directoryList) {
			loadAvailableFiles(eachDirFullPath, resourceNamePattern, patternMatchStyle, filenameFilter, fileMap);
		}
	}

}
