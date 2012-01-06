package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.LocalizationUtil;
import gov.noaa.nws.ncep.viz.localization.impl.AbstractMultiFileInfoLoader;

import java.io.FilenameFilter;
import java.io.File; 
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class MultiFileLoaderImpl extends AbstractMultiFileInfoLoader<File> {

	@Override
	protected void loadAvailableFiles(String fileParentPathValue, String resourceNamePattern,
			int patternMatchStyle, 
			FilenameFilter filenameFilter, Map<String, File> fileMap) {
		String[] filenameArray = null; 
		File file = new File(fileParentPathValue); 
		if(file != null) {
			filenameArray = file.list(filenameFilter); 
		}
		
		if(filenameArray == null)
			return; 
		
		List<String> directoryList = new ArrayList<String>(); 
		for(String currentFilename : filenameArray) {
			String currentFileFullPath = fileParentPathValue + File.separator + currentFilename; 
			File currentFile = new File(currentFileFullPath);
			if(currentFile != null) {
				if(currentFile.isDirectory()) {
					directoryList.add(currentFileFullPath); 
//					loadAvailableFiles(currentFileFullPath, resourceNamePattern, filenameFilter, fileMap);
				} else if(LocalizationUtil.isMatched(currentFilename, resourceNamePattern, patternMatchStyle)) {
					if(!fileMap.containsKey(currentFilename)) 
						fileMap.put(currentFilename, currentFile); 
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
