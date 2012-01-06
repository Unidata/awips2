package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.impl.AbstractMultiFileDirectoryInfoLoader;

import java.io.FilenameFilter;
import java.io.File; 
import java.util.Map;

public class DirectoryLoaderImpl extends AbstractMultiFileDirectoryInfoLoader<File> {

	@Override
	protected boolean loadAvailableFiles(String fileParentPathValue, String resourceNamePattern,
			int patternMatchStyle, 
			FilenameFilter filenameFilter, Map<String, File> fileMap) {
		String[] filenameArray = null; 
		boolean isLoaded = false; 
		File file = new File(fileParentPathValue); 
		if(file != null && file.exists()) {
			isLoaded = true; 
			filenameArray = file.list(filenameFilter); 
		}
		
		if(filenameArray == null)
			return isLoaded; 
		
		for(String currentFilename : filenameArray) {
			String currentFileFullPath = fileParentPathValue + File.separator + currentFilename; 
			File currentFile = new File(currentFileFullPath);
			if(currentFile != null) {
//				if(isFileGoodToBeAddedToMap(currentFilename, fileMap)) {
					fileMap.put(currentFilename, currentFile); 
//				}
			}
		}
		return isLoaded; 
	}
	
//	private boolean isFileGoodToBeAddedToMap(String mapKeyValue, Map<String, File> fileMap) {
//		boolean isFileOKToBeAdded = false; 
//		if(fileMap != null && !StringUtil.isStringEmpty(mapKeyValue)) {
//			if(!fileMap.containsKey(mapKeyValue)) {
//				isFileOKToBeAdded = true; 
//			}
//		}
//		return isFileOKToBeAdded; 
//	}
}
