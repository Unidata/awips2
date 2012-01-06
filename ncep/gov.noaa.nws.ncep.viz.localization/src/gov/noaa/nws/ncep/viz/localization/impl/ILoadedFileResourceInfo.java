package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.impl.IMultiResourceFileInfoLoader;
import gov.noaa.nws.ncep.viz.localization.impl.ISingleResourceFileInfoLoader;

import java.io.FilenameFilter;
import java.util.Map;

public interface ILoadedFileResourceInfo<T> {
	String getIndividualFilename(); 
	String getLocalPath(); 
	String getBasePath(); 
	String getSitePath(); 
	String getDeskPath(); 
	String getUserPath(); 
	String getUserSiteDeskPath(); 
	String getUserSitePath(); 
	String getIndividualDirPortion(); 
	int getResourceNamePatternMatchStyle(); 
	int getLocalizationLevel(); 
	String getResourceNamePattern(); 
	void setLocalizationLevel(int localizationLevel); 
	FilenameFilter getFilenameFilter(); 
//	T[] doFileInfoLoading(IMultiResourceFileInfoLoader<T> fileInfoLoader); 
	Map<String, T> doFileInfoLoading(IMultiResourceFileInfoLoader<T> fileInfoLoader); 
	T doFileInfoLoading(ISingleResourceFileInfoLoader<T> fileInfoLoader); 
}
