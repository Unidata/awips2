package gov.noaa.nws.ncep.viz.localization.impl;

import java.util.Map;

import gov.noaa.nws.ncep.viz.localization.impl.ILoadedFileResourceInfo;

public interface IMultiResourceFileInfoLoader<T> {
//	T[] loadMultiFileInfoAsArray(ILoadedFileResourceInfo<T> fileInfo); 
	Map<String, T> loadMultiFileInfo(ILoadedFileResourceInfo<T> fileInfo); 
}
