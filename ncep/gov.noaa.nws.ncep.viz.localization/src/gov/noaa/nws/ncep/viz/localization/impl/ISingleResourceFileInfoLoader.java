package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.impl.ILoadedFileResourceInfo;

public interface ISingleResourceFileInfoLoader<T> {
	T loadSingleFielInfo(ILoadedFileResourceInfo<T> fileInfo);
}
