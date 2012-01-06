package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.StringUtil;
import gov.noaa.nws.ncep.viz.localization.impl.ILoadedFileResourceInfo;
import gov.noaa.nws.ncep.viz.localization.impl.IMultiResourceFileInfoLoader;

import java.io.File;
import java.io.FilenameFilter;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public abstract class AbstractMultiFileDirectoryInfoLoader<T> implements IMultiResourceFileInfoLoader<T> {

//	@Override
//	public T[] loadMultiFileInfoAsArray(ILoadedFileResourceInfo<T> fileInfo) {
//		return getAvailableMultiFileInfoInArray(fileInfo);
//	}

	@Override
	public Map<String, T> loadMultiFileInfo(ILoadedFileResourceInfo<T> fileInfo) {
		return getAvailableMultiFileInfoMap(fileInfo);
	}

	private T[] getAvailableMultiFileInfoInArray(ILoadedFileResourceInfo<T> fileInfo) {
		Map<String, T> fileInfoMap = getAvailableMultiFileInfoMap(fileInfo); 
		Collection<T> collection = fileInfoMap.values(); 
		T[] genericArray = convertToArray(collection); 
		if(genericArray != null)
			Arrays.sort(genericArray);
		return genericArray; 

	}
	private Map<String, T> getAvailableMultiFileInfoMap(ILoadedFileResourceInfo<T> fileInfo) {
		String localFilePath = fileInfo.getLocalPath(); 
		String baseFilePath = fileInfo.getBasePath(); 
		String deskFilePath = fileInfo.getDeskPath(); 
		String siteFilePath = fileInfo.getSitePath(); 
		String userFilePath = fileInfo.getUserPath(); 
		String userSiteFilePath = fileInfo.getUserSitePath(); 
		String userSiteDeskFilePath = fileInfo.getUserSiteDeskPath(); 

		if(!StringUtil.isStringEmpty(fileInfo.getIndividualDirPortion())) {
			localFilePath = localFilePath + File.separator + fileInfo.getIndividualDirPortion(); 
			baseFilePath = baseFilePath + File.separator + fileInfo.getIndividualDirPortion(); 
			deskFilePath = deskFilePath + File.separator + fileInfo.getIndividualDirPortion(); 
			siteFilePath = siteFilePath + File.separator + fileInfo.getIndividualDirPortion(); 
			userFilePath = userFilePath + File.separator + fileInfo.getIndividualDirPortion(); 
			userSiteFilePath = userSiteFilePath + File.separator + fileInfo.getIndividualDirPortion(); 
			userSiteDeskFilePath = userSiteDeskFilePath + File.separator + fileInfo.getIndividualDirPortion(); 
		}
		
		HashMap<String, T> fileInfoMap = new HashMap<String, T>(); 
		FilenameFilter filenameFilter = fileInfo.getFilenameFilter(); 
		String resourceNamePattern = fileInfo.getResourceNamePattern(); 
		int resourceNamePatternMatchStyle = fileInfo.getResourceNamePatternMatchStyle(); 
		
		boolean isLoaded = false; 
		switch(fileInfo.getLocalizationLevel()) {
			/*
			 * Single combination, the total number is 8
			 */
			case LocalizationConstants.DEFAULT: 
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break;
			case LocalizationConstants.BASE:
				loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL:
				if(!loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK:
				if(!loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.SITE:
				if(!loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.USER:
				if(!loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.USER_SITE:
				if(!loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.USER_SITE_DESK:
				if(!loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			/*
			 * double combination, the total number is 21 = (7!)/(2! * (7-2)!)
			 */
			case (LocalizationConstants.LOCAL | LocalizationConstants.BASE):
				if(!loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.DESK):
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.SITE):
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.USER):
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.USER_SITE):
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.USER_SITE_DESK):
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			
			case (LocalizationConstants.BASE | LocalizationConstants.DESK):
				if(!loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE:
				if(!loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER:
				if(!loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER_SITE:
				if(!loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER_SITE_DESK:
				if(!loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap))
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
	
			case LocalizationConstants.DESK | LocalizationConstants.SITE:
				isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER_SITE:
				isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER_SITE_DESK:
				isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.SITE | LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
				isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
				isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.USER | LocalizationConstants.USER_SITE:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
				isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			/*
			 * triple combination, the total number is 35 = (7!)/(3! * (7-3)!)
			 */
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
					LocalizationConstants.DESK:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
					LocalizationConstants.SITE:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
					LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
					LocalizationConstants.SITE:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
					LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
					LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
	
			case LocalizationConstants.LOCAL | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the triple combination starting with "LOCAL", the total is 15 
			 */
		
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
					LocalizationConstants.SITE:
				isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
					LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
					LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.BASE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the triple combination starting with "BASE", the total is 10 
			 */

			case LocalizationConstants.DESK | LocalizationConstants.SITE |
					LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
		
			case LocalizationConstants.DESK | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the triple combination starting with "DESK", the total is 6 
			 */
			
			case LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the triple combination starting with "SITE", the total is 3 
			 */

			case LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the triple combination starting with "USER", the total is 1 
			 */
			/*
			 * End of the triple combination, the total is 35 
			 */
			
			/*
			 * quadruple combination, the total number is 35 = (7!)/(4! * (7-4)!)
			 */
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
					LocalizationConstants.DESK | LocalizationConstants.SITE:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
					LocalizationConstants.DESK | LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
					LocalizationConstants.SITE | LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/BASE", the total is 10 
			 */
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
					LocalizationConstants.SITE | LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/DESK", the total is 6 
			 */

			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/SITE", the total is 3 
			 */
			
			case LocalizationConstants.LOCAL | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/USER", the total is 1 
			 */
			/*
			 * End of the quadruple combination starting with "LOCAL", the total is 20 
			 */
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
					LocalizationConstants.SITE | LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "BASE/DESK", the total is 6 
			 */
			
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "BASE/SITE", the total is 3 
			 */

			case LocalizationConstants.BASE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "BASE/USER", the total is 1 
			 */
			/*
			 * End of the quadruple combination starting with "BASE", the total is 10 
			 */

			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "DESK/SITE", the total is 3 
			 */
			
			case LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "DESK/USER", the total is 1 
			 */
			/*
			 * End of the quadruple combination starting with "DESK", the total is 4 
			 */
			
			case LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quadruple combination starting with "SITE/USER", the total is 1 
			 */
			/*
			 * End of the quadruple combination, the total is 35 
			 */
			
			
			/*
			 * Quintuple combination, the total number is 21 = (7!)/(5! * (7-5)!)
			 */
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
					LocalizationConstants.DESK | LocalizationConstants.SITE |
					LocalizationConstants.USER:
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
		
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quintuple combination starting with "LOCAL/BASE", the total is 10 
			 */
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
	
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
		
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quintuple combination starting with "LOCAL/DESK", the total is 4 
			 */
	
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quintuple combination starting with "LOCAL/SITE", the total is 1 
			 */
			/*
			 * End of the quintuple combination starting with "LOCAL", the total is 15 
			 */
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
	
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 

			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quintuple combination starting with "BASE", the total is 5 
			 */

			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the quintuple combination starting with "DESK", the total is 1 
			 */
			
			/*
			 * End of the quintuple combination, the total is 21 
			 */

			/*
			 * Sextuple combination, the total number is 7 = (7!)/(6! * (7-6)!)
			 */
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
		
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
	
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
					if(!isLoaded)
						isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
			break; 
			/*
			 * End of the Sextuple combination, the total is 7 
			 */
			
			
			default: 
				isLoaded = loadAvailableFiles(localFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteDeskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(userSiteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(deskFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(siteFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
				if(!isLoaded)
					isLoaded = loadAvailableFiles(baseFilePath, resourceNamePattern, resourceNamePatternMatchStyle, filenameFilter, fileInfoMap); 
		}
		
//		Collection<T> collection = fileInfoMap.values(); 
//		T[] genericArray = convertToArray(collection); 
//		if(genericArray != null)
//			Arrays.sort(genericArray);
//		return genericArray; 
		return fileInfoMap; 
	}
	
	protected abstract boolean loadAvailableFiles(String fileParentPathValue, String resourceNamePattern, int patternMatchStyle, FilenameFilter filenameFilter, Map<String, T> fileMap); 

	@SuppressWarnings("unchecked")
	private T[] convertToArray(Collection<T> collection) {
		if(collection == null)
			return null; 
		T object = getGenericObject(collection); 
		if(object == null)
			return null; 
		
		T[] array = (T[])Array.newInstance(object.getClass(), collection.size()); 
		populateArray(array, collection); 
		return array; 
	}
	
	private T getGenericObject(Collection<T> collection) {
		Iterator<T> itr = collection.iterator(); 
		T object = null; 
		if(itr.hasNext())
			object = itr.next(); 
		return object; 
	}
	
	private void populateArray(T[] array, Collection<T> collection) {
		if(collection == null)
			return; 
		int arrayIndex = 0; 
		for(T currentT : collection) {
			array[arrayIndex++] = currentT; 
		}
	}
}
