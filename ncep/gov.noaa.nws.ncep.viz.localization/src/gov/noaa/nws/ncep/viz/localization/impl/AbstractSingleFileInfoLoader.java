package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.StringUtil;
import gov.noaa.nws.ncep.viz.localization.impl.ILoadedFileResourceInfo;
import gov.noaa.nws.ncep.viz.localization.impl.ISingleResourceFileInfoLoader;

import java.io.File;

public abstract class AbstractSingleFileInfoLoader<T> implements ISingleResourceFileInfoLoader<T> {

	@Override
	public T loadSingleFielInfo(ILoadedFileResourceInfo<T> fileInfo) {
		return getAvailableSingleFileInfo(fileInfo);
	}

	private T getAvailableSingleFileInfo(ILoadedFileResourceInfo<T> fileInfo) {
		String absoluteLocalFilePathOnly = fileInfo.getLocalPath(); 
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
		
		boolean isResourceAFile = false; 
		if(!StringUtil.isStringEmpty(fileInfo.getIndividualFilename())) {
			isResourceAFile = true; 
			absoluteLocalFilePathOnly = absoluteLocalFilePathOnly + File.separator + fileInfo.getIndividualFilename(); 
			localFilePath = localFilePath + File.separator + fileInfo.getIndividualFilename(); 
			baseFilePath = baseFilePath + File.separator + fileInfo.getIndividualFilename(); 
			deskFilePath = deskFilePath + File.separator + fileInfo.getIndividualFilename(); 
			siteFilePath = siteFilePath + File.separator + fileInfo.getIndividualFilename(); 
			userFilePath = userFilePath + File.separator + fileInfo.getIndividualFilename(); 
			userSiteFilePath = userSiteFilePath + File.separator + fileInfo.getIndividualFilename(); 
			userSiteDeskFilePath = userSiteDeskFilePath + File.separator + fileInfo.getIndividualFilename(); 
		}
		
		
		T singleFileInfo = null; 
		switch(fileInfo.getLocalizationLevel()) {
			/*
			 * Single combination, the total number is 8
			 */
			case LocalizationConstants.DEFAULT: 
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE:
				singleFileInfo = loadAvailableSingleFileInfo(baseFilePath);  
			break; 
			case LocalizationConstants.DESK:
				singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.SITE:
				singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.USER_SITE:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			/*
			 * double combination, the total number is 21 = (7!)/(2! * (7-2)!)
			 */
			case (LocalizationConstants.LOCAL | LocalizationConstants.BASE):
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.DESK):
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.SITE):
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.USER):
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.USER_SITE):
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case (LocalizationConstants.LOCAL | LocalizationConstants.USER_SITE_DESK):
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case (LocalizationConstants.BASE | LocalizationConstants.DESK):
				singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE:
				singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER_SITE:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case (LocalizationConstants.DESK | LocalizationConstants.SITE):
				singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER_SITE:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.SITE | LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.USER | LocalizationConstants.USER_SITE:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			/*
			 * triple combination, the total number is 35 = (7!)/(3! * (7-3)!)
			 */
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER_SITE_DESK:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE_DESK:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the triple combination starting with "LOCAL", the total is 15 
			 */
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE:
				singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
					LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the triple combination starting with "BASE", the total is 10 
			 */
			
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
					LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
	
			case LocalizationConstants.DESK | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the triple combination starting with "DESK", the total is 6 
			 */
	
			case LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
	
			case LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the triple combination starting with "SITE", the total is 3 
			 */
	
			case LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
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
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/BASE", the total is 10 
			 */
		
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER:
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
		
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/DESK", the total is 6 
			 */

			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
		
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/SITE", the total is 3 
			 */
			
			case LocalizationConstants.LOCAL | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "LOCAL/USER", the total is 1 
			 */
			/*
			 * End of the quadruple combination starting with "LOCAL", the total is 20 
			 */
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER:
				singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "BASE/DESK", the total is 6 
			 */
		
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "BASE/SITE", the total is 3 
			 */
			
			case LocalizationConstants.BASE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "BASE/USER", the total is 1 
			 */
			/*
			 * End of the quadruple combination starting with "BASE", the total is 10 
			 */

			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
		
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "DESK/SITE", the total is 3 
			 */
	
			case LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quadruple combination starting with "DESK/USER", the total is 1 
			 */
			/*
			 * End of the quadruple combination starting with "DESK", the total is 4 
			 */

			case LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
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
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quintuple combination starting with "LOCAL/BASE", the total is 10 
			 */

			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quintuple combination starting with "LOCAL/DESK", the total is 4 
			 */
	
			case LocalizationConstants.LOCAL | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
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
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
		
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			
			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the quintuple combination starting with "BASE", the total is 5 
			 */
		
			case LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE |
				LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
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
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.SITE |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.DESK | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.BASE |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.LOCAL | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					if(isResourceAFile) {
						singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
						if(singleFileInfo == null)
							singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
					}
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 

			case LocalizationConstants.BASE | LocalizationConstants.DESK |
				LocalizationConstants.SITE | LocalizationConstants.USER |
				LocalizationConstants.USER_SITE | LocalizationConstants.USER_SITE_DESK:
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
			break; 
			/*
			 * End of the Sextuple combination, the total is 7 
			 */
			
			default: 
				if(isResourceAFile) {
					singleFileInfo = loadAvailableSingleFileInfo(absoluteLocalFilePathOnly);  
					if(singleFileInfo == null)
						singleFileInfo = loadAvailableSingleFileInfo(localFilePath); 
				}
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteDeskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(userSiteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(deskFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(siteFilePath); 
				if(singleFileInfo == null)
					singleFileInfo = loadAvailableSingleFileInfo(baseFilePath); 
		}
		
		return singleFileInfo; 
	}
	
	protected abstract T loadAvailableSingleFileInfo(String filePathValue); 

}
