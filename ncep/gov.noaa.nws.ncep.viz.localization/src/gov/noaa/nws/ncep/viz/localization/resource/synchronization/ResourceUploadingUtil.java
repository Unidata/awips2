package gov.noaa.nws.ncep.viz.localization.resource.synchronization;

import java.io.File;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;

import gov.noaa.nws.ncep.viz.localization.Activator;
import gov.noaa.nws.ncep.viz.localization.ResourceBundleUtil;
import gov.noaa.nws.ncep.viz.localization.StringUtil;
//import gov.noaa.nws.ncep.viz.localization.adapter.NcepCAVELocalizationAdapter;

public class ResourceUploadingUtil {
	private static ResourceUploadingUtil instance; 
	
	private CAVELocalizationAdapter caveLocalizationAdapter; 
//	private NcepCAVELocalizationAdapter caveLocalizationAdapter; 
	
	private String [] topLevelResourcesDirArray; 
	
	static {
		instance = new ResourceUploadingUtil(); 
	}
	
	private ResourceUploadingUtil() {}; 
	
	public static ResourceUploadingUtil getInstance() {
		return instance; 
	}
	
	public void uploadActiveSiteAndDesk() {
		uploadActiveSiteOnly(); 
		uploadActiveDeskOnly(); 
	}
	
	public void uploadActiveSiteOnly() {
		LocalizationContext caveNcepSiteLevelLocalizationContext = Activator.getDefault().getCaveNcepSiteLevelLocalizationContext(); 
		CAVELocalizationAdapter localizationAdapter = getCAVELocalizationAdapter(); 
//		NcepCAVELocalizationAdapter localizationAdapter = getCAVELocalizationAdapter(); 
		
		String [] topResourcesDirArray = getTopLevelResourcesDirArray();
		String [] topAbsoluteFileArray = getAbsoluteFilePathArray(caveNcepSiteLevelLocalizationContext, 
				localizationAdapter, topResourcesDirArray); 
		
		doFileUploading(caveNcepSiteLevelLocalizationContext, localizationAdapter, topAbsoluteFileArray); 
	}
	
	public void uploadActiveDeskOnly() {
		LocalizationContext caveNcepDeskLevelLocalizationContext = Activator.getDefault().getCaveNcepDeskLevelLocalizationContext(); 
		CAVELocalizationAdapter localizationAdapter = getCAVELocalizationAdapter(); 
//		NcepCAVELocalizationAdapter localizationAdapter = getCAVELocalizationAdapter(); 
		
		String [] topResourcesDirArray = getTopLevelResourcesDirArray();
		String [] topAbsoluteFileArray = getAbsoluteFilePathArray(caveNcepDeskLevelLocalizationContext, 
				localizationAdapter, topResourcesDirArray); 

		doFileUploading(caveNcepDeskLevelLocalizationContext, localizationAdapter, topAbsoluteFileArray); 
	}
	
	private String[] getAbsoluteFilePathArray(LocalizationContext localizationContext, 
			CAVELocalizationAdapter localizationAdapter, String[] fileNameArray) {
//	private String[] getAbsoluteFilePathArray(LocalizationContext localizationContext, 
//			NcepCAVELocalizationAdapter localizationAdapter, String[] fileNameArray) {
		String [] absoluteFilePathArray = null; 
		if(fileNameArray != null) {
			absoluteFilePathArray = new String[fileNameArray.length]; 
			for(int i=0; i<fileNameArray.length; i++) {
				String eachFileName = fileNameArray[i]; 
				File eachFile = localizationAdapter.getPath(localizationContext, eachFileName.trim());
//				if(eachFile != null) {
//					System.out.println("=======, eachFile is not NULL, eachFile.getAbsolutePath()="+eachFile.getAbsolutePath()); 
//					if(eachFile.exists()) 
//						System.out.println("=======, eachFile does exist, eachFile.getAbsolutePath()="+eachFile.getAbsolutePath()); 
//					else
//						System.out.println("=======, eachFile does NOTNOTNOT exist, eachFile.getAbsolutePath()="+eachFile.getAbsolutePath()); 
//				} else {
//					System.out.println("=======, eachFile is NULL!!!!!!!!!!!"); 
//				}
					
				if(eachFile != null && eachFile.exists())
					absoluteFilePathArray[i] = eachFile.getAbsolutePath(); 
			}
		}
			
		return absoluteFilePathArray; 
	}
	
	private CAVELocalizationAdapter getCAVELocalizationAdapter() {
		if(caveLocalizationAdapter == null)
			caveLocalizationAdapter = new CAVELocalizationAdapter(); 
		return caveLocalizationAdapter; 
	}
//	private NcepCAVELocalizationAdapter getCAVELocalizationAdapter() {
//		if(caveLocalizationAdapter == null)
//			caveLocalizationAdapter = new NcepCAVELocalizationAdapter(); 
//		return caveLocalizationAdapter; 
//	}
	
	private String[] getTopLevelResourcesDirArray() {
		if(topLevelResourcesDirArray == null)
			topLevelResourcesDirArray = ResourceBundleUtil.getNcepResourceDirInfo(",");
		return topLevelResourcesDirArray; 
	}
	
	private void doFileUploading(LocalizationContext localizationContext, CAVELocalizationAdapter localizationAdapter, 
//	private void doFileUploading(LocalizationContext localizationContext, NcepCAVELocalizationAdapter localizationAdapter, 
			String[] absoluteFilePathArray) {
		if(absoluteFilePathArray == null)
			return; 
		for(String eachAbsoluteFilePath : absoluteFilePathArray) {
			File eachFile = constructFile(eachAbsoluteFilePath); 
			if(eachFile != null && eachFile.exists()) {
				if(eachFile.isDirectory()) {
					String [] subFileNameArray = eachFile.list(); 
					String [] absoluteSubFilePathArray = constructAbsoluteFilePathArray(eachAbsoluteFilePath, subFileNameArray); 
					LocalizationContext newLocalizationContext = getNewLocalizationContext(localizationContext, eachFile.getName()); 
					doFileUploading(newLocalizationContext, localizationAdapter, absoluteSubFilePathArray); 
				} else {
					try {
						localizationAdapter.save(eachFile, localizationContext, eachFile.getName()); 
					} catch(LocalizationOpFailedException lofe) {
						System.out.println("LocalizationOpFailedException is caught, error="+lofe.getMessage()); 
					}
				}
			}
		}
	}
	
	private File constructFile(String absoluteFilePath) {
		File file = null; 
		if(!StringUtil.isStringEmpty(absoluteFilePath))
			file = new File(absoluteFilePath); 
		return file; 
	}
	
	private LocalizationContext getNewLocalizationContext(LocalizationContext currentLocalizationContext, String newPostfixOfContextNameValue) {
		LocalizationContext newLocalizationContext = new LocalizationContext(currentLocalizationContext.getLocalizationType(), 
				currentLocalizationContext.getLocalizationLevel()); 
		String currentContextName = currentLocalizationContext.getContextName(); 
		String updatedContextName = currentContextName + File.separator + newPostfixOfContextNameValue; 
		newLocalizationContext.setContextName(updatedContextName); 
		return newLocalizationContext; 
	}
	
	private String [] constructAbsoluteFilePathArray(String parentAbsoluteFilePath, String [] subFileNameArray) {
		String [] absoluteFilePathArray = null; 
		if(subFileNameArray != null && subFileNameArray.length > 0) {
			absoluteFilePathArray = new String[subFileNameArray.length]; 
			for(int i=0; i<subFileNameArray.length; i++) {
				absoluteFilePathArray[i] = parentAbsoluteFilePath + File.separator + subFileNameArray[i]; 
			}
		}
		
		return absoluteFilePathArray; 
	}
	
}
