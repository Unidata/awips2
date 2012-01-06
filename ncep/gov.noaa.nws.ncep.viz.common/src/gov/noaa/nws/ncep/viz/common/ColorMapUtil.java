package gov.noaa.nws.ncep.viz.common;

import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;


/**
 * 
 * Facilitates loading of colormaps
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer     Description
 * ------------ ----------  -----------  --------------------------
 * 11/17/2009   187          Q.Zhou      Initial created.
 * 12/17/2009                G. Hull     placeholder for category
 * 01/02/2010   204			 M. Li	     check for Radar or Sat resource
 * 03/14/2010                B. Hebbard	 add path separator btw tblDir and rsc (2 places);
 *                                       fix circular build dependency on MosaicResource	
 * 03/21/2010   259          G. Hull     load by category                                      
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class ColorMapUtil {
	ColorMapUtil() {

	}

    /**
     * Load a colormap by name
     * 
     * @param name
     *            name of the colormap
     * @return the colormap representation
     * @throws VizException
     */
    public static IColorMap loadColorMap(String cat, String name) throws VizException {
    	String cmapCat = cat.substring(0,1)+cat.substring(1).toLowerCase();
    	
        try {        	
        	/*
        	 * comment out by M. Gao
        	 */
//			String cmapDir = LocalizationManager.getInstance().getFilename("lutsDir");
//        	File f = new File(cmapDir + File.separator + cmapCat + File.separator + name + ".cmap");
        	File f = LocalizationManager.getInstance().getLocalizationFileDirectly(LocalizationResourcePathConstants.LUTS_RESOURCES_DIR
        			+ File.separator + cmapCat,
        			name + ".cmap");
        	 
            if (f != null) {
            	ColorMap cm = (ColorMap) SerializationUtil   
                        .jaxbUnmarshalFromXmlFile(f.getAbsolutePath());

                cm.setName(name);
                return cm;
            } else {
                throw new VizException("Can't find colormap dude " + name);
            }
        } catch (SerializationException e) {
            throw new VizException("Unable to parse colormap " + name, e);
        }   
    }
    
    public static boolean colorMapExists( String cat, String name ) {
    	/*
    	 * comment out by M. Gao
    	 */
//		String cmapDir = LocalizationManager.getInstance().getFilename("lutsDir");
//		String fname = cmapDir + File.separator + cat + File.separator + name;
//		if( !name.endsWith(".cmap")) {
//			fname = fname + ".cmap";
//		}
//    	File f = new File( fname );
		String fname = name;
		if( !name.endsWith(".cmap")) {
			fname = fname + ".cmap";
		}
    	File f = LocalizationManager.getInstance().getLocalizationFileDirectly(LocalizationResourcePathConstants.LUTS_RESOURCES_DIR
    			+ File.separator + cat,
    			fname);
    	 
        return f.exists();
    }

    public static String[] getColorMapCategories() {
    	return new String[] {"Satellite", "Radar", "Other" };
    }
    
    /**
     * Lists all the colormaps available in the colormaps dir
     * 
     * 
     * @return an array of all the colormap names
     */
    public static String[] listColorMaps( String cat ) {
    	
    	String[] fileNames; 
//    	File[] files;
//    	ArrayList<String> allfiles = new ArrayList<String>();
    	String cmapCat = cat.substring(0,1)+cat.substring(1).toLowerCase();

    	/*
    	 * comment out by M. Gao. This portion can only retrieve files under
    	 * base level if the localization. If we want to be more dynamically, 
    	 * we can switch to use MultiFileLoading extension 
    	 */
//		String tblDir = LocalizationManager.getInstance().getFilename("lutsDir");
//		File tbl = new File(tblDir + File.separator + cmapCat + File.separator);
//    	File tbl = LocalizationManager.getInstance().getLocalizationFileDirectory(LocalizationResourcePathConstants.LUTS_RESOURCES_DIR
//    			+ File.separator + cmapCat,
//    			LocalizationConstants.LOCALIZATION_BASE_LEVEL);
    	File tbl = LocalizationManager.getInstance().reloadingResourceInfo(LocalizationResourcePathConstants.LUTS_RESOURCES_DIR, cmapCat); 
    	/*
    	 * End of M. Gao's change
    	 */
    	
		FilenameFilter filter = new FilenameFilter() {
	        public boolean accept(File dir, String name) {
	            return name.endsWith(".cmap") ;
	        }
		};
	    
		fileNames = tbl.list(filter);

		String[] cmaps = new String[fileNames.length];
		for (int i = 0; i < fileNames.length; i++) {
			cmaps[i] = fileNames[i].substring(0, fileNames[i].lastIndexOf(".") );
		}

        Arrays.sort(cmaps);

        return cmaps;
    }

    public static void saveColorMap( ColorMap colorMap, String cmapCat, String cmapName) 
    				throws VizException {

    	/*
    	 * comment out and rewriteen by M. Gao
    	 */
//		String cmapDir = LocalizationManager.getInstance().getFilename("lutsDir");
//		
//		File catDir = new File(cmapDir + File.separator + cmapCat + File.separator);
//
//		if( !catDir.exists() ) {
//			throw new VizException("ColorMap Category, "+cmapCat+" doesn't exist");
//		}
//		String cmapFilename = catDir.getAbsolutePath() + File.separator + cmapName;
//
//		if( !cmapFilename.endsWith(".cmap") ) {
//			cmapFilename += ".cmap";
//        }
		String cmapFilename = cmapName;
		if( !cmapFilename.endsWith(".cmap") ) {
			cmapFilename += ".cmap";
        }

		cmapFilename = LocalizationManager.getInstance().getLocalizationFileNameDirectly(LocalizationResourcePathConstants.LUTS_RESOURCES_DIR
				+ File.separator + cmapCat,
				cmapFilename);
		
		if( cmapFilename == null || cmapFilename.trim().length() == 0 ) {
			throw new VizException("ColorMap Category file, "+cmapFilename+" doesn't exist");
		}

        try {
            SerializationUtil.jaxbMarshalToXmlFile( colorMap, cmapFilename );
        } catch (SerializationException e) {
			throw new VizException("Unable to Marshal ColorMap "+ colorMap.getName() );          
        } 
    }
    
    public static void deleteColorMap( String cmapCat, String cmapName ) 
    			throws VizException {
    	/*
    	 * comment out and rewriteen by M. Gao
    	 */
//		String cmapDir = LocalizationManager.getInstance().getFilename("lutsDir");
//		
//		File catDir = new File(cmapDir + File.separator + cmapCat + File.separator);
//
//		if( !catDir.exists() ) {
//			throw new VizException("ColorMap Category, "+cmapCat+" doesn't exist");
//		}
//		String cmapFilename = catDir.getAbsolutePath() + File.separator + cmapName;
//
//		if( !cmapFilename.endsWith(".cmap") ) {
//			cmapFilename += ".cmap";
//        }
		String cmapFilename = cmapName;

		if( !cmapFilename.endsWith(".cmap") ) {
			cmapFilename += ".cmap";
        }
		File cmapFile = LocalizationManager.getInstance().getLocalizationFileDirectly(LocalizationResourcePathConstants.LUTS_RESOURCES_DIR
				+ File.separator + cmapCat,
				cmapFilename);
		if( !cmapFile.exists() ) {
			throw new VizException("ColorMap "+ cmapFile.getAbsolutePath() + 
					" doesn't exist." );
		}
		if( !cmapFile.delete() ) {
			throw new VizException("Error deleting ColorMap "+ cmapFile.getAbsolutePath() );
		}
    }
}