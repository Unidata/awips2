package gov.noaa.nws.ncep.viz.localization.adapter;

import java.io.File;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
//import com.raytheon.uf.common.localization.exception.LocalizationCommunicationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.core.localization.BundleScanner;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

public class NcepCAVELocalizationAdapter extends CAVELocalizationAdapter {

	public NcepCAVELocalizationAdapter() {
		super(); 
	}
	
	/*
	 * Override Raytheon's CAVELocalizationAdapter and add NOAA logic 
	 * @see com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter#getPath(com.raytheon.uf.common.localization.LocalizationContext, java.lang.String)
	 */
    @Override
    public File getPath(LocalizationContext context, String fileName) {

        String baseDir = null;
        String typeString = getDirNameForType(context.getLocalizationType());
        LocalizationLevel level = context.getLocalizationLevel();
        if (level == LocalizationLevel.BASE) {
            if (context.getLocalizationType() == LocalizationType.COMMON_STATIC) {
                // Common files are downloaded for all levels, including base
                baseDir = LocalizationManager.getUserDir() + File.separator
                        + typeString + File.separator + "base";
            } else {
                if (context.getLocalizationType() == LocalizationType.CAVE_STATIC) {
                    // Check to see if it is resident in a bundle first
                    // else go to the cave static dir
                    if (context.getContextName() != null) {
                        return BundleScanner.searchInBundle(
                                context.getContextName(), fileName);
                    } 
                    /*
                     * NOAA NCEP's change starts here
                     */
                    else {
                        baseDir = LocalizationManager.getBaseDir() + File.separator
                    		+ typeString;
                    }
                    /*
                     * NOAA NCEP's change ends here
                     */
                }
                /*
                 * NOAA NCEP's change starts here
                 */
//                else if (context.getLocalizationType() == LocalizationType.CAVE_NCEP) {
//                    baseDir = LocalizationManager.getUserDir() + File.separator
//                    	+ typeString + File.separator + "base"; 
//                }
                /*
                 * NOAA NCEP's change ends here
                 */
                else {
                    baseDir = LocalizationManager.getBaseDir() + File.separator
                    	+ typeString;
                }
            }
        } else if (level == LocalizationLevel.SITE) {
            baseDir = LocalizationManager.getUserDir() + File.separator
                    + typeString + File.separator + "site" + File.separator
                    + context.getContextName();
        } else if (level == LocalizationLevel.USER) {
            baseDir = LocalizationManager.getUserDir() + File.separator
                    + typeString + File.separator + "user" + File.separator
                    + context.getContextName();
            /*
             * NOAA NCEP's change starts here
             */
       // } else if (level == LocalizationLevel.DESK) {
       //     baseDir = LocalizationManager.getUserDir() 
       //     	+ typeString + File.separator + "site" + File.separator
       //     	+ context.getContextName();
            /*
             * NOAA NCEP's change ends here
             */    
       } else {
            throw new IllegalArgumentException(
                    "Unsupported localization level: "
                            + context.getLocalizationLevel());
        }

        return new File(baseDir + File.separator + fileName);
    }

//    /*
//     * Not sure why Raytheon considers cave_static.base and cave_config.base belong
//     * to baseline locally, comment out by Michael Gao.      
//     * @see com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter#retrieve(com.raytheon.uf.common.localization.LocalizationContext, java.lang.String)
//     */
//    @Override
//    public void retrieve(LocalizationContext context, String fileName)
//            throws LocalizationOpFailedException {
//
//        // cave_static.base and cave_config.base is baselined locally, not on
//        // the server
//
//    	/*
//    	 * comment out by NOAA NCEP
//    	 */
////        if ((context.getLocalizationLevel() == LocalizationLevel.BASE)
////                && ((context.getLocalizationType() == LocalizationType.CAVE_STATIC) || (context
////                        .getLocalizationType() == LocalizationType.CAVE_CONFIG))) {
////            return;
////        }
//
//        LocalizationManager.getInstance().retrieve(context, fileName);
//    }

    /*
     * Override Raytheon's code and add CAVE_NCEP
     * @see com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter#getDirNameForType(com.raytheon.uf.common.localization.LocalizationContext.LocalizationType)
     */
    @Override
    public String getDirNameForType(LocalizationType type) {
        if (type == LocalizationType.COMMON_STATIC) {
            return "common";
        } else if (type == LocalizationType.CAVE_STATIC) {
            return "etc";
          /*
           * Check CAVE_NCEP localizationType. NOAA NCEP changes start here. 
           */
//        } else if (type == LocalizationType.CAVE_NCEP) {
//            return "ncep";
          /*
           * Check CAVE_NCEP localizationType. NOAA NCEP changes end here. 
           */
        } else if (type == LocalizationType.CAVE_CONFIG) {
            return "configuration";
        } else {
            throw new IllegalArgumentException("Unsupported type: " + type);
        }
    }

    /*
     * override Raytheon's method and add DESK concept 
     * @see com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter#getContext(com.raytheon.uf.common.localization.LocalizationContext.LocalizationType, com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel)
     */
    @Override
    public LocalizationContext getContext(LocalizationType type,
            LocalizationLevel level) {
        LocalizationContext ctx = new LocalizationContext(type, level);
        if (level == LocalizationLevel.BASE) {
            // nothing to add
        } else if (level == LocalizationLevel.SITE) {
            // fill in current site name
            ctx.setContextName(LocalizationManager.getInstance()
                    .getCurrentSite());
        } else if (level == LocalizationLevel.USER) {
            // fill in current user name
            ctx.setContextName(LocalizationManager.getInstance()
                    .getCurrentUser());
            /*
             * add one more LocalizationLevel option DESK. NOAA NCEP changes start here. 
             */
        //} else if (level == LocalizationLevel.DESK) {
            // fill in current desk name
       // 	ctx.setContextName(LocalizationManager.getInstance()
       // 			.getCurrentDesk());
            /*
             * add one more LocalizationLevel option DESK. NOAA NCEP changes end here. 
             */
        } else {
            throw new IllegalArgumentException("LocalizationLevel."
                    + level.name() + " is not supported by "
                    + this.getClass().getName());
        }
        return ctx;
    }

}
