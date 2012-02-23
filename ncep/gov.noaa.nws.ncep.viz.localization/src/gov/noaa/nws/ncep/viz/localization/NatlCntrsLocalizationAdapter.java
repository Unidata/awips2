package gov.noaa.nws.ncep.viz.localization;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.core.localization.BundleScanner;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Adapter for the NcPathManager 
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/22/11       #450      Greg Hull    Created
 * 
 * </pre>
 * 
 * @author ghull 
 * @version 1
 */
public class NatlCntrsLocalizationAdapter extends CAVELocalizationAdapter {
	
	// TODO : CAVELocalizationAdapter's contexts map is private so we have our own.
    private final Map<LocalizationType, LocalizationContext[]> contextMap;

	private static LocalizationLevel DESK;

    private static ArrayList<LocalizationLevel> ncLevels;

    
	public NatlCntrsLocalizationAdapter() {
		super(); 
		
		contextMap = new HashMap<LocalizationType, LocalizationContext[]>();
		
		ncLevels = new ArrayList<LocalizationLevel>();
		ncLevels.add( LocalizationLevel.BASE );
		ncLevels.add( LocalizationLevel.SITE );
		DESK = LocalizationLevel.createLevel("DESK", 650 ); // SITE < DESK < USER
		ncLevels.add( DESK );
		ncLevels.add( LocalizationLevel.USER );
		
        LocalizationManager.getInstance();
        
        if( !Activator.getCurrentDesk().isEmpty() ) {
        	LocalizationManager.registerContextName(
        			DESK, Activator.getCurrentDesk() );
        }

	}
	
	/*
	 * Override Raytheon's CAVELocalizationAdapter and add NOAA logic 
	 * @see com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter#getPath(com.raytheon.uf.common.localization.LocalizationContext, java.lang.String)
	 */
    @Override
    public File getPath(LocalizationContext context, String fileName) {

        String baseDir = null;
        String typeString = getDirNameForType( context.getLocalizationType());
        
        LocalizationLevel level = context.getLocalizationLevel();
        
        if (level == LocalizationLevel.BASE) {
            if (context.getLocalizationType() == LocalizationType.COMMON_STATIC) {
                // Common files are downloaded for all levels, including base
                baseDir = LocalizationManager.getUserDir() + File.separator
                        + typeString + File.separator + "base";
            } 
            else {
                if (context.getLocalizationType() == LocalizationType.CAVE_STATIC) {
                    // Check to see if it is resident in a bundle first
                    // else go to the cave static dir
                    if (context.getContextName() != null) {
                        return BundleScanner.searchInBundle(
                                context.getContextName(), fileName);
                    } 
                    else {
                        baseDir = LocalizationManager.getBaseDir() + File.separator
                    		+ typeString;
                    }               
                }
                else {
                    baseDir = LocalizationManager.getBaseDir() + File.separator
                    	+ typeString;
                }
            }
        } else if (level == LocalizationLevel.SITE) {
            baseDir = LocalizationManager.getUserDir() + File.separator
                    + typeString + File.separator + "site" + File.separator
                    + context.getContextName();
        } else if (level == DESK ) {
            baseDir = LocalizationManager.getUserDir() + File.separator
                    + typeString + File.separator + "desk" + File.separator
                    + context.getContextName();
        } else if (level == LocalizationLevel.USER) {
            baseDir = LocalizationManager.getUserDir() + File.separator
                    + typeString + File.separator + "user" + File.separator
                    + context.getContextName();
       } else {
            throw new IllegalArgumentException(
                    "Unsupported localization level: "
                            + context.getLocalizationLevel());
        }

        return new File(baseDir + File.separator + fileName);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.localization.ILocalizationAdapter#
     * getLocalSearchHierarchy
     * (com.raytheon.uf.common.localization.LocalizationContext
     * .LocalizationType)
     */
    @Override
    public LocalizationContext[] getLocalSearchHierarchy( LocalizationType type ) {
        synchronized ( contextMap ) {
            LocalizationContext[] ctx = contextMap.get(type);
            ctx =  null;
            if (ctx == null) {
//                LocalizationLevel[] levels = LocalizationLevel.values();
//                Arrays.sort(levels, LocalizationLevel.REVERSE_COMPARATOR);

            	// TODO : add DESK
            	LocalizationLevel[] levels = { LocalizationLevel.USER, 
            			DESK,
            			LocalizationLevel.SITE, LocalizationLevel.BASE };
            	
                ctx = new LocalizationContext[levels.length];
                
                for (int i = 0; i < levels.length - 1; ++i) {
                    ctx[i] = getContext( type, levels[i] );
                }

       // TODO : we could restrict this to only bundles that extent our 
       // extention point?
       //
                if( type == LocalizationType.CAVE_STATIC ) {
                	
                	
                    // only search bundles for cave_static
                    Set<String> bndls = BundleScanner.getListOfBundles();

                    ctx = Arrays.copyOf(ctx, ctx.length + bndls.size());

                    int i = levels.length - 1;

                    for (String b : bndls) {
                        ctx[i] = new LocalizationContext(type, LocalizationLevel.BASE);
                        ctx[i].setContextName(b);
                        i++;
                    }
                }

                ctx[ctx.length - 1] = new LocalizationContext(type,
                        LocalizationLevel.BASE);
                contextMap.put(type, ctx);
            }

            return ctx;
        }
    }

    /*
     * Override Raytheon's code and append "ncep".
     * @see com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter#getDirNameForType(com.raytheon.uf.common.localization.LocalizationContext.LocalizationType)
     */
    @Override
    public String getDirNameForType(LocalizationType type) {

    	// Note : we could alternatively prepend "ncep" to the paths of all files that NCEP localizes.
    	// (do this in the NcPathManager and make all ncep localization access go thru NcPathManager
    	return super.getDirNameForType(type);//+File.separator+"ncep";
//        if (type == LocalizationType.COMMON_STATIC) {
//            return "common"+File.separator+"ncep";
//        } else if (type == LocalizationType.CAVE_STATIC) {
//            return "etc"+File.separator+"ncep";
//        } else if (type == LocalizationType.CAVE_CONFIG) {
//            return "configuration"+File.separator+"ncep";
//        } else {
//            throw new IllegalArgumentException("Unsupported type: " + type);
//        }
    }


    /*
     * override Raytheon's method and add DESK concept 
     * @see com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter#getContext(com.raytheon.uf.common.localization.LocalizationContext.LocalizationType, com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel)
     */
    @Override
    public LocalizationContext getContext(LocalizationType type,
            LocalizationLevel level) {
        String contextName = null;
           // contextName = LocalizationManager.getContextName(level);

        if (level == LocalizationLevel.BASE) {
            // nothing to add
        } else if (level == LocalizationLevel.SITE) {
            // fill in current site name
            contextName = LocalizationManager.getInstance().getCurrentSite();
        } else if (level == LocalizationLevel.USER) {
            // fill in current user name
        	contextName = LocalizationManager.getInstance().getCurrentUser();
        } else if (level == DESK) {
            // fill in current desk name
        	contextName = Activator.getCurrentDesk();
        } else {
            throw new IllegalArgumentException("LocalizationLevel."
                    + level.name() + " is not supported by "
                    + this.getClass().getName());
        }

        LocalizationContext ctx = new LocalizationContext(type, level, contextName);

        return ctx;
    }
    
}
