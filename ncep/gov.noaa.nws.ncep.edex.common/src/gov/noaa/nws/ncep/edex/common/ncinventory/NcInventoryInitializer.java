package gov.noaa.nws.ncep.edex.common.ncinventory;

import java.io.File;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Called once on edex-request startup to initialize all NcInventory's from
 * the NcInventoryDefinitions in the Localization.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  05/24/12      #606       Greg Hull   Created
 *  11/15/12      #950       Greg Hull   don't look in subdirectories of NcInventoryDefinitions.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcInventoryInitializer {

 	public static final String NCINVENTORY_DEFN_PATH = "ncep/NcInventoryDefinitions"; 
 	
    public void initialize() throws Exception {
        System.out.println("initializing ncinventory");
        
        IPathManager pathMngr = PathManagerFactory.getPathManager();
        
        LocalizationContext lCntxts[] = new LocalizationContext[3]; 
        lCntxts[0] = pathMngr.getContext( 
        		LocalizationType.COMMON_STATIC, LocalizationLevel.BASE );
//      lCntxts[1] = pathMngr.getContext( 
//        		LocalizationType.COMMON_STATIC, LocalizationLevel.DESK );
        lCntxts[1] = pathMngr.getContext( 
        		LocalizationType.COMMON_STATIC, LocalizationLevel.SITE );
        lCntxts[2] = pathMngr.getContext( 
        		LocalizationType.COMMON_STATIC, LocalizationLevel.USER );
        
        LocalizationFile[] invDefnsFiles =
        	pathMngr.listFiles( lCntxts, NCINVENTORY_DEFN_PATH, 
        			new String[]{".xml"}, false, true );
        
        for( LocalizationFile lFile : invDefnsFiles ) {
        	System.out.println("invDefn  file is :"+ lFile.getName() );
        	File invDefnFile = lFile.getFile();
    		
    		Object xmlObj;
    		try {
    			xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
    								invDefnFile.getAbsolutePath() );
    			if( !(xmlObj instanceof NcInventoryDefinition) ) {
    				throw new Exception("NcInventoryDefinition .xml file is not an NcInventoryDefinition object???");
    			}
    			NcInventoryDefinition invDefn = (NcInventoryDefinition)xmlObj;
    			
    			NcInventory.initInventory( invDefn, false );        			    			
    		}
    		catch( Exception ex ) {
    			NcInventory.logError( ex.getMessage() );
    		}
        }
    }

}
