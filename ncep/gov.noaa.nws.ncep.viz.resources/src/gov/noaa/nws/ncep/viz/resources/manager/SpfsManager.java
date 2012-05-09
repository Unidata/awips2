package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.io.File;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationAdapter;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;

/**
 * Common class for constants, utility methods ...
 *  * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/24/10		  #226		Greg Hull    Break out from NmapCommon
 * 03/04/10       #226      Greg Hull    special case for PGEN
 * 06/22/10       #273      Greg Hull    move some methods to ResourceDefnsMngr
 * 07/28/11       #450      Greg Hull    split out Predefined Areas methods and
 *                                       renamed to SpfsManager. Refactored to 
 *                                       read from NcPathManager
 * 11/14/11       #???      B. Hebbard   Optionally have saveRbdToSpf(...) switch
 *                                       cycle times to LATEST before marshaling,
 *                                       and restore actual times afterwards.
 * 11/15/11                  ghull       add resolveLatestCycleTimes
 * 01/01/12                  J. Zeng     Add listener for multiple CAVEs to get SPFs info from each other 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class SpfsManager implements ILocalizationFileObserver  {	

	private static SpfsManager instance = null;

	// TODO : do we want to store the RbdBundle or just the LocalizationFile
	// (Store the RbdBundle but don't give it out unless we are making a copy
	// of it (by unmarshalling the file in LFIle.)
	private Map<String, Map<String, Map<String,RbdBundle>>> spfsMap = null;
	
	public static SpfsManager getInstance() {
		if( instance == null ) {
			instance = new SpfsManager();			
		}
		return instance;
	}
	
	private SpfsManager() {
		findAvailSpfs();
	}
	
	private void findAvailSpfs() {
		spfsMap = new TreeMap<String,
							Map<String, Map<String,RbdBundle>>>();
		
		// This will find all the directories for SPFs and SPF groups as well
		// as the RBDs
		Map<String, LocalizationFile> spfsLFileMap = NcPathManager.getInstance().listFiles( 
				NcPathConstants.SPFS_DIR, null, true, false );
		
		// loop thru the LocalizationFiles and save to 
		for( LocalizationFile lFile : spfsLFileMap.values() ) {
			
			lFile.addFileUpdatedObserver(this);
			
			String[] dirs = lFile.getName().split( File.separator );
			
			// if this is a SPF Group or SPF directory
			if( lFile.getFile().isDirectory() ) {
				// if an SPF Group
				if( dirs.length < 3 ) {
					// the NcPathConstants.SPFS_DIR 
				}
				else if( dirs.length == 3 ) {
					// this will be redundant if there are actually SPFs in the SPF group since
					// the SPF directory will create the group too.
					addSpfGroup( dirs[2] );// , lFile ); // don't know if we need the lFile here or not
				    //spfGroupsMap.put( dirs[2], lFile );					
				}
				else if( dirs.length == 4 ) {
					//addSpfGroup( dirs[2] ); // don't know if we need the lFile here or not
					addSpf( dirs[2], dirs[3] );//, lFile );// store the lFile?
					//spfsMap.put( dirs[3], lFile );
				}
				else {
					System.out.println("Found dir under SPFs with more than 3 paths???:"+lFile.getName() );
				}
			}
			else { // if this is an RBD, check for .xml and in an SPF directory
				if( !lFile.getName().endsWith(".xml" ) ) {
					System.out.println("Non-xmlfile found under SPFs dir???:"+lFile.getName() );
				}
				else if( dirs.length != 5 ) {
					System.out.println("xml file found in non-SPF directory? "+lFile.getName() );
				}
				else {
					try {
						RbdBundle rbd = RbdBundle.unmarshalRBD( lFile.getFile(), null );

						rbd.setLocalizationFile( lFile );
						addRbd( dirs[2], dirs[3], rbd );
					}
					catch (VizException e ) {
						// log error
						System.out.println("Error unmarshalling rbd: "+lFile.getName()
								+ "\n"+e.getMessage() );
					}
				}
			}
		}
		
		System.out.println("#spfs ="+Integer.toString( spfsMap.size() )); 
		
	}
	
	// ? it may be possible to have groups in 2 different contexts. I don't think 
	// we will need to store the context for each group though.??
	//
	public void addSpfGroup( String grpName ) { //, LocalizationFile lFile ) {
		if( !spfsMap.containsKey( grpName ) ) {
			spfsMap.put( grpName, new TreeMap<String, Map<String,RbdBundle>>() );
		}
	}

	public void addSpf( String grpName, String spfName ) {//, LocalizationFile lFile ) {
		addSpfGroup( grpName );
		
		Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( grpName );

		if( !grpMap.containsKey( spfName ) ) {
			grpMap.put( spfName, new TreeMap<String, RbdBundle>() );
		}
		
	}
	
	public void addRbd( String grpName, String spfName, RbdBundle rbd ) {
		
		// can we allow an RBD at this point without a LocalizationFile?
		//
		if( rbd.getLocalizationFile() == null ) {
			
		}
		
		addSpfGroup( grpName );
		addSpf( grpName, spfName );
		
		Map<String,Map<String,RbdBundle>> gMap = spfsMap.get( grpName );
		Map<String,RbdBundle>             sMap = gMap.get( spfName );
		
		if( !sMap.containsKey( rbd.getRbdName() ) ) {
			sMap.put( rbd.getRbdName(), rbd );
		}
	}
	
	// return an array of all the sub directories in the spf groups dir.
    public String[] getAvailSPFGroups() {
    	String[] avail_groups = spfsMap.keySet().toArray( new String[0] );
   		Arrays.sort( avail_groups );
   		return avail_groups;
   	}
    
    // return an array of all the spf (.xml) files in the given spf group dir.
    //
	public String[] getSpfNamesForGroup( String grpName ) {
		
		Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( grpName );

		if( grpMap == null ) {
			return new String[]{};
		}
		
		String[] spfNames = grpMap.keySet().toArray( new String[0] );
			 
		Arrays.sort( spfNames );

		return spfNames;
	}

    //
	public String[] getRbdNamesForSPF( String grpName, String spfName ) {
		Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( grpName );

		if( grpMap == null ) {
			return new String[]{};
		}
		Map<String,RbdBundle> sMap = grpMap.get( spfName );
		
		if( sMap == null ) {
			return new String[]{};
		}
		
		String[] rbdNames = sMap.keySet().toArray( new String[0] );

		Arrays.sort( rbdNames );
		
		return rbdNames;
	}
	
	// return a copy of the Rbds in the given SPF.
	public ArrayList<RbdBundle> getRbdsFromSpf( String grpName, String spfName,
			boolean resolveLatestCycleTimes ) throws VizException {
		
		if( grpName == null || spfName == null ) {
			throw new VizException("spf group or spf name is null");
		}
		
		Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( grpName );
		if( grpMap == null ) {
			throw new VizException("SPF Group "+ grpName+" doesn't exist.");
		}
		Map<String,RbdBundle> sMap = grpMap.get( spfName );
		
		if( sMap == null ) {
			throw new VizException("SPF "+ spfName+" doesn't exist.");
		}
		
		ArrayList<RbdBundle> rbdsList = new ArrayList<RbdBundle>();
		
		for( RbdBundle rbd : sMap.values() ) {
			RbdBundle newRbd = RbdBundle.unmarshalRBD( 
									rbd.getLocalizationFile().getFile(), null );
			if( resolveLatestCycleTimes ) {
				newRbd.resolveLatestCycleTimes();
				
				// if unable to resolve the cycle time then leave as Latest and 
				// resources will have to gracefully handle NoData.
			}

			rbdsList.add( newRbd ); 
		}
		
		return rbdsList;
	}
	
	// TODO : decide what is/isn't a valid rbd name ...
	//
	public boolean isValidRbdName( String rbdName ) {
		if( rbdName != null && !rbdName.isEmpty() ) {
			if( !rbdName.contains( File.separator ) ) { 
				// more invalid checks....
				
				return true;
			}			
		}
		return false;
	}
	// 
	public boolean doesRbdExistInUserContext( 
			String spfGroup, String spfName, String rbdName ) {
		
		if( spfsMap.containsKey( spfGroup ) ) {
			Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( spfGroup );
			
			if( grpMap.containsKey( spfName ) ) {
				Map<String,RbdBundle> sMap = grpMap.get( spfName );
				
				if( sMap.containsKey( rbdName ) ) {
					RbdBundle rbd = sMap.get( rbdName );
					return (rbd.getLocalizationFile().getContext().getLocalizationLevel() ==
						          LocalizationLevel.USER );
				}
			}
		}
		
		return false;
	}
	
//	private Map<String,RbdBundle> getRbdList( String grpName, String spfName, RbdBundle rbd ) {
//		addSpfGroup( grpName );
//		
//		addSpf( grpName, spfName );
//				
//	}
	
	// 
	public void saveRbdToSpf( String grpName, String spfName, RbdBundle rbd ) throws VizException {
		saveRbdToSpf( grpName, spfName, rbd, false );  // default save cycle time as LATEST
	}
	
	// 
	public void saveRbdToSpf( String grpName, String spfName, RbdBundle rbd, boolean saveTimeAsConstant ) throws VizException {
	
		// The localization code will handle creating the group and spf directories if needed
		//
		String rbdLclName = NcPathConstants.SPFS_DIR + File.separator +
							grpName + File.separator + spfName + File.separator + 
							rbd.getRbdName()+".xml";
		LocalizationContext usrCntxt = NcPathManager.getInstance().getContext(
				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );
		
		LocalizationFile lFile = NcPathManager.getInstance().getLocalizationFile(
				usrCntxt,  rbdLclName );
		
		if( lFile == null ||  lFile.getFile() == null ) {
			throw new VizException("Error creating localization file for rbd: "+rbdLclName 	);
		}
		File rbdFile = lFile.getFile();

		// If user requested saving cycle times as LATEST (as opposed to Constant), then for each
		// requestable forecast resource in the RBD, make it so -- before marshaling out to XML.
		// But first, save the 'real' cycle time in a map, for restoration later (below).
		Map<String,DataTime> resourceNameToCycleTimeMap = new HashMap<String,DataTime>();
		if (!saveTimeAsConstant) {
			for (AbstractRenderableDisplay display : rbd.getDisplays()) {
				for (ResourcePair rp : display.getDescriptor().getResourceList()) {
					AbstractResourceData ard = rp.getResourceData();
					if (ard instanceof AbstractNatlCntrsRequestableResourceData) {
						AbstractNatlCntrsRequestableResourceData ancrrd = (AbstractNatlCntrsRequestableResourceData) ard;
						ResourceName rn = ancrrd.getResourceName();
						if (rn.isForecastResource()) {
							resourceNameToCycleTimeMap.put(rn.toString(),rn.getCycleTime());
							rn.setCycleTimeLatest();
						}
					}
				}
			}
		}
		// marshal out the rbd to the file on disk, set the localizationFile for the rbd,
		// save the localization file and update the spfsMap with the rbd and possible new
		// group and spf
		//
		try {
			SerializationUtil.jaxbMarshalToXmlFile( rbd, rbdFile.getAbsolutePath() );

			rbd.setLocalizationFile( lFile );
			
			lFile.save();
			
			addRbd( grpName, spfName, rbd );
			
			lFile.addFileUpdatedObserver(this);
			
		} catch (LocalizationOpFailedException e) {
			throw new VizException(e);
		} catch (SerializationException e) {
			throw new VizException(e);
		} finally {
			// If we saved cycle times as LATEST (as opposed to Constant), then restore the 'real'
			// cycle times in each requestable forecast resource in the RBD.  (See above.)
			if (!saveTimeAsConstant) {
				for (AbstractRenderableDisplay display : rbd.getDisplays()) {
					for (ResourcePair rp : display.getDescriptor().getResourceList()) {
						AbstractResourceData ard = rp.getResourceData();
						if (ard instanceof AbstractNatlCntrsRequestableResourceData) {
							AbstractNatlCntrsRequestableResourceData ancrrd = (AbstractNatlCntrsRequestableResourceData) ard;
							ResourceName rn = ancrrd.getResourceName();
							if (rn.isForecastResource() && resourceNameToCycleTimeMap.containsKey(rn.toString())) {
								rn.setCycleTime(resourceNameToCycleTimeMap.get(rn.getCycleTime()));
							}
						}
					}
				}
			}
			
		}
	}

	@Override
	public void fileUpdated(FileUpdatedMessage message) {

		
		String chgFile = message.getFileName();
		FileChangeType chgType = message.getChangeType();
		LocalizationContext chgContext = message.getContext();
		
		if (chgType.toString().equals("ADDED")) {

			String[] dirsf = chgFile.split( File.separator);
			
			if( ! chgFile.endsWith(".xml" ) ) {
				System.out.println("Non-xmlfile found under SPFs dir???:"+ chgFile );
			}
			else if( dirsf.length != 5 ) {
				System.out.println("xml file found in non-SPF directory? "+ chgFile );
			}else {
				try {
					LocalizationFile lFile = NcPathManager.getInstance().getLocalizationFile( 
							chgContext, chgFile );
					RbdBundle rbd = RbdBundle.unmarshalRBD( lFile.getFile(), null );
					//System.out.println("Add Rbd name is " + rbd.rbdName );
					rbd.setLocalizationFile( lFile );
					addRbd( dirsf[2], dirsf[3], rbd );
				}
				catch (VizException e ) {
					// log error
					System.out.println("Error unmarshalling rbd: "+ chgFile
							+ "\n"+e.getMessage() );
				}
			
			}
		}

	}
	

}