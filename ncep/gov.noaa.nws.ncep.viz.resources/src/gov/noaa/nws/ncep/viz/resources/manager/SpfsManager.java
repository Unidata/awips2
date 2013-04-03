package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
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
import com.raytheon.uf.common.localization.PathManager;
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
 * 04/29/12       #606      Greg Hull    now called from the PerspectiveManager. Better timing/count output. 
 * 06/13/12       #817      Greg Hull    add resolveDominantResource() 
 * 06/18/12       #713      Greg Hull    in addRbd() overwrite existing rbds if they exist.
 * 06/29/12       #568      Greg Hull    add deleteRbd()
 * 07/22/12       #568      Greg Hull    return Rbds and rbdNames sorted by seq num.
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class SpfsManager implements ILocalizationFileObserver  {	

	private static SpfsManager instance = null;

	private static long rbdCount = 0; // Might not want to rely on these counts for anything critical.
	private static long spfCount = 0;
	private static long spfGrpCount = 0;
	
	// TODO : do we want to store the RbdBundle or just the LocalizationFile
	// (Store the RbdBundle but don't give it out unless we are making a copy
	// of it (by unmarshalling the file in LFile.)
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
		long t0 = System.currentTimeMillis();
		
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
			// NOTE: Wait to add the Group/SPF so that only directories 
			// that have RBDs in them get added to the map.
			if( lFile.getFile().isDirectory() ) {
				// if an SPF Group
				if( dirs.length < 3 ) {
					// the NcPathConstants.SPFS_DIR 
				}
				else if( dirs.length == 3 ) {
					// this will be redundant if there are actually SPFs in the SPF group since
					// the SPF directory will create the group too.
//					addSpfGroup( dirs[2] );// , lFile ); // don't know if we need the lFile here or not
				}
				else if( dirs.length == 4 ) {
					//addSpfGroup( dirs[2] ); // don't know if we need the lFile here or not
//					addSpf( dirs[2], dirs[3] );//, lFile );// store the lFile?
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
						RbdBundle rbd = RbdBundle.getRbd( lFile.getFile() );

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
		
		System.out.println("Time to Read "+ rbdCount + " RBDs: "+ 
				(System.currentTimeMillis()-t0) + " msecs.");

	}
	
	// ? it may be possible to have groups in 2 different contexts. I don't think 
	// we will need to store the context for each group though.??
	//
	public void addSpfGroup( String grpName ) { //, LocalizationFile lFile ) {
		synchronized ( spfsMap ) {			
		if( !spfsMap.containsKey( grpName ) ) {
			spfsMap.put( grpName, new TreeMap<String, Map<String,RbdBundle>>() );
			spfGrpCount++;
		}
	}
	}

	public void addSpf( String grpName, String spfName ) { 
		
		synchronized ( spfsMap ) {			
		addSpfGroup( grpName );
		
		Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( grpName );

		if( !grpMap.containsKey( spfName ) ) {
			grpMap.put( spfName, new TreeMap<String, RbdBundle>() );
			spfCount++;
		}
		}		
	}
	
	public void addRbd( String grpName, String spfName, RbdBundle rbd ) {
		// can we allow an RBD at this point without a LocalizationFile?
		//
		if( rbd.getLocalizationFile() == null ) {
			
		}
		
		synchronized ( spfsMap ) {			
		addSpfGroup( grpName );
		addSpf( grpName, spfName );
		
		Map<String,Map<String,RbdBundle>> gMap = spfsMap.get( grpName );
		Map<String,RbdBundle>             sMap = gMap.get( spfName );
		
		if( !sMap.containsKey( rbd.getRbdName() ) ) {
			rbdCount++;
		}
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
		
//		String[] rbdNames = sMap.keySet().toArray( new String[0] );
//		Arrays.sort( rbdNames );
		// Sort according to the sequence number in the RBD.
		RbdBundle[] rbds = sMap.values().toArray( new RbdBundle[0] );
		Arrays.sort( rbds );
		
		String rbdNames[] = new String[ rbds.length ];
		for( int i=0 ; i<rbds.length ; i++ ) {
			rbdNames[i] = rbds[i].getRbdName();
		}
		return rbdNames;
	}
	
	// return a copy of the Rbds in the given SPF.
	public List<RbdBundle> getRbdsFromSpf( String grpName, String spfName,
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
		
		RbdBundle rbdsList[] = new RbdBundle[ sMap.values().size() ];
		int r=0;
		
		for( RbdBundle rbd : sMap.values() ) {
			
			rbdsList[r] = RbdBundle.clone( rbd ); 						
			
			rbdsList[r].resolveDominantResource();
			
			if( resolveLatestCycleTimes ) {
				rbdsList[r].resolveLatestCycleTimes();
				
				// if unable to resolve the cycle time then leave as Latest and 
				// resources will have to gracefully handle NoData.
			}

			r++;
		}
		
		Arrays.sort( rbdsList );

		// make a copy to allow the user to modify the list.
		return new ArrayList<RbdBundle>( Arrays.asList( rbdsList ) );
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
//	
//	private Map<String,RbdBundle> getRbdList( String grpName, String spfName, RbdBundle rbd ) {
//		addSpfGroup( grpName );
//		
//		addSpf( grpName, spfName );
//				
//	}
		
	// create a new SPF with the given rbds. The rbdsList should be in order and the
	// SPF should not exist yet.
	//
	public void createSpf( String grpName, String spfName, List<RbdBundle> rbdsList,
			Boolean saveRefTime, Boolean saveCycleTime ) throws VizException {
		// make sure the spf doesn't exist.
		if( rbdsList.isEmpty() || grpName == null || grpName.isEmpty() ||
			spfName == null || spfName.isEmpty() ) {
			throw new VizException("Error creating SPF. Null spf name or no rbds are selected." );
		}
			
		Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( grpName );

		if( grpMap != null ) {
				Map<String,RbdBundle> sMap = grpMap.get( spfName );
				
			if( sMap != null ) {
				throw new VizException("The SPF "+grpName+File.separator+spfName+" already exists." );
				}
			}

		for( RbdBundle rbd : rbdsList ) {
			saveRbdToSpf( grpName, spfName, rbd, saveRefTime, saveCycleTime );
		}
		
		// The updating of the SpfManager is done in the Notification thread to give it time to update
		// before refetching the rbds again.
    	try { Thread.sleep(300); } catch (InterruptedException e) { }
	}
	
	// the SPF should already exist. This will delete any existing Rbds that aren't in the given list.
//		
	public void saveSpf( String grpName, String spfName, List<RbdBundle> rbdsList, 
							Boolean saveRefTime, Boolean saveCycleTime ) throws VizException {
		if( rbdsList.isEmpty() || 
			grpName == null || grpName.isEmpty() ||
			spfName == null || spfName.isEmpty() ) {
				throw new VizException("Error saving SPF. Null spf name or no rbds are selected." );
		}
		
		// get the current Rbds so we can delete those that have been removed.
//				
		List<RbdBundle> existingRbds = getRbdsFromSpf( grpName, spfName, false );
	
		boolean deleteRbdFlag = true;
		
		for( RbdBundle existingRbd : existingRbds ) {
			deleteRbdFlag = true;
			
			for( RbdBundle rbd : rbdsList ) { 
				if( existingRbd.getRbdName().equals( rbd.getRbdName() ) ) {
					deleteRbdFlag = false;
					break;
				}
			}
			if( deleteRbdFlag ) {
				deleteRbd( existingRbd );
			}
		}

		// TODO : it would be nice if we could determine if the spf has
		// changed so that we don't have to override BASE/SITE level rbds that haven't changed.
	// 
		for( RbdBundle rbd : rbdsList ) {
			saveRbdToSpf( grpName, spfName, rbd, saveRefTime, saveCycleTime );
		}

		// The updating of the SpfManager is done in the Notification thread to give it time to update
		// before refetching the rbds again.
    	try { Thread.sleep(300); } catch (InterruptedException e) { }
	}
	
	// 
	public void saveRbdToSpf( String grpName, String spfName, RbdBundle rbd,
			      boolean saveRefTime, boolean saveCycleTime ) throws VizException {
	
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

		// if the user elects not to save out the refTime then don't marshal it out. 
  		//
		DataTime savedRefTime = rbd.getTimeMatcher().getRefTime();
		
		if( !saveRefTime ) {
			rbd.getTimeMatcher().setCurrentRefTime();     			
		}

		// If user requested saving cycle times as LATEST (as opposed to Constant), then for each
		// requestable forecast resource in the RBD, make it so -- before marshaling out to XML.
		// But first, save the 'real' cycle time in a map, for restoration later (below).
		//
		// TODO : do we still have to do this now that we can clone the RBDs?
		//
		Map<String,DataTime> resourceNameToCycleTimeMap = new HashMap<String,DataTime>();
		if( !saveCycleTime ) {
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
			if( !saveRefTime ) {
				rbd.getTimeMatcher().setRefTime( savedRefTime );     			
			}

			// If we saved cycle times as LATEST (as opposed to Constant), then restore the 'real'
			// cycle times in each requestable forecast resource in the RBD.  (See above.)
			if (!saveCycleTime) {
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
	
	public void deleteSpfGroup( String delGroup ) throws VizException {
		LocalizationFile groupLocDir = NcPathManager.getInstance().getStaticLocalizationFile(
				NcPathConstants.SPFS_DIR + File.separator + delGroup );
		
		if( groupLocDir == null ) {
			throw new VizException( "Could not find Localization File for:\n"+
					NcPathConstants.SPFS_DIR + File.separator + delGroup);
		}
		else if( groupLocDir.getContext().getLocalizationLevel() != LocalizationContext.LocalizationLevel.USER ) {
			throw new VizException( "Can not delete a non-user defined SPF." );
		}
		else if( getSpfNamesForGroup( delGroup ).length > 0 ) {
			throw new VizException( "Can't delete non-empty SPF:\n"+delGroup);
		}
		else if( !groupLocDir.isDirectory() ) { // sanity check
			throw new VizException( "Localization File for SPF is not a directory:\n"+ delGroup );
		}

		// Note that this will trigger the fileUpdated which will remove the group from the map
		try {
			if( !groupLocDir.delete() ) {
				throw new VizException("Error deleting file:"+groupLocDir.getFile().getAbsolutePath() );
			}
		} catch (LocalizationOpFailedException e) {
			throw new VizException(e);
		}
	}

	// use this to check to see if the given User-level SPF has a superceding SPF. 
	//
	public LocalizationContext getSpfContext( String spfGroup, String spfName ) throws VizException {
		LocalizationFile spfLocDir = NcPathManager.getInstance().getStaticLocalizationFile(
				NcPathConstants.SPFS_DIR + File.separator + spfGroup + File.separator + spfName );
		return ( spfLocDir == null ? null : spfLocDir.getContext() );
	}
	
	// Ideally we could just check that the SPF dir is in User-Level Context. The SPF Manager GUI 
	// should enforce this but it may be possible for the Localization perspective to be used to 
	// create USER level SPFs with SITE or DESK level RBDs.
	//    This method will return false if the SPF dir or any of its RBDs have a non USER level file.
	//
	public Boolean isUserLevelSpf( String spfGroup, String spfName ) {
		try {
			LocalizationContext spfCntx = getSpfContext( spfGroup, spfName );
			
			for( RbdBundle rbd : getRbdsFromSpf( spfGroup, spfName, false ) ) {
				// TODO : should we look for the File if the LocalizationFile is not set? 
				// Just assum that the RBD hasn't been created yet....			
				if( rbd.getLocalizationFile() != null ) {

					if( rbd.getLocalizationFile().getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
						return false;
					}
				}
			}
		} catch (VizException e) {
			System.out.println("error getting Spf Localization Dir.???");
			// assume it hasn't been created yet			
		}
		
		return true;
	}

	
	// This assumes that all the RBDs and the SPF all are in the USER's Localization.
	
	// This will delete all the user-level RBDs in the SPF as well as the SPF.
	//    If there are SITE, or Base level files in the SPF then we will 'revert' back to
	//
	public void deleteSpf( String spfGroup, String delSpfName ) throws VizException {
	
		LocalizationFile spfLocDir = NcPathManager.getInstance().getStaticLocalizationFile(
				NcPathConstants.SPFS_DIR + File.separator + spfGroup + File.separator + delSpfName );
		
		if( spfLocDir == null ) {
			throw new VizException( "Could not find Localization File for:\n"+
					NcPathConstants.SPFS_DIR + File.separator + spfGroup + File.separator + delSpfName );
		}
		else if( !isUserLevelSpf(spfGroup, delSpfName ) ) {
			throw new VizException("Either the SPF Localization Dir or one of the RBD " +
									"Localization Files is not in the User-Level Context.");
		}
		else if( !spfLocDir.isDirectory() ) { // sanity check
			throw new VizException( "Localization File for SPF is not a directory:\n"+
					spfGroup + File.separator + delSpfName );
		}

		// get a list of the RBDs in this spf and delete them
		List<RbdBundle> existingRbds = getRbdsFromSpf( spfGroup, delSpfName, false );

		for( RbdBundle delRbd : existingRbds ) {
// check to see if this RBD supercedes a SITE or DESK level RBD 		
//			LocalizationFile rbdLocFiles[] = NcPathManager.getInstance().getTieredLocalizationFile( 
//					NcPathConstants.SPFS_DIR + File.separator + spfGroup + File.separator + delSpfName+File.separator+rbd.getR );

			deleteRbd( delRbd );
		}


		// Note that this will trigger the fileUpdated which will remove the spf from the map
		try {
			spfLocDir.delete();
			
			// The updating of the SpfManager is done in the Notification thread to give it time to update
			// before refetching the rbds again.
	    	try { Thread.sleep(300); } catch (InterruptedException e) { }

		} catch (LocalizationOpFailedException e) {
			throw new VizException(e);
		}		
	}

	public void deleteRbd( RbdBundle rbd ) throws VizException {
		LocalizationFile lFile = rbd.getLocalizationFile();
		
		if( lFile == null ) {			
			throw new VizException("Rbd, "+rbd.getRbdName()+" has no Localization File to delete.");			
		}
		else if( lFile.getContext().getLocalizationLevel() != 
			 			LocalizationLevel.USER ) {
			throw new VizException("Can not delete a non-USER level RBD: "+rbd.getRbdName() );
		}
		
		// this will trigger the fileUpdated method which will
		// remove the Rbd from the map
		try {
			lFile.delete();
		} catch (LocalizationOpFailedException e) {
			throw new VizException(e);
		}
		
		rbd.setLocalizationFile( null );		
	}

	public void removeEntryByFile( LocalizationFile lFile ) {
		
// 
		Map<LocalizationLevel, LocalizationFile> superFiles = 
			NcPathManager.getInstance().getTieredLocalizationFile( lFile.getName() );
		superFiles.remove( LocalizationLevel.USER );
		
		if( !superFiles.isEmpty() ) {
			System.out.println("Removing FIle "+lFile.getName()+
					" that has a lower level File. Need to Revert." );
		}
		
		
		String spfPaths[] = lFile.getName().split(File.separator);
		int i = NcPathConstants.SPFS_DIR.split(File.pathSeparator).length;
		int pathCount = spfPaths.length-i-1;
		
		String spfGroup = spfPaths[i+1];
		
		Map<String,Map<String,RbdBundle>> grpMap = spfsMap.get( spfGroup );

		if( grpMap == null ) {
			System.out.println("Could not find Group "+spfGroup+" for RBD "+
					lFile.getName() );
			return;
		}

		// if this is an Spf Group then remove it and return
		if( pathCount == 1 ) {
			// sanity check that the group is empty
			// 
			if( spfsMap.containsKey( spfGroup ) ) {
				if( !spfsMap.get( spfGroup ).isEmpty() ) {
					System.out.println("???deleting non-empty SPF Group: "+ spfGroup );
				}
			}

			spfsMap.remove( spfGroup );
			
			return;
		}
		
		String spfName  = spfPaths[i+2];

		Map<String,RbdBundle> sMap = grpMap.get( spfName );

		if( sMap == null ) {
			System.out.println("Could not find SPF "+spfName+" for RBD "+
					lFile.getName() );
			return;
		}
		
		// if this is an Spf then remove it and return
		if( pathCount == 2 ) {
			if( grpMap.containsKey( spfName ) ) {
				if( !grpMap.get( spfName ).isEmpty() ) {
					System.out.println("???deleting non-empty SPF : "+ spfName );
				}
			}

			grpMap.remove( spfName );
			
			if( grpMap.isEmpty() ) {
				spfsMap.remove( spfGroup );
			}
			return;
		}

		long saveRbdCount = rbdCount;

		String rbdFileName = spfPaths[i+3];

		for( String rbdName : sMap.keySet() ) {
			LocalizationFile lf = sMap.get( rbdName ).getLocalizationFile(); 
			if( lf != null &&
				lf.getName().equals( lFile.getName() ) ) {
				
				sMap.remove( rbdName );

				rbdCount--;
				break;
			}
		}
		
		if( saveRbdCount == rbdCount ) {
			System.out.println("Could not find rbd to remove for File:"+lFile.getName() );
		}
	}

	@Override
	public void fileUpdated(FileUpdatedMessage message) {		
		String chgFile = message.getFileName();
		FileChangeType chgType = message.getChangeType();
		LocalizationContext chgContext = message.getContext();
		
		// TODO : need to handle the UPDATED cases
		//
		LocalizationFile lFile = NcPathManager.getInstance().getLocalizationFile( 
				chgContext, chgFile );
			String[] dirsf = chgFile.split( File.separator);
			
		try {
			if( chgType == FileChangeType.ADDED ) {
				// 
			if( !chgFile.endsWith(".xml" ) ) {
				System.out.println("Non-xmlfile found under SPFs dir???:"+ chgFile );
			}
			else if( dirsf.length != 5 ) {
				System.out.println("xml file found in non-SPF directory? "+ chgFile );
			} 
			else {
					RbdBundle rbd = RbdBundle.getRbd( lFile.getFile() );
					//System.out.println("Add Rbd name is " + rbd.rbdName );
					rbd.setLocalizationFile( lFile );
					addRbd( dirsf[2], dirsf[3], rbd );
				}
			}
			else if( chgType == FileChangeType.DELETED ) {

				removeEntryByFile( lFile );
			}
			// TODO
			else if( chgType == FileChangeType.UPDATED ) {
				System.out.println("SpfsManager recieved FileUpdated msg but not handleing");
			}
		}
				catch (VizException e ) {
					// log error
					System.out.println("Error unmarshalling rbd: "+ chgFile
							+ "\n"+e.getMessage() );
				}			
			}
		}
