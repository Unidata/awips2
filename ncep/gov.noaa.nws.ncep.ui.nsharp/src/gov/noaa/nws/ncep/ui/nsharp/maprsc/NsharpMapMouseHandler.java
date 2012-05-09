/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapMouseHandler
 * 
 * This java class performs the NSHARP Modal functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.maprsc;


import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.SurfaceStationPointData;
import gov.noaa.nws.ncep.ui.nsharp.menu.ModelSoundingDialogContents;
import gov.noaa.nws.ncep.ui.nsharp.menu.NsharpLoadDialog;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDisplay;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;


//@SuppressWarnings("unchecked")
public class NsharpMapMouseHandler extends InputHandlerDefaultImpl {
	 
	public NsharpMapMouseHandler() {
		instance = this;
	}
	//private NsharpSkewTDisplay renderableDisplay=null;
	
	private static final double NctextuiPointMinDistance = 45000;
	//private int prevMouseX, prevMouseY;
	private static NsharpMapMouseHandler instance;
	
	private double lat, lon;
	
	/**
	 * Index of the selected point.
	 */
	private NsharpSkewTResource skewRsc=null;
	//private List<String> userSelectedTimeList = null;
	
	public double getLat() {
		return lat;
	}


	public double getLon() {
		return lon;
	}

	
	public void setSkewRsc(NsharpSkewTResource skewRsc) {
		this.skewRsc = skewRsc;
	}

 	
 	public static NsharpMapMouseHandler getAccess(){
 		return instance;
 	}
 	
 	//public void setSelectedTimeList(List<String> selectedTimeList) {
 	//	this.userSelectedTimeList = selectedTimeList;
 	//}
	/**
	 * For single point element, the original location is needed for undo.
	 */
	//Coordinate oldLoc = null;
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
     *      int, int)
     */
    @Override	   	
    public boolean handleMouseDown(int x, int y, int button) { 
    	//System.out.println("nsharp map mouse down");
    	//prevMouseX = x;
    	//prevMouseY = y;
        return false;    	
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     *      int, int)
     *  handle left button, so user be able to shift map while it is down
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int button) {
    	/*if (button == 1 ){
    		NCMapEditor mapEditor = NsharpMapResource.getMapEditor();
    		if(mapEditor != null){
    			IDisplayPane[] panes = ( mapEditor.arePanesGeoSynced() ?
    					mapEditor.getDisplayPanes() : mapEditor.getSelectedPanes() );

    			for( IDisplayPane p : panes ) {
    				p.shiftExtent(new double[] { x, y }, new double[] {
    						prevMouseX, prevMouseY });
    			}

    			mapEditor.refresh();

    			prevMouseX = x;
    			prevMouseY = y;
    		}

    	}*/
    	return false;
            
    }
    @Override
	public boolean handleMouseMove(int x, int y) {
		// TODO Auto-generated method stub
		return false;
	}
    
	/*
	private List<SoundingLayer>  readDataFromH5File(String fname, long validtime) {
		
		List<SoundingLayer> soundLyLst = new ArrayList<SoundingLayer>();
		
		   // retrieve an instance of H5File
        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

        if (fileFormat == null)
        {
        	System.out.print("Cannot find HDF5 FileFormat.");
            return null;
        }

        // open the file with read access
        FileFormat h5File;
		try {
			h5File = fileFormat.open(fname, FileFormat.READ);
			if (h5File == null)
			{
				System.out.print("Failed to open file: "+fname);
				return null;
			}


			// open the file and retrieve the file structure
			h5File.open();
			//Group root = (Group)((javax.swing.tree.DefaultMutableTreeNode)testFile.getRootNode()).getUserObject();
			//all data time line is stored in validTime table
			Dataset dataset = (Dataset)h5File.get("validTime");
			dataset.init();
			//long rank = dataset.getRank(); Note: rank is defined to get dimension, e,g rank=2 means a 2 dimension table
			long[] dim = dataset.getDims();

			//for (int i=0; i<rank; i++)
			//{
			//	System.out.println("dim"+i+"= "+ dim[i]);
			//}
			long [] dataRead = (long [])dataset.read();
			int selectedTimeIndex=0; 
			for (int j=0; j<dim[0]; j++)
			{
				//find the index of user picked data time line 
				if(dataRead[j] == validtime){
					selectedTimeIndex = j;
					System.out.println("selected valid time is " + validtime + " index="+ j);
					break;
				}
			}
			SoundingLayer soundingLy;
			// get temp, dew point, pressure, wind u/v components, specHum, omega
			Dataset omegadataset = (Dataset)h5File.get("omega");
			omegadataset.init();
			dim = omegadataset.getDims();       
			float[] omegaRead = (float [])omegadataset.read();
			Dataset pressuredataset = (Dataset)h5File.get("pressure");
			pressuredataset.init();
			float[] pressureRead = (float [])pressuredataset.read();
			Dataset temperaturedataset = (Dataset)h5File.get("temperature");
			temperaturedataset.init();
			float[] temperatureRead = (float [])temperaturedataset.read();
			Dataset specHumdataset = (Dataset)h5File.get("specHum");
			specHumdataset.init();
			float[] specHumRead = (float [])specHumdataset.read();
			Dataset vCompdataset = (Dataset)h5File.get("vComp");
			vCompdataset.init();
			float[] vCompRead = (float [])vCompdataset.read();
			Dataset uCompdataset = (Dataset)h5File.get("uComp");
			uCompdataset.init();
			float[] uCompRead = (float [])uCompdataset.read();
			if(dim[0] > selectedTimeIndex)
			{
				for (int i=0; i<dim[1]; i++)
				{
					soundingLy = new SoundingLayer(NsharpSkewTResource.UNVALID_DATA, NsharpSkewTResource.UNVALID_DATA, NsharpSkewTResource.UNVALID_DATA, 
							NsharpSkewTResource.UNVALID_DATA,NsharpSkewTResource.UNVALID_DATA, NsharpSkewTResource.UNVALID_DATA,NsharpSkewTResource.UNVALID_DATA);	
					soundingLy.setOmega(omegaRead[selectedTimeIndex*(int)dim[1]+i]);
					soundingLy.setTemperature((float)NsharpConstants.kelvinToCelsius.convert(temperatureRead[selectedTimeIndex*(int)dim[1]+i]));
					soundingLy.setPressure(pressureRead[selectedTimeIndex*(int)dim[1]+i]/100F);
					soundingLy.setWindU(uCompRead[selectedTimeIndex*(int)dim[1]+i]);
					soundingLy.setWindV(vCompRead[selectedTimeIndex*(int)dim[1]+i]);
					soundingLy.setDewpoint(specHumRead[selectedTimeIndex*(int)dim[1]+i]);
					System.out.print(specHumRead[selectedTimeIndex*(int)dim[1]+i] + ", ");
					soundLyLst.add(soundingLy);
				}

			}

			// close file resource
			h5File.close();
		} catch (Exception e) {
			
			e.printStackTrace();
		}
		
		return soundLyLst;
       
	}
	*/
/*	private void getPfcSndDataMy(List<NsharpStationInfo> stnPtDataLineLst, Map<String, List<SoundingLayer>> soundingLysLstMap) {
using hdf5 obj lib
		String pickedStnInfo = "";
		SoundingSite sndSite = new SoundingSite();
		sndSite.setPluginName("modelsounding");
		System.out.println("snd site plug in name = "+sndSite.getPluginName());
		
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		cal.setTimeInMillis(stnPtDataLineLst.get(0).getReftime().getTime());
		
		DataTime dataTime = new DataTime(cal);
		sndSite.setDataTime(dataTime);
		sndSite.setDataURI(stnPtDataLineLst.get(0).getDatauri());
		File hdf5loc = NsharpHD5PathProvider.findHDF5Location(sndSite);
		
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
		
		
		
		//IDataStore dataStore = DataStoreFactory.getDataStore(hdf5loc);

		//System.out.println("snd site hdf5 path = " + hdf5loc.getAbsolutePath());
		System.out.println(" validtime= "+StnPt.getRangestarttime().getTime()/1000);
		
		List<SoundingLayer>  sndLyList = readDataFromH5File(hdf5loc.getAbsolutePath(), StnPt.getRangestarttime().getTime()/1000);
		if(sndLyList != null &&  sndLyList.size() > 0){  
			if(pickedStnInfo == ""){
				//use as first stn to show
				pickedStnInfo = StnPt.getStnDisplayInfo();
			}
			//update sounding data so they can be used by Skewt Resource and PalletWindow
			//soundLyLst = dataHandling.updateObsSoundingDataForShow(soundLyLst, (float)StnPt.getElevation());
			//if(soundLyLst.size() >0)//Chin..check this later...
				soundingLysLstMap.put(StnPt.getStnDisplayInfo(), sndLyList);
			//System.out.println(StnPt.getStnDisplayInfo() + " with sound layer size of "+ soundLyLst.size());
		}
		} // end for loop of  stnPtsLst 
*/		
/**/		
		
		
		
		/* Another way to read H5 file.....
            String hdf5File = hdf5loc.getAbsolutePath();
            //String group = sndSite.getDataURI();
            String dataset = "omega";//TBD
            IDataRecord dr;
            
            
            int file_id = -1;
    		int dataset_id = -1;
    		float[][] dset_data = new float[182][64];
    		// Open file using the default properties.
    		try {
    			file_id = H5.H5Fopen(hdf5File, HDF5Constants.H5F_ACC_RDONLY,
    					HDF5Constants.H5P_DEFAULT);
    		}
    		catch (Exception e) {
    			e.printStackTrace();
    		}

    		// Open dataset using the default properties.
    		try {
    			if (file_id >= 0)
    				dataset_id = H5.H5Dopen(file_id, dataset);
    		}
    		catch (Exception e) {
    			e.printStackTrace();
    		}

    		// Read the data using the default properties.
    		try {
    			if (dataset_id >= 0)
    				
    				H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_FLOAT,
    						HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
    						HDF5Constants.H5P_DEFAULT, dset_data);
    		}
    		catch (Exception e) {
    			e.printStackTrace();
    		}

    		// Output the data to the screen.
    		System.out.println(dataset + ":");
    		for (int indx = 0; indx < 182; indx++) {
    			System.out.print(" [ ");
    			for (int jndx = 0; jndx < 64; jndx++)
    				System.out.print(dset_data[indx][jndx] + " ");
    			System.out.println("]");
    		}
    		System.out.println();

            
    		// Close the dataset.
    		try {
    			if (dataset_id >= 0)
    				H5.H5Dclose(dataset_id);
    		}
    		catch (Exception e) {
    			e.printStackTrace();
    		}
*/
    		//get all dataset names
    		/*
    		String[] retVal;
    		int group_id = -1;
            try {
                group_id = H5.H5Gopen(file_id, "/");

                long[] numObjs = new long[1];
                H5.H5Gget_num_objs(group_id, numObjs);

                retVal = new String[(int) numObjs[0]];

                for (int i = 0; i < retVal.length; i++) {
                    String[] tmp = new String[1];
                    H5.H5Gget_objname_by_idx(group_id, i, tmp, 255);
                    retVal[i] = tmp[0];
                    System.out.println(retVal[i]);
                }

               
            } catch (HDF5LibraryException e) {
                
            } finally {

                if (group_id >= 0) {
                    try {
                        H5.H5Gclose(group_id);
                    } catch (HDF5LibraryException e) {
                        // Ignore
                    }
                }

            }
            
    		// Close the file.
    		try {
    			if (file_id >= 0)
    				H5.H5Fclose(file_id);
    		}
    		catch (Exception e) {
    			e.printStackTrace();
    		}
             */
    		
    		
    		
            /*
            
          
            try {
                dr = dataStore.retrieve("", dataset, Request.ALL);
                System.out.println("name = " +dr.getName());
                System.out.println("SizeInBytes = " +dr.getSizeInBytes());
                System.out.println("getDimension = " +dr.getDimension());
                System.out.println("getFillValue = " +dr.getFillValue());
                System.out.println("group = " +dr.getGroup());
                System.out.println("getMaxSizes = " +dr.getMaxSizes());
                System.out.println("getMinIndex = " +dr.getMinIndex());
                
                
                ///System.out.println(dr.getDataObject());
            } catch (FileNotFoundException e) {
                
                e.printStackTrace();
                return;
            } catch (StorageException e) {
                
                e.printStackTrace();
                return;
            }
		
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
			List<SoundingLayer> soundLyLst = new ArrayList<SoundingLayer>();
			//query database
			String queryStr;
			soundLyLst.clear();
			
			if(soundLyLst.size() > 0){  
				if(pickedStnInfo == ""){
					//use as first stn to show
					pickedStnInfo = StnPt.getStnDisplayInfo();
				}
				//update sounding data so they can be used by Skewt Resource and PalletWindow
				//do we have to do this for pfc sounding?
				//soundLyLst = dataHandling.updateObsSoundingDataForShow(soundLyLst, (float)StnPt.getElevation());
				if(soundLyLst.size() >0)//Chin..check this later...
					soundingLysLstMap.put(StnPt.getStnDisplayInfo(), soundLyLst);
				//System.out.println(StnPt.getStnDisplayInfo() + " with sound layer size of "+ soundLyLst.size());
			}
		} // end for loop of  stnPtsLst 
	}
*/	
	/*
	private List<SoundingLayer>  convertToSoundingLayerList(List<NcSoundingLayer>  sndLst){
		List<SoundingLayer>   newLst = new ArrayList<SoundingLayer>();
		for(NcSoundingLayer inLayer: sndLst){
			SoundingLayer outLayer = new SoundingLayer();
			outLayer.setDewpoint(inLayer.getDewpoint());
			outLayer.setTemperature(inLayer.getTemperature());
			outLayer.setPressure(inLayer.getPressure());
			outLayer.setGeoHeight(inLayer.getGeoHeight());
			outLayer.setWindDirection(inLayer.getWindDirection());
			outLayer.setWindSpeed(inLayer.getWindSpeed());
			outLayer.setOmega(inLayer.getOmega());
			newLst.add(outLayer);
		}
		return newLst;
	}
	*/
	/*
	private void getPfcSndData(List<NsharpStationInfo> stnPtDataLineLst, Map<String, List<SoundingLayer>> soundingLysLstMap) {
		String pickedStnInfo = "";
		SoundingSite sndSite = new SoundingSite();
		sndSite.setPluginName("modelsounding");
		//System.out.println("snd site plug in name = "+sndSite.getPluginName());
		
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		cal.setTimeInMillis(stnPtDataLineLst.get(0).getReftime().getTime());
		
		DataTime refTime = new DataTime(cal);
		sndSite.setDataTime(refTime);
		sndSite.setDataURI(stnPtDataLineLst.get(0).getDatauri());
		//File hdf5loc = NsharpHD5PathProvider.findHDF5Location(sndSite);
		
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
			NcSoundingProfile sndPf= PfcSoundingQuery.getPfcSndData(StnPt.getDatauri(),(float)StnPt.getLatitude(), (float)StnPt.getLongitude(), StnPt.getReftime(), 
					StnPt.getRangestarttime(), PfcSoundingQuery.PfcSndType.NAMSND);
		
			List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
			List<SoundingLayer>  sndLyList = convertToSoundingLayerList(rtnSndLst);
			if(sndLyList != null &&  sndLyList.size() > 0){  
				if(pickedStnInfo == ""){
					//use as first stn to show
					pickedStnInfo = StnPt.getStnDisplayInfo();
				}
				//update sounding data so they can be used by Skewt Resource and PalletWindow
				//soundLyLst = dataHandling.updateObsSoundingDataForShow(soundLyLst, (float)StnPt.getElevation());
				//if(soundLyLst.size() >0)//Chin..check this later...
				soundingLysLstMap.put(StnPt.getStnDisplayInfo(), sndLyList);
				//System.out.println(StnPt.getStnDisplayInfo() + " with sound layer size of "+ soundLyLst.size());
			}
		//System.out.println(" validtime= "+StnPt.getRangestarttime().getTime()/1000);
		}
	}
	*/
	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
     *      int)
     *      handle right button, so user be able to pick stn and print text report
     */
    @Override
    public boolean handleMouseUp(int x, int y, int button) {
    	//System.out.println("NsharpMapMouseHandler handleMouseUp called");
       	// button 1 is left mouse button
    	if (button == 1 ){
    		NCMapEditor mapEditor = NsharpMapResource.getMapEditor();
    		if(mapEditor != null){
    			//for(int i=0; i< mapEditor.getDescriptor().getResourceList().size(); i++)
    	        //	System.out.println( "C resourcename="+mapEditor.getDescriptor().getResourceList().get(i).getResource().getName());

    			//  Check if mouse is in geographic extent
    			Coordinate loc = mapEditor.translateClick(x, y);
    			if ( loc == null ) 
    				return false;
    			NsharpLoadDialog loadDia = NsharpLoadDialog.getAccess();		
    			if(loadDia!=null){
    				if(loadDia.getActiveLoadSoundingType() == NsharpLoadDialog.MODEL_SND && loadDia.getMdlDialog()!=null&& loadDia.getMdlDialog().getLocationText()!=null){

    					if(loadDia.getMdlDialog().getCurrentLocType() == ModelSoundingDialogContents.LocationType.STATION){
    						//System.out.println("mouse up 1 loc.x "+ loc.x+ " loc.y="+ loc.y);
    						String stnName = SurfaceStationPointData.calculateNearestPoint(loc);
    						//System.out.println("mouse up 2 loc.x "+ loc.x+ " loc.y="+ loc.y);
    						//System.out.println("stn name = "+ stnName);
    						if(stnName == null)
    							stnName = "";
    						loadDia.getMdlDialog().getLocationText().setText(stnName);
    					}
    					else {
    						String latLonStr = String.format("%6.2f;%7.2f",loc.y,loc.x);
    						//System.out.println("mouse up 2 loc.x "+ loc.x+ " loc.y="+ loc.y + " latlonStr=" +latLonStr);
    						loadDia.getMdlDialog().getLocationText().setText(latLonStr);
    					}
    					
    				}
    				else {
    					//get the stn (point) list
    					int activeLoadType = loadDia.getActiveLoadSoundingType();
    					List<NsharpStationInfo> points = NsharpMapResource.getOrCreateNsharpMapResource().getPoints();//loadDia.getNsharpMapResource().getPoints();
    					if(points.isEmpty() == false){
    						
    						//get the stn close to loc "enough" and retrieve  report for it
    						// Note::One stn may have more than one dataLine, if user picked multiple data time lines
    						List<NsharpStationInfo> stnPtDataLineLst = getPtWithinMinDist(points, loc);
     						if(stnPtDataLineLst!= null && stnPtDataLineLst.size() > 0){
    							//System.out.println("MapMouseHandler creating NsharpSkewTDisplay");
    							//hash map, use stn display info as key
    							Map<String, List<NcSoundingLayer>> soundingLysLstMap = new HashMap<String, List<NcSoundingLayer>>();
    							
    							//String soundingType;
    							if(activeLoadType == NsharpLoadDialog.OBSER_SND){ 	
    								NsharpMapResource.startWaitCursor();
    								NsharpObservedSoundingQuery.getObservedSndData(stnPtDataLineLst,loadDia.getObsDialog().isRawData(),soundingLysLstMap);   								
    								NsharpMapResource.stopWaitCursor();
    							} 
    							else if (activeLoadType == NsharpLoadDialog.PFC_SND){
    								NsharpMapResource.startWaitCursor();
    								NsharpPfcSoundingQuery.getPfcSndDataBySndTmRange(stnPtDataLineLst,soundingLysLstMap);
    								NsharpMapResource.stopWaitCursor();
    							} 
    							else 
    								return false;
    							//System.out.println("MAP size/" +soundingLysLstMap.size());
    							if(soundingLysLstMap.size() <=0){
    								//win.setAndOpenMb("Invalid sounding data returned from DB for this station!");
    								Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    								MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING
    										| SWT.OK);
    								mb.setMessage("Invalid sounding data returned from DB for this station!!");
    								mb.open();
    								loadDia.closeDiaOnly();    		
    								return false;
    							}
    							loadDia.closeDiaOnly();    		
    							// create an editor NsharpSkewTEditor
    							NsharpSkewTEditor skewtEdt = NsharpSkewTEditor.createOrOpenSkewTEditor();
    							NsharpSkewTDisplay renderableDisplay = (NsharpSkewTDisplay) skewtEdt
    							.getActiveDisplayPane()
    							.getRenderableDisplay();

    							renderableDisplay.setEditorNum(skewtEdt
    									.getEditorNum());
    							skewRsc = renderableDisplay.getDescriptor().getSkewtResource();
    							skewRsc.addRsc(soundingLysLstMap, stnPtDataLineLst.get(0));
    							mapEditor = NsharpMapResource.getMapEditor();
    							if (mapEditor != null) {
    								mapEditor.refresh();
    							}
    							bringSkewTEdToTop();
    						}
    						else
    						{	
    							//System.out.println("Mouse point too far from stn");
    						}
    					}
    					else
    					{	//debug
    						//System.out.println("points is null");
    					}
    				}
    			}
    		}
    		
    		
    	}
    	else if(button == 3){
    		//NsharpSkewTEditor.bringSkewTEditorToTop(); 
    		bringSkewTEdToTop();
    	}
    	
        return false;        
    }
    /*
     * Chin Note: If calling NsharpSkewTEditor.bringSkewTEditorToTop() directly in mouse handler API, e.g.
     * handleMouseUp(), then handleMouseUp() will be called one more time by System. Do not know the root cause of it.
     * To avoid handling such event twice (e.g. query sounding data twice), we will call NsharpSkewTEditor.bringSkewTEditorToTop()
     * from another Job (thread).
     */
    private void bringSkewTEdToTop(){
    	Job uijob = new UIJob("clear source selection"){ //$NON-NLS-1$
			public IStatus runInUIThread(
					IProgressMonitor monitor) {
				NsharpSkewTEditor.bringSkewTEditorToTop(); 
				return Status.OK_STATUS;
			}

		};
		uijob.setSystem(true);
		uijob.schedule();
    }
    
     /**
     * Gets the nearest point of an selected element to the input point
     * @param el 	element
     * @param pt 	input point
     * @return
     */
    private List<NsharpStationInfo> getPtWithinMinDist( List<NsharpStationInfo> points , Coordinate pt ){

    	NsharpStationInfo thePoint = null;
    	double	minDistance = NctextuiPointMinDistance; 	
    	GeodeticCalculator gc;
    	List<NsharpStationInfo> thePoints = new ArrayList<NsharpStationInfo>();
    	NCMapEditor mapEditor = NsharpMapResource.getMapEditor();
    	if(mapEditor != null){
    		IMapDescriptor desc = (IMapDescriptor) mapEditor.getActiveDisplayPane().getRenderableDisplay().getDescriptor();
    		gc = new GeodeticCalculator(desc.getCRS());
    		gc.setStartingGeographicPoint(pt.x, pt.y);
    		//int textDispIndex = 1;//debug
    		for ( NsharpStationInfo point : points){

    			gc.setDestinationGeographicPoint( point.getLongitude(),point.getLatitude());
    			double dist;
    			try{
    				dist = gc.getOrthodromicDistance();
    				//System.out.println("dist to point " + textDispIndex++ + " is " +  dist);
    				if (  dist < minDistance ) {

    					minDistance = dist;
    					thePoint = point; 
    				}
    			}
    			catch (Exception e) {

    				//e.printStackTrace();
    				//System.out.println("getOrthodromicDistance exception happened!");
    			}


    		}			
    		// Chin, there may be more than one point for a selected stn. As user may selected more than one data time,
    		// For same stn, each data time will have one point to represent it. So, a stn may have more than one points
    		if(thePoint != null){
    			for ( NsharpStationInfo point : points){
    				if ((thePoint.getLatitude() == point.getLatitude())&& (thePoint.getLongitude() == point.getLongitude())){
    					thePoints.add(point);
    				}
    			}
    			
    			//marked X on selected point
    			NsharpMapResource.getOrCreateNsharpMapResource().setPickedPoint(thePoint);
    			
    		}

    	}
    	return thePoints;

    }

}

