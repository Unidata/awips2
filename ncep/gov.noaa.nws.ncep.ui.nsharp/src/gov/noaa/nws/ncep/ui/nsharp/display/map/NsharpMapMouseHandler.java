/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapMouseHandler
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
 * 03/11/2013   972         Greg Hull   NatlCntrsEditor
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display.map;


import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.SurfaceStationPointData;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.view.ModelSoundingDialogContents;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpLoadDialog;
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.viz.ui.display.NatlCntrsEditor;

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
	
	
	public double getLat() {
		return lat;
	}


	public double getLon() {
		return lon;
	}
 	
 	public static NsharpMapMouseHandler getAccess(){
 		return instance;
 	}
 	
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
    	return false;
            
    }
    @Override
	public boolean handleMouseMove(int x, int y) {
		// TODO Auto-generated method stub
		return false;
	}
	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
     *      int)
     *      handle right button, so user be able to pick stn and print text report
     */
    @Override
    public boolean handleMouseUp(int x, int y, int button) {
    	//if ( !NsharpMapResource.getOrCreateNsharpMapResource().isEditable()) return false;
    	//System.out.println("NsharpMapMouseHandler handleMouseUp called");
       	// button 1 is left mouse button
    	if (button == 1 ){
    		NatlCntrsEditor mapEditor = NsharpMapResource.getMapEditor();
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
    						// create an editor NsharpEditor
							//NsharpEditor skewtEdt = NsharpEditor.createOrOpenEditor();
							
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
    							
    							//NsharpResourceHandler skewRsc = skewtEdt.getRscHandler();
    							//skewRsc.addRsc(soundingLysLstMap, stnPtDataLineLst.get(0));
    							loadDataToNsharpResources(soundingLysLstMap, stnPtDataLineLst.get(0));
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
    		//NsharpEditor.bringSkewTEditorToTop(); 
    		bringSkewTEdToTop();
    	}
    	
        return false;        
    }
    /*
     * Chin Note: If calling NsharpEditor.bringSkewTEditorToTop() directly in mouse handler API, e.g.
     * handleMouseUp(), then handleMouseUp() will be called one more time by System. Do not know the root cause of it.
     * To avoid handling such event twice (e.g. query sounding data twice), we will call NsharpEditor.bringSkewTEditorToTop()
     * from another Job (thread). 
     */
    private void bringSkewTEdToTop(){
    	Job uijob = new UIJob("clear source selection"){ //$NON-NLS-1$
			public IStatus runInUIThread(
					IProgressMonitor monitor) {
				NsharpEditor.bringEditorToTop(); 
				return Status.OK_STATUS;
			}

		};
		uijob.setSystem(true);
		uijob.schedule();
    }
   /*
    * Same reason to use UIJob as bringSkewTEdToTop()
    */
    private void loadDataToNsharpResources(final Map<String, List<NcSoundingLayer>> soundMap, final NsharpStationInfo stnInfo){
    	Job uijob = new UIJob("clear source selection"){ //$NON-NLS-1$
			public IStatus runInUIThread(
					IProgressMonitor monitor) {
				NsharpResourceHandler rscHdr = NsharpEditor.createOrOpenEditor().getRscHandler();
				rscHdr.addRsc(soundMap, stnInfo);
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
    	// TODO : can't assume this is a map Editor/MapDescriptor
    	NatlCntrsEditor mapEditor = NsharpMapResource.getMapEditor();
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

