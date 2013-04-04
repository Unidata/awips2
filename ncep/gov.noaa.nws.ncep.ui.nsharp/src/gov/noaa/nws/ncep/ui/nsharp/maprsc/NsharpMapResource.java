/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.rsc.NsharpMapResource
 * 
 * This java class performs the NSHARP Resource functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 08/17/2012	655			B. Hebbard	Added paintProps as parameter to IDisplayable draw (2)
 * 
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.maprsc;


import java.awt.Color;
import java.io.File;
import java.util.List;
import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.EditorUtil;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;


public class NsharpMapResource  extends AbstractVizResource<NsharpMapResourceData,MapDescriptor> 
	implements RemoveListener{
	private static NsharpMapResource mapRsc=null;
	private static NCMapEditor mapEditor=null;
	private static  NsharpMapMouseHandler mouseHandler;
	private static Cursor waitCursor=null;
	private static Control cursorControl;
	private static boolean mouseHandlerRegistered=false;
	public static void bringMapEditorToTop(){
		try{
			if(mapEditor!=null&& PlatformUI.getWorkbench()!=null && PlatformUI.getWorkbench().getActiveWorkbenchWindow()!=null
					&& PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()!=null){
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().bringToTop(mapEditor);
				mapEditor.refresh();
			}
		}
		catch (Exception e) {
		}
		
	}
	public static NCMapEditor getMapEditor() {
		return mapEditor;
	}

	public static NsharpMapResource getMapRsc() {
		return mapRsc;
	}
	private NsharpMapResourceData nsharpMapResourceData;
	/** The set of symbols with similar attributes across many locations */
	private SymbolLocationSet symbolSet = null;
	private SymbolLocationSet symbolToMark = null;
	private List<NsharpStationInfo> points = new ArrayList<NsharpStationInfo>();
	private List<NsharpStationInfo> pickedPoint = new ArrayList<NsharpStationInfo>();
	public void setPickedPoint(NsharpStationInfo point) {
		this.pickedPoint.add(point);
	}
	public List<NsharpStationInfo> getPoints() {
		return points;
	}

	public void setPoints(List<NsharpStationInfo> points) {
		if (points == null){
			this.pickedPoint.clear();
			symbolToMark= null;
			symbolSet=null;
			this.points.clear();
		}
		else{
			this.points = points;
		}
	}

	public void addPoint(NsharpStationInfo point) {
		points.add(point);
	}

	/**
	 * Default constructor
	 */
	protected NsharpMapResource(NsharpMapResourceData resourceData,
			LoadProperties loadProperties) {
		super(resourceData, loadProperties);
		this.nsharpMapResourceData = resourceData;
		//System.out.println("NsharpMapResource constructed");
		
	}
	public static void startWaitCursor(){
		waitCursor = new Cursor( Display.getCurrent(), SWT.CURSOR_WAIT);
		cursorControl = Display.getCurrent().getCursorControl();
		if(cursorControl!=null && waitCursor!=null)
			cursorControl.setCursor(waitCursor);
   	}
   	public static void stopWaitCursor(){
   		if(cursorControl!=null&& waitCursor!=null){
   			cursorControl.setCursor(null);
   		}
   		if(waitCursor!=null){
   			waitCursor.dispose();
   			waitCursor= null;
   		}
   	}
   	/*
	private static void createMapEditor(){
		// create an editor MapEditor
		File rbdFile = NcPathManager.getInstance().getStaticFile( 
		         NcPathConstants.DFLT_RBD );
		try {
			
			IEditorPart ep = EditorUtil.getActiveEditor();
	        if (ep instanceof NCMapEditor) {
	        	mapEditor= (NCMapEditor) ep;
	        }else {
	        	mapEditor = NmapUiUtils.createNatlCntrsEditor("BasicWX-US","NSHARP" );
	        }
		
	       
	        for(int i=0; i< mapEditor.getDescriptor().getResourceList().size(); i++)
	        	System.out.println( "A resourcename="+mapEditor.getDescriptor().getResourceList().get(i).getResource().getName());
			RbdBundle rbd = RbdBundle.unmarshalRBD( rbdFile, null );
			ResourceBndlLoader rbdLoader = new ResourceBndlLoader("DefaultMap");
			rbdLoader.addRBD( rbd, mapEditor );
			VizApp.runSync( rbdLoader );
			//System.out.println("NsharpMapResource create editor "+ mapEditor.toString());
			for(int i=0; i< mapEditor.getDescriptor().getResourceList().size(); i++)
	        	System.out.println( "B resourcename="+mapEditor.getDescriptor().getResourceList().get(i).getResource().getName());
			
		}
		catch ( Exception ve ) {
			System.out.println("NsharpMapResource Could not load initial editor: " + ve.getMessage());
			ve.printStackTrace();
		}
	}*/
	private static void createMapEditorTest(){
		// create an editor MapEditor
		try {

			IEditorPart ep = EditorUtil.getActiveEditor();
			if (ep instanceof NCMapEditor) {
				mapEditor= (NCMapEditor) ep;
				System.out.println("NsharpMapResource using existing editor ");
			}else {
				mapEditor = NmapUiUtils.createNatlCntrsEditor("BasicWX-US","NSHARP" );


				RbdBundle rbd = RbdBundle.getDefaultRBD();
				ResourceBndlLoader rbdLoader = new ResourceBndlLoader("DefaultMap");
				rbdLoader.addRBD( rbd, mapEditor );
				VizApp.runSync( rbdLoader );
				System.out.println("NsharpMapResource create new editor "+ mapEditor.toString());
			}

			for(int i=0; i< mapEditor.getDescriptor().getResourceList().size(); i++)
				System.out.println( "Editor resource "+ i+" name="+mapEditor.getDescriptor().getResourceList().get(i).getResource().getName());

		}
		catch ( Exception ve ) {
			System.out.println("NsharpMapResource Could not load initial editor: " + ve.getMessage());
			ve.printStackTrace();
		}
	}
	public static void registerMouseHandler(){
		if(mouseHandlerRegistered)
			return;
		
		mouseHandler = getMouseHandler();
		if(mapEditor!=null && mouseHandler!=null){
			mapEditor.registerMouseHandler((IInputHandler) mouseHandler );
			mouseHandlerRegistered = true;
		}
	}
	public static void unregisterMouseHandler(){
		if(!mouseHandlerRegistered)
			return;
		mouseHandler = getMouseHandler();
		if(mapEditor!=null && mouseHandler!=null){
			mapEditor.unregisterMouseHandler((IInputHandler) mouseHandler );
			mouseHandlerRegistered= false;
		}
	}
	/**
     * Create a new MapResource and add it to the current editor.
     * @return the MapResource
     */
    public static NsharpMapResource getOrCreateNsharpMapResource() {
    	if(mapRsc == null ){
    		if(mapEditor == null)
    			createMapEditorTest();//createMapEditor();
    		if(mapEditor!=null){
    			IMapDescriptor desc = (IMapDescriptor) mapEditor.getActiveDisplayPane().getRenderableDisplay().getDescriptor();
    			try {	                
    				mapRsc = new NsharpMapResourceData().construct(new LoadProperties(), desc);
    				desc.getResourceList().add( mapRsc );
    				mapRsc.init( mapEditor.getActiveDisplayPane().getTarget());
    				
    				//register mouse handler
    				mouseHandler = getMouseHandler();
    		        mapEditor.registerMouseHandler((IInputHandler) mouseHandler );
    		    	
    	            
    			} catch (Exception e) {
    				e.printStackTrace();           
    			}
    		}
    	}
    	
        return mapRsc;
    }

    public static void deleteNsharpMapResource() {
    	//System.out.println("NsharpMapResource:deleteNsharpMapResource ");
    	if(mapRsc != null ){
    		mapRsc.dispose();
    		mapRsc = null;
    	}
    }

	/**
	 * Called when resource is disposed
	 * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
	 */
	@Override
	public void disposeInternal() {
		//System.out.println("NsharpMapResource:disposeInternal "+ this.toString());
		
		if(mapEditor != null ){
			mapEditor.unregisterMouseHandler( mouseHandler );
			mouseHandler = null;
            //close editor
			/*if((PlatformUI.getWorkbench()!= null)&&(PlatformUI.getWorkbench().getActiveWorkbenchWindow()!= null)
				&& (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()!=null))
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeEditor(mapEditor, false);*/
			mapEditor = null;
        	
        }
		pickedPoint = null;
		points = null;
		symbolSet = null;
		symbolToMark = null;
		mapRsc = null;
		if(waitCursor!=null)
			waitCursor.dispose();
		waitCursor=null;
		mouseHandlerRegistered= false;
	}


	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.rsc.IVizResource#getCoordinateReferenceSystem()
	 */
	public CoordinateReferenceSystem getCoordinateReferenceSystem() {

		if (descriptor == null)
			return null;

		return descriptor.getCRS();

	}

	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.rsc.IVizResource#getName()
	 */
	@Override
	public String getName() {

		return "NSHARP Resource";

	}


	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.rsc.IVizResource#getShortName()
	 */
	public String getShortName() {	

		return null;

	}

	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.IGraphicsTarget)
	 */
	@Override
	public void initInternal(IGraphicsTarget target) throws VizException {
		//System.out.println("NsharpMapResource:initInternal called");
		//mapfont = target.initializeFont("Monospace",
		//		(float) (12 * nsharpMapResourceData.getMarkerTextSize().getSoftwareSize()), null);
	}

	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.rsc.IVizResource#isApplicable(com.raytheon.viz.core.PixelExtent)
	 */
	public boolean isApplicable(PixelExtent extent) {

		return true;

	}

	private void generateSymbolForDrawing()
	{
		String type;
		float lineWidth = nsharpMapResourceData.getMarkerWidth();
		Boolean clear= false;;
		String category = new String("Marker");
		double sizeScale = nsharpMapResourceData.getMarkerSize();

		
		if (points.isEmpty() == true) {
			symbolSet = null;
		}
		else {
			//  SymbolLocationSet constructor requires a positive-length array of Coordinate
			Coordinate[] locations =  new Coordinate[points.size()];
			Color[] colors = new Color[] {new Color(NsharpConstants.color_green.red, 
					NsharpConstants.color_green.green,
					NsharpConstants.color_green.blue)};
			//System.out.println( "generateSymbolSet: size ="+ points.size());
			int i = 0;
			for (NsharpStationInfo p : points) {
				double lon, lat;
				lon = p.getLongitude();
				lat = p.getLatitude();
				locations[i++] = new Coordinate(lon,lat);
			}
			type = nsharpMapResourceData.getMarkerType().toString();
			//System.out.println( "generateSymbolSet done size ="+ i);
			symbolSet = new SymbolLocationSet (
					null,
					colors,
					lineWidth,
					sizeScale,
					clear,
					locations,
					category,
					type);


		}
		//generate symbol for picked stn to mark X 
		if(pickedPoint!= null && pickedPoint.size()>0){
			Coordinate[] locations =  new Coordinate[pickedPoint.size()];
			int i = 0;
			for (NsharpStationInfo p : pickedPoint) {
				double lon, lat;
				lon = p.getLongitude();
				lat = p.getLatitude();
				locations[i++] = new Coordinate(lon,lat);
			}
			type = nsharpMapResourceData.getStnMarkerType().toString();
			Color[] colors = new Color[] {new Color(NsharpConstants.color_red.red, 
					NsharpConstants.color_red.green,
					NsharpConstants.color_red.blue)};
			symbolToMark = new SymbolLocationSet(null,
					colors,
					lineWidth,
					sizeScale*2,
					clear,
					locations,
					category,
					type);
		}
		else
			symbolToMark= null;
	}

	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core.IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
	 */
	@Override
	public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
	throws VizException {
		//System.out.println("paintInternal called!");
		//IFont font = target.initializeFont("Monospace",
		//		(float) (12 * nsharpMapResourceData.getMarkerTextSize().getSoftwareSize()), null);

		generateSymbolForDrawing();
		DisplayElementFactory df = new DisplayElementFactory (target, this.descriptor);
		if (symbolSet != null)
		{	
			ArrayList<IDisplayable> elements = df.createDisplayElements(symbolSet, paintProps);
			for (IDisplayable each : elements)
			{
				try {
					each.draw(target, paintProps);
					each.dispose();
				}
				catch (Exception e) {
					e.printStackTrace();
					//System.out.println("paintInternal caught draw exception!");
				}
			}
		}
		if(symbolToMark != null){
			ArrayList<IDisplayable> elements = df.createDisplayElements(symbolToMark, paintProps);
			for (IDisplayable each : elements)
			{
				try {
					each.draw(target, paintProps);
					each.dispose();
				}
				catch (Exception e) {
					e.printStackTrace();
					//System.out.println("paintInternal caught draw exception!");
				}
			}
		}
		//font.dispose();
	}




	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.rsc.capabilities.IProjectableResource#isProjectable(org.opengis.referencing.crs.CoordinateReferenceSystem)
	 */
	public boolean isProjectable(CoordinateReferenceSystem mapData) {

		return true;

	}

	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org.opengis.referencing.crs.CoordinateReferenceSystem)
	 */
	@Override
	public void project(CoordinateReferenceSystem mapData) throws VizException {
		//System.out.println("NctextuiResource: project ");
	}
	
    /**
     * Returns the current mouse handler.
     * @return
     */   
    private static  NsharpMapMouseHandler getMouseHandler() {	
    
        if ( mouseHandler == null ) {
        	
        	mouseHandler = new NsharpMapMouseHandler();
        	
        }

        return mouseHandler;
        
    }
    
    @Override
	public boolean okToUnload() {
		/*
		 * DisAllow unloading of Resource 
		 */
		
		return false;
		
			
	}
	@Override
	public void notifyRemove(ResourcePair rp) throws VizException {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void propertiesChanged(ResourceProperties updatedProps) {
         if ( updatedProps.isVisible() ) {
        	 reopenTextView ();
         }
         else {
        	 hideTextView ();	 
         }
    }
    
	private void hideTextView () {
        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.nsharp" );
        if ( wpage.isPartVisible(vpart) ) {
        	NsharpPaletteWindow paletteWin = NsharpPaletteWindow.getInstance();
        	if(paletteWin!=null){
        		paletteWin.setEditorVisible(false);
        		wpage.hideView(vpart);
        	}
        }
	}
	    
	private void reopenTextView () {
        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.nsharp" );
        if ( !wpage.isPartVisible(vpart) ) {
        	NsharpPaletteWindow paletteWin = NsharpPaletteWindow.getInstance();
        	if(paletteWin!=null){
        		paletteWin.setEditorVisible(true);
        		try {
        			vpart = wpage.showView( "gov.noaa.nws.ncep.ui.nsharp" );
        		} catch (Exception e) {
        			e.printStackTrace();
        		}
        	}
        }
	}
}

