/*
 * gov.noaa.nws.ncep.ui.nctextui.rsc.NctextuiResource
 * 
 * 1/7/2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * @author Chin Chen
 * @version 1.0
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/28/2011    T402       X. Guo     Re-format NCTEXT view panel, check
 *                                     the click action on nctext legend
 * 02/15/2012    T627       Archana    Updated the call to addRbd() to accept 
 *                                     a NCMapEditor object as one of the arguments
 *                                     Removed the call to setNcEditor()                                       
 * 08/17/2012    T655       B. Hebbard Added paintProps as parameter to IDisplayable draw (2)
 */
package gov.noaa.nws.ncep.ui.nctextui.rsc;


import java.awt.Color;
import java.io.File;
import java.util.List;
import java.util.ArrayList;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.EditorUtil;

import gov.noaa.nws.ncep.ui.nctextui.dbutil.NctextStationInfo;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.ui.nctextui.palette.NctextuiPaletteWindow;

public class NctextuiResource  extends AbstractVizResource<NctextuiResourceData,MapDescriptor> {

	private NctextuiResourceData nctextuiResourceData;
	private static NctextuiResource nctextuiResource=null;
	/** The set of symbols with similar attributes across many locations */
    private SymbolLocationSet symbolSet = null;
    private SymbolLocationSet pickedSymbolSet = null;
    private static NCMapEditor mapEditor=null;
//    private static int mapEditorNum=0;
    private static  NctextuiMouseHandler mouseHandler;
    
    /*public static NCMapEditor getOrCreateMapEditor() {
		if(mapEditor== null)
			createMapEditor();
		return mapEditor;
	}*/
    public static NCMapEditor getMapEditor() {
    	
		return mapEditor;
	}
   
    private List<NctextStationInfo> points = new ArrayList<NctextStationInfo>();
    private List<NctextStationInfo> pickedStnPt = new ArrayList<NctextStationInfo>();
    
	public List<NctextStationInfo> getPickedStnPt() {
		return pickedStnPt;
	}

	public void setPickedStnPt(List<NctextStationInfo> pickedStnPt) {
		if (pickedStnPt == null)
			this.pickedStnPt.clear();
		else
			this.pickedStnPt = pickedStnPt;
	}

	public List<NctextStationInfo> getPoints() {
		return points;
	}

	public void setPoints(List<NctextStationInfo> points) {
		if (points == null)
			this.points.clear();
		else
			this.points = points;
	}
	
	private static void createMapEditor(){
		// create an editor MapEditor
		if(mapEditor != null)
			return;
		
		try {
			IEditorPart ep = EditorUtil.getActiveEditor();
			if ( ep instanceof NCMapEditor ) {
				mapEditor= (NCMapEditor) ep;
			}
			else {
				mapEditor = NmapUiUtils.createNatlCntrsEditor("BasicWX-US","NCTEXT" );

				RbdBundle rbd = RbdBundle.getDefaultRBD();
				ResourceBndlLoader rbdLoader = new ResourceBndlLoader("DefaultMap");
				rbdLoader.addRBD( rbd, mapEditor );
				VizApp.runSync( rbdLoader );
			}
			//register mouse handler
			mouseHandler = getMouseHandler();
			mapEditor.registerMouseHandler((IInputHandler) mouseHandler );
			//System.out.println("NctextuiPaletteWindow create editor "+ mapEditor.toString());
		}
		catch ( Exception ve ) {
			System.out.println("Could not load initial editor: " + ve.getMessage());
			ve.printStackTrace();
		}
	}
	/**
     * Create a new resource and add it to the current editor.
     * @return the Resource
     */
    private static NctextuiResource createNewResource(NCMapEditor mapEditor) {
    	if(mapEditor != null){
    		IMapDescriptor desc = (IMapDescriptor) mapEditor.getActiveDisplayPane().getRenderableDisplay().getDescriptor();
    		nctextuiResource = new NctextuiResource(new NctextuiResourceData(),new LoadProperties());
    		desc.getResourceList().add( nctextuiResource );
    		try {
    			nctextuiResource.init( mapEditor.getActiveDisplayPane().getTarget());
    		} catch (VizException e) {

    			e.printStackTrace();
    		}
    		//System.out.println("NctextuiPaletteWindow createNewResource");
    		
    	}
        return nctextuiResource;
    }
    

	public static NctextuiResource getNctextuiResource() {
		if(nctextuiResource == null){
			if(mapEditor == null )
				createMapEditor();
			//nctextuiResource = createNewResource(mapEditor);
			if ( mapEditor != null) {
				IMapDescriptor desc = (IMapDescriptor) mapEditor.getActiveDisplayPane().getRenderableDisplay().getDescriptor();
    			try {	                
    				nctextuiResource = new NctextuiResource(new NctextuiResourceData(),new LoadProperties());
    	    		desc.getResourceList().add( nctextuiResource );
    				nctextuiResource.init( mapEditor.getActiveDisplayPane().getTarget()); 	
    	            
    			} catch (Exception e) {
    				e.printStackTrace();           
    			}
			}
		}
		return nctextuiResource;
	}
    
	/**
	 * Default constructor
	 */
	protected NctextuiResource(NctextuiResourceData resourceData,
			LoadProperties loadProperties) {
		super(resourceData, loadProperties);
		this.nctextuiResourceData = resourceData;
	}

	/**
	 * Called when resource is disposed
	 * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
	 */
	@Override
	public void disposeInternal() {
		//System.out.println("NctextuiResource:disposeInternal");
		if(mapEditor != null ){
			mapEditor.unregisterMouseHandler( mouseHandler );
			mouseHandler = null;
            //close editor
//			if((PlatformUI.getWorkbench()!= null)&&(PlatformUI.getWorkbench().getActiveWorkbenchWindow()!= null)
//				&& (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()!=null)){
				
				//System.out.println("NctextuiResource:disposeInternal close map editor");
//				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeEditor(mapEditor, false);
//			}
        	mapEditor = null;
//        	mapEditorNum=0;
        }
		closeTextView ();
		nctextuiResource = null;
	}
	
	public static void registerMouseHandler(){
		mouseHandler = getMouseHandler();
		if(mapEditor!=null && mouseHandler!=null)
			mapEditor.registerMouseHandler((IInputHandler) mouseHandler );
	}
	public static void unregisterMouseHandler(){
		mouseHandler = getMouseHandler();
		if(mapEditor!=null && mouseHandler!=null)
			mapEditor.unregisterMouseHandler((IInputHandler) mouseHandler );
	}

	@Override
	public void propertiesChanged(ResourceProperties updatedProps) {
		//System.out.println("NctextuiResource:propertiesChanged");
         if ( updatedProps.isVisible() ) {
        	 reopenTextView ();
         }
         else {
        	 hideTextView ();	 
         }
    }
    
	private void hideTextView () {
        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        //System.out.println("NctextuiResource:hideTextView");
        IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.NCTEXTUI" );
        if ( wpage.isPartVisible(vpart) ) {
        	NctextuiPaletteWindow paletteWin = NctextuiPaletteWindow.getAccess();
        	paletteWin.setEditorVisible(false);
        	wpage.hideView(vpart);
        }
	}
	private void closeTextView () {
		//System.out.println("NctextuiResource:closeTextView");
		IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		if(wpage!= null){
			IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.NCTEXTUI" );
			wpage.hideView(vpart);
		}
        
        NmapUiUtils.setPanningMode();
	}
    
	private void reopenTextView () {
		//System.out.println("NctextuiResource:reopenTextView");
        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.NCTEXTUI" );
        if ( !wpage.isPartVisible(vpart) ) {
        	NctextuiPaletteWindow paletteWin = NctextuiPaletteWindow.getAccess();
        	paletteWin.setEditorVisible(true);
        	try {
        	    vpart = wpage.showView( "gov.noaa.nws.ncep.ui.NCTEXTUI" );
        	} catch (Exception e) {
        		e.printStackTrace();
        	}
        }
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

		return "NCText";

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
		//System.out.println("NctextuiResource:initInternal");
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
		float lineWidth = nctextuiResourceData.getMarkerWidth();
		Boolean clear= false;;
		String category = new String("Marker");
		double sizeScale = nctextuiResourceData.getMarkerSize();
		//NctextuiPaletteWindow nctextuiPaletteWindow = NctextuiPaletteWindow.getAccess();
		if (points.isEmpty() == true) {
			symbolSet = null;
		}
		else {
			//  SymbolLocationSet constructor requires a positive-length array of Coordinate
			Coordinate[] locations = new Coordinate[points.size()];
			
			//System.out.println( "generateSymbolSet: size ="+ points.size());
			int i = 0;
			for (NctextStationInfo p : points) {
				double lon, lat;
				lon = p.getLongitude();
				lat = p.getLatitude();
				locations[i++] = new Coordinate(lon,lat);
				
				
			}

			Color[] colors = new Color[] {new Color(nctextuiResourceData.getColor().red, 
					nctextuiResourceData.getColor().green,
					nctextuiResourceData.getColor().blue)};
			type = nctextuiResourceData.getMarkerType().toString();
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
		if (pickedStnPt.isEmpty() == true) {
			pickedSymbolSet = null;
		}
		else {
			//  SymbolLocationSet constructor requires a positive-length array of Coordinate
			Coordinate[] locations = new Coordinate[pickedStnPt.size()];
			
			//System.out.println( "generatePickedSymbolSet: size ="+ pickedStnPt.size());
			int i = 0;
			for (NctextStationInfo p : pickedStnPt) {
				double lon, lat;
				lon = p.getLongitude();
				lat = p.getLatitude();
				locations[i++] = new Coordinate(lon,lat);				
			}

			Color[] colors = new Color[] {new Color(nctextuiResourceData.getPkStncolor().red, 
					nctextuiResourceData.getPkStncolor().green,
					nctextuiResourceData.getPkStncolor().blue)};
			type = nctextuiResourceData.getPkStnmarkerType().toString();
			
			pickedSymbolSet = new SymbolLocationSet (
					null,
					colors,
					lineWidth,
					sizeScale,
					clear,
					locations,
					category,
					type);
		
		
		}
		
		
	}

	/* (non-Javadoc)
	 * @see com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core.IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
	 */
	@Override
	public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
	throws VizException {
		//System.out.println("paintInternal called!");
		IFont font = target.initializeFont("Monospace",
				(float) (12 * nctextuiResourceData.getMarkerTextSize().getSoftwareSize()), null);

		generateSymbolForDrawing();
		
		if (symbolSet != null)
		{
			
			
			DisplayElementFactory df = new DisplayElementFactory (target, this.descriptor);
			ArrayList<IDisplayable> elements = df.createDisplayElements(symbolSet, paintProps);
			for (IDisplayable each : elements)
			{
				try {
					each.draw(target, paintProps);
					each.dispose();
				}
				catch (Exception e) {
		        	
		        	//e.printStackTrace();
					//System.out.println("paintInternal caught draw exception!");
				}
			}
		}
		if (pickedSymbolSet != null)
		{
			
			
			DisplayElementFactory df = new DisplayElementFactory (target, this.descriptor);
			ArrayList<IDisplayable> elements = df.createDisplayElements(pickedSymbolSet, paintProps);
			for (IDisplayable each : elements)
			{
				try {
					each.draw(target, paintProps);
					each.dispose();
				}
				catch (Exception e) {
		        	
		        	//e.printStackTrace();
					//System.out.println("paintInternal caught draw exception on pickedSymbolSet!");
				}
			}
		}
		font.dispose();
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
	private static  NctextuiMouseHandler getMouseHandler() {	
	    
        if ( mouseHandler == null ) {
        	
        	mouseHandler = new NctextuiMouseHandler();
        	
        }

        return mouseHandler;
        
    }

}

