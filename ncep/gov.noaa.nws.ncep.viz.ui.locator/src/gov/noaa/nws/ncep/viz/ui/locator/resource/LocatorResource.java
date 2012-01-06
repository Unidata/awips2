package gov.noaa.nws.ncep.viz.ui.locator.resource;

import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.locator.LocatorEditDialog;
import gov.noaa.nws.ncep.viz.ui.locator.util.LocatorInfo;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;


public class LocatorResource  	extends AbstractVizResource<LocatorResourceData,IMapDescriptor> 
								implements INatlCntrsResource,ILocator {
	
	public static final String LOCATOR_NAME = "Locator";
	
	public static final String NO_DISPLAY = "";
	
	public static final String NOT_AVAILABLE = "---NO DATA---";
	
	private LocatorResourceData locatorResourceData;
	
	protected RGB color = new RGB(255, 255, 255);
	
	protected Coordinate coor = null;
	
	private File fontFile = PathManagerFactory.getPathManager().getStaticFile("fonts" + File.separator + "VeraMono.ttf");
	
	private IFont font = null;
	
	static {
		LocatorInfo.init();
	}

	public/*protected*/ LocatorResource(LocatorResourceData resourceData,	LoadProperties loadProperties) {
		super(resourceData, loadProperties);
		locatorResourceData = resourceData;
		locatorResourceData.setLocatorResource(this);
		
		new gov.noaa.nws.ncep.viz.ui.locator.LocatorCoorBean();
	}

	@Override
	protected void disposeInternal() {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void paintInternal(IGraphicsTarget target, PaintProperties paintProps) 
																	throws VizException {

		target.clearClippingPlane();		
		
        //File fontFile = PathManagerFactory.getPathManager().getStaticFile("fonts" + File.separator + "VeraMono.ttf");
        if(font == null) font = target.initializeFont(fontFile, 12.0f, null);
        

        double r = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;       
		double y0 = paintProps.getView().getExtent().getMaxY() - (7 * r);
		double x0 = paintProps.getView().getExtent().getMinX() + (18 * r);
		
		String[] ss = getLocatorLabels(coor);
		
		for(int i=0; i<ss.length; i++){
			
			double maxHeight = 0.0; 
			
			java.awt.geom.Rectangle2D textBounds = target.getStringBounds(null,ss[i]);
			
			if (textBounds.getHeight() > maxHeight) {
				if(ss[i]!=null && ( ! ss[i].isEmpty()) )	
					maxHeight = target.getStringBounds(null,"J_/").getHeight();//textBounds.getHeight();
		    }
			
			target.drawString(font,ss[i],x0, y0, 0, IGraphicsTarget.TextStyle.BLANKED, color, HorizontalAlignment.LEFT, 0.0);
			y0 -= (maxHeight * r);
		}

//		target.drawStrings(font, getLocatorLabels(coor), x0,y0,0.0, IGraphicsTarget.TextStyle.BLANKED, new RGB[]{color,new RGB(255,0,0),new RGB(0,255,0)}, HorizontalAlignment.LEFT, VerticalAlignment.BOTTOM);
		target.setupClippingPlane(paintProps.getClippingPane());		
	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		// TODO Auto-generated method stub		
	}
	
	@Override
	public String getName() {
		return LOCATOR_NAME;
	}
	
	public void setCoordinate(Coordinate ll){
		
		if(ll == null)
			coor = new Coordinate(0.0,0.0);
		
		coor = ll;
		NmapUiUtils.getActiveNatlCntrsEditor().refresh();
	}
	
	public static LocatorResource getLocatorResource(){
				
		AbstractEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
		if( editor != null ) {
			
			IRenderableDisplay disp = editor.getActiveDisplayPane().getRenderableDisplay();
			if( disp != null ) {
			
				IDescriptor idtor= disp.getDescriptor();
				if( idtor != null) {				
					ResourceList rscList = idtor.getResourceList();				
					
					for( ResourcePair rp : rscList ) {
						
						if(LocatorResource.LOCATOR_NAME.equals(rp.getResource().getName())){
							return (LocatorResource)rp.getResource();							
						}
					}										
				}
			}
		}
		return null;
	}
	
	@Override
    public LocatorResourceData getResourceData() {
        return resourceData;
    }

	@Override
	public void resourceAttrsModified() {
		// TODO Auto-generated method stub		
	}		

	/*
	 * All methods BELOW are originally from LocatorInfo's same name static methods; 
	 * with multi-pane, instance methods needed to avoid the case that all panes call 
	 * the same method thus display the SAME text!
	 */	
	
	public String[] getLocatorLabels(Coordinate coor){		
	
		LocatorEditDialog.initDisplayData( this);
			
		int length = LocatorEditDialog.NUM_OF_LOCATORS;
										
		String[] list = new String[length];
										
		for(int i=0; i<length; i++){	
				
			if( ! LocatorEditDialog.DISP_POSONOFF_MAP.get(this).get(i))
				list[i]=NO_DISPLAY;				
			else if(LocatorEditDialog.DISP_LOCATORLIST_MAP.get(this)[i] == null)
				list[i]=NOT_AVAILABLE;
			else 
				list[i] = getLocatorString(coor,LocatorEditDialog.DISP_LOCATORLIST_MAP.get(this)[i]);
		}			
		return list;		
	}	
	
	public String getLocatorString(Coordinate coor, Locator locator){
		
		String n = locator.getLocatorName(), s = locator.getShapefileName();
		
		//TODO: SFSTATION is null too, maybe others!
		if(s == null ){
			if(n.contains("LATLON")) 
				return getLatLonText(coor,locator);
			if( n.contains("SFSTATION"))
				return getBoundsDataText(coor,locator);
			
			return NOT_AVAILABLE;
		}else{			
		
			if(s.contains("stns"))
				return getPointDataText(coor,locator);
			
			if(s.contains("bounds"))
				return getBoundsDataText(coor,locator);
			
			return NOT_AVAILABLE;
		}
	}
	
	public String getLatLonText(Coordinate theLatLon, Locator l){

		if(theLatLon == null || l == null) 
			return NOT_AVAILABLE;		
	
		return  LocatorTool.formatCoordinate(theLatLon,l.getDisplayOptions().getLatLonUnit());
	}
	
	public String getPointDataText(Coordinate aLatLon, Locator l){
		if(aLatLon == null || l == null) 
			return NOT_AVAILABLE;
		
		DisplayOptions displayOption = ( l == null ? new DisplayOptions() : l.getDisplayOptions() );
		
		setDisOpsFields(displayOption);
		
		citiesPointData cities = new citiesPointData();	
		cities.setLocator(l);
			 
		String label = null;
		try{
			cities.setLatLon(aLatLon);
			cities.setRawPointList(null);
			label=cities.calculateNearestPoint2(aLatLon, displayOption);
			
			//label = cities.calculateNearestPoint(aLatLon, displayOption); 
		}catch(Exception e){
			System.out.println("____Error: in getPointDataText() of LocatorResource: "+e.getMessage());				 
		}
		
		return getNormalizedString(label);
	}
	
	public String getBoundsDataText(Coordinate coor, Locator l){		
		if(coor == null || l == null) 
			return NOT_AVAILABLE;
		
		try{ 

			LocatorBoundsResource lbr = new LocatorBoundsResource();
			lbr.setCurrentLocator(l);
			String s = lbr.getBoundsValue(coor);
			
			return getNormalizedString(s);
		
		}catch(Exception e){
			System.out.println("---Error in getBoundsDataText() of LocatorResource: "+e.getMessage());
		}		
		return NOT_AVAILABLE;
	}
	
	public void setDisOpsFields(DisplayOptions displayOptions){

		if(displayOptions.getRoundingToNearest() == null) displayOptions.setRoundingToNearest(1);
		if(displayOptions.getDistanceUnit() == null)	  displayOptions.setDistanceUnit(LocatorTool.DISTANCEUNIT_OPTIONS[2]);
		if(displayOptions.getDirectionUnit() == null)	  displayOptions.setDirectionUnit(LocatorTool.DIRECTIONUNIT_OPTIONS[1]);
		
	}
	
	public String getNormalizedString(String s){
		if(s==null || s.isEmpty())
			return NOT_AVAILABLE;
		
		s = s.replace("_", " ");
		return s.toUpperCase();
	}	
}
