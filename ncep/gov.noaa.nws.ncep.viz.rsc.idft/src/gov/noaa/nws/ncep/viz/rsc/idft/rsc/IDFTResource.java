/*
 * gov.noaa.nws.ncep.viz.idft.rsc.IDFTResource
 * 
 * September 2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.idft.rsc;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.display.IVector;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.DrawableString;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display IDFT data.
 * 
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/11/2009   154        Gang Zhang  Initial creation.
 * 06/03/2010   migration  ghull       to11dr11
 * 09/30/2010   307        ghull       dont need to override queryRecords anymore
 * 10/25/2010   307        ghull       rm idftParam for the forecast day and use the
 *                                     forecast hour in the data.
 * 04/11        #?         B. Yin      Re-factor IAttribute
 * 01/06/2012   530        B. Hebbard  In initResource filter DB query by cycle time
 * 05/23/12     785        Q. Zhou     Added getName for legend.
 * 08/17/12     655        B. Hebbard  Added paintProps as parameter to IDisplayable draw (2)
 * 03/07/13     982        Archana     Refactored initResource(), paintFrame() and getIDFTRscDataObj()
 *                                     to improve the performance.     
 * </pre>
 * 
 * @author gzhang 
 * @version 1.0
 */
public class IDFTResource extends
		AbstractNatlCntrsResource<IDFTResourceData, NCMapDescriptor> implements
		INatlCntrsResource {
	
	ArrayList<DataTime> dataTimes = new ArrayList<DataTime>();
	
	private IDFTResourceData idftRscData;

    private IFont font;

	private class IDFTRscDataObj implements IRscDataObject {
    	DataTime dataTime;
		double lat, lon;
    	double   direction;
		float distanceNm;
		String idftDistanceStr;
		public String pointNum;
    	
		@Override
		public DataTime getDataTime() {		
			return dataTime;
		}
    }
	    
    private class FrameData extends AbstractFrameData {
        
        ArrayList<IDFTRscDataObj> idftDataList;

        public FrameData(DataTime frameTime, int timeInt) {
			super(frameTime, timeInt);
        	idftDataList = new ArrayList<IDFTRscDataObj>();
    	}
        
		@Override
		public boolean updateFrameData(IRscDataObject rscDataObj) {
			if (!(rscDataObj instanceof IDFTRscDataObj)) {
				System.out
						.println("IDFT:updateFrameData expecting IDFTRscDataObj instead of: "
								+ rscDataObj.getClass().getName());
    			return false;
			} else {
				idftDataList.add((IDFTRscDataObj) rscDataObj);
        		return true;
    		}
    	}
    	
    }
	
	public IDFTResource(IDFTResourceData ncresourceData,
			LoadProperties loadProperties) {
    	super(ncresourceData, loadProperties);
    	this.idftRscData = (IDFTResourceData) resourceData;    	
    }
	
	private IDFTRscDataObj getIDFTRscDataObj(IdftRecord idftRec) {
    	IDFTRscDataObj idftData = new IDFTRscDataObj();
    	
		idftData.dataTime = new DataTime ( idftRec.getValidTime());//idftRec.getDataTime();
		idftData.pointNum = idftRec.getPointNum().toString(); // not used
    	idftData.lat = idftRec.getLat();
    	idftData.lon = idftRec.getLon();
    	idftData.direction = idftRec.getDirection();
    	idftData.distanceNm = idftRec.getDistanceNm(); 
		idftData.idftDistanceStr = String.format("%.1f", idftData.distanceNm) ;

        return idftData;
    }
  
	@Override
	protected IRscDataObject[] processRecord(Object pdo) {
		if (!(pdo instanceof IdftRecord)) {
			System.out
					.println("IDFT processRecord() : Expecting IdftRecord object instead of: "
							+ pdo.getClass().getName());
			return null;
		}
		
		IDFTRscDataObj idftRscDataObj = getIDFTRscDataObj((IdftRecord) pdo);

		if (idftRscData == null) {
			return new IDFTRscDataObj[0];
		} else {
			return new IDFTRscDataObj[] { idftRscDataObj };
		}
	}
	
	@Override
	public void paintFrame(AbstractFrameData frameData,
			IGraphicsTarget grphTarget, PaintProperties paintProps)
			throws VizException {
		
		
		double screenToWorldRatio = paintProps.getCanvasBounds().width /paintProps.getView().getExtent().getWidth();
		FrameData currFrameData = (FrameData) frameData;
		
		if (currFrameData.idftDataList == null || currFrameData.idftDataList.isEmpty() )
			 return;
		RGB stationNumColor       = new RGB(0,255,0);
       	RGB pointColorRGB = idftRscData.getPointColor();
		java.awt.Color pointColor = new java.awt.Color(pointColorRGB.red,
				                        pointColorRGB.green, pointColorRGB.blue);
		Coordinate dummyCoordinate = new Coordinate(0.0,0.0);
		
		/*
		 * Create the symbol once and render it at all the different locations
		 */
		
		Symbol pointSymbol           = new Symbol(null, new Color[] { pointColor},
				idftRscData.getArrowLineWidth().floatValue(),idftRscData.getPointSize(),
                true, dummyCoordinate, "Symbol", "DOT");		
    	
    	RGB    arrowColorRGB = idftRscData.getArrowColor();
		java.awt.Color arrowColor = new java.awt.Color(arrowColorRGB.red,
				                        arrowColorRGB.green, arrowColorRGB.blue);		
		/*
		 * Create the vector once and render it at all the different locations
		 */
    	
		Vector arrowVector = new Vector(
				new Coordinate[]{dummyCoordinate},
				new Color[]{ arrowColor},
				idftRscData.getArrowLineWidth().floatValue(),// lineWidth
				idftRscData.getArrowLength(),// sizeScale
				true, dummyCoordinate,
				gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType.ARROW,
				1.0, 1.0,
				idftRscData.getArrowLength(),// arrowHeadSize, using arrowLength for a match
				true, "Vector", "Arrow");
		
		

		DisplayElementFactory df = new DisplayElementFactory(grphTarget,getNcMapDescriptor());
    	
		List<IVector> arrowList      = new ArrayList<IVector>();
		List<Coordinate> listOfSymbolCoords = new ArrayList<Coordinate>();
		Coordinate[] arrayOfSymbolCoords = null;

		for (IDFTRscDataObj idftData : currFrameData.idftDataList) {
        	IExtent extent = paintProps.getView().getExtent();
			double maxX = (extent.getMaxX() < 0 ? 0 : extent.getMaxX());
			double minX = (extent.getMinX() < 0 ? 0 : extent.getMinX());
			double maxY = (extent.getMaxY() < 0 ? 0 : extent.getMaxY());
			double minY = (extent.getMinY() < 0 ? 0 : extent.getMinY());
			maxX = (maxX > 19999 ? 19999 : maxX);
			minX = (minX > 19999 ? 19999 : minX);
			maxY = (maxY > 9999 ? 9999 : maxY);
			minY = (minY > 9999 ? 9999 : minY);

			PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY,
					maxY);


			
			Coordinate worldCoord = new Coordinate(idftData.lon, idftData.lat);
		
		    double[] zonePix = this.descriptor.worldToPixel(new double[] {
					(double) idftData.lon, (double) idftData.lat });
			if (zonePix != null
					&& correctedExtent.contains(zonePix[0], zonePix[1])) {
				
			    arrowVector = (Vector) arrowVector.copy();
				arrowVector.setLocation(worldCoord);
				arrowVector.setDirectionOnly(true);
				arrowVector.setDirection(idftData.direction);
				arrowVector.setSizeScale(idftRscData.getArrowLength());
				arrowVector.setLineWidth(idftRscData.getArrowLineWidth().floatValue());
				arrowList.add( arrowVector);				
				
				listOfSymbolCoords.add(worldCoord);
				DrawableString distanceInTenthStringTxt = new DrawableString(
						idftData.idftDistanceStr,
						idftRscData.getDistanceColor());
				distanceInTenthStringTxt.setCoordinates(zonePix[0], zonePix[1]);
				distanceInTenthStringTxt.textStyle = TextStyle.NORMAL;
				distanceInTenthStringTxt.horizontalAlignment = HorizontalAlignment.LEFT;
				grphTarget.drawStrings(distanceInTenthStringTxt);

				if ( idftRscData.getDisplayStationNumber() ){
					DrawableString stationNumber               = new DrawableString ( idftData.pointNum, stationNumColor );
                    stationNumber.textStyle                    = TextStyle.BOXED;
                    Rectangle2D distanceInTenthStringTxtBounds = grphTarget.getStringsBounds(distanceInTenthStringTxt);
                    
                    stationNumber.setCoordinates(zonePix[0] + distanceInTenthStringTxtBounds.getWidth()/screenToWorldRatio, 
                    		                     zonePix[1] + distanceInTenthStringTxtBounds.getHeight()/screenToWorldRatio );
                    grphTarget.drawStrings(stationNumber);
				}

			}
		}

		ArrayList<IDisplayable> displayElsArrow = df.createDisplayElements(arrowList, paintProps);
        for (IDisplayable each : displayElsArrow) {
        			each.draw(grphTarget, paintProps);
        			each.dispose();
        		}
        arrayOfSymbolCoords = new Coordinate[listOfSymbolCoords.size()];
        arrayOfSymbolCoords = listOfSymbolCoords.toArray(arrayOfSymbolCoords);
		SymbolLocationSet pointLocationSet = new SymbolLocationSet(pointSymbol, arrayOfSymbolCoords);

		ArrayList<IDisplayable> displayElsPoint = df.createDisplayElements(pointLocationSet, paintProps);
		for (IDisplayable each : displayElsPoint) {
        			each.draw(grphTarget, paintProps);
        			each.dispose();
        		}


		
	
	}
	
	@Override
	public void disposeInternal() {  
		super.disposeInternal();
		if (font != null) {
			font.dispose();
		}    
	}
	
    @Override
	protected AbstractFrameData createNewFrame(DataTime frameTime, int timeInt) {
		return new FrameData(frameTime, timeInt);
	}
	
	@Override
	public void initResource(IGraphicsTarget grphTarget) throws VizException {

//		// Request from the DB all IDFT data corresponding to the
//		// selected cycle (initial) time, and use the returned PDOs to
//		// fill up the newRscDataObjsQueue (with RDOs) for later use.
//
		
		ResourceName rscName = getResourceData().getResourceName();
		DataTime   cycleTime = rscName.getCycleTime();

        HashMap<String, RequestConstraint> metadataMap = 
        	new HashMap<String, RequestConstraint>(
        			getResourceData().getMetadataMap() );
       
		RequestConstraint timeConstraint = new RequestConstraint(); 
		timeConstraint.setConstraintType(ConstraintType.EQUALS);
		timeConstraint.setConstraintValue(cycleTime.toString());

		metadataMap.put( "dataTime.refTime", timeConstraint );
		DbQueryRequest request = new DbQueryRequest();
		request.setConstraints(metadataMap);
		try{
			
			DbQueryResponse response = (DbQueryResponse) ThriftClient.sendRequest(request);
			if ( response != null ){
				List<Map<String, Object>> responseList = null;
	        	if (response != null) {
	        		responseList = response.getResults();
	        		for ( Map<String,Object> eachResponse : responseList ){
	        			Collection<Object> idftRecordObj = eachResponse.values();
	        			for (Object pdo : idftRecordObj ){
	        				for (IRscDataObject dataObject : processRecord(pdo)) {
	    					newRscDataObjsQueue.add(dataObject);
		}
		}
				}
			}
		}

		}catch(Exception e){
			e.printStackTrace();
		}

		
	}
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null
				|| fd.idftDataList.size() == 0) {
			return legendString + "-No Data";
		}		
		return legendString + " "
				+ NmapCommon.getTimeStringFromDataTime(fd.getFrameTime(), "/");
	}
}
