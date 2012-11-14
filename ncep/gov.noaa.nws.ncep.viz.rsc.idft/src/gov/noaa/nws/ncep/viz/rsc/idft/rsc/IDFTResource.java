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
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
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
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;

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
 * </pre>
 * 
 * @author gzhang 
 * @version 1.0
 */
public class IDFTResource extends AbstractNatlCntrsResource<IDFTResourceData, MapDescriptor> 
                         implements INatlCntrsResource {
	
	ArrayList<DataTime> dataTimes = new ArrayList<DataTime>();
	
	private IDFTResourceData idftRscData;

    private IFont font;

	private class IDFTRscDataObj implements IRscDataObject {
    	Symbol   pointSymbol;
    	Vector	 vector;
    	DataTime dataTime;
//    	int 	 pointNum;  // not used
    	double   lat,lon;
    	double   direction;
    	double   distanceNm;
    	String   distanceInTenthString;
    	
		@Override
		public DataTime getDataTime() {		
			return dataTime;
		}
    }
	    
    private class FrameData extends AbstractFrameData {
        
        ArrayList<IDFTRscDataObj> idftDataList;

        public FrameData(DataTime frameTime, int timeInt) {
        	super( frameTime, timeInt );
        	idftDataList = new ArrayList<IDFTRscDataObj>();
    	}
        
    	public boolean updateFrameData( IRscDataObject rscDataObj ) {
    		if( !(rscDataObj instanceof IDFTRscDataObj) ) {
    			System.out.println("IDFT:updateFrameData expecting IDFTRscDataObj instead of: "+ 
    					rscDataObj.getClass().getName() );
    			return false;
    		}
    		else {
    			idftDataList.add( (IDFTRscDataObj)rscDataObj );
        		return true;
    		}
    	}
    	
    }
	
	public IDFTResource(IDFTResourceData ncresourceData,
			LoadProperties loadProperties) {
    	super(ncresourceData, loadProperties);
    	this.idftRscData = (IDFTResourceData) resourceData;    	
    }
	
    private IDFTRscDataObj getIDFTRscDataObj( IdftRecord idftRec){
    	IDFTRscDataObj idftData = new IDFTRscDataObj();
    	
    	// the dataTime uses the issue time as the refTime and computes the forecast hour from
    	// the validTime in the record.
//    	int fcstSecs = (int)(idftRec.getValidTime().getTime().getTime() / 1000) -
//    			       (int)(idftRec.getIssueTime().getTime().getTime() / 1000);
//    	idftData.dataTime = new DataTime( idftRec.getIssueTime().getTime(), fcstSecs );
    	idftData.dataTime = idftRec.getDataTime();
//    	idftData.pointNum = idftRec.getPointNum();  // not used
    	idftData.lat = idftRec.getLat();
    	idftData.lon = idftRec.getLon();
    	idftData.direction = idftRec.getDirection();
    	idftData.distanceNm = idftRec.getDistanceNm(); 
    	idftData.distanceInTenthString = Integer.toString((int)(Math.round(idftData.distanceNm*10))); 
    	com.vividsolutions.jts.geom.Coordinate coor = new com.vividsolutions.jts.geom.Coordinate(idftData.lon,idftData.lat);
    	idftData.vector = new Vector( null,
				new Color[]{Color.YELLOW},
				idftRscData.getArrowLineWidth().floatValue(),//lineWidth
				idftRscData.getArrowLength(),//sizeScale 
				true,   coor,
				gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType.ARROW,
				idftData.distanceNm,
				idftData.direction,
				idftRscData.getArrowLength(),//arrowHeadSize, using arrowLength for a match
				true, "Vector", "Arrow"); // hard-code for clarity
    	idftData.pointSymbol = new Symbol( null,
    			new Color[]{Color.RED},
    			idftRscData.getArrowLineWidth().floatValue(),// lineWidth, same as arrow's
    			idftRscData.getPointSize(),
    			true, coor,
    			"Symbol", "DOT");

        return idftData;
    }
  

	protected IRscDataObject[] processRecord( Object pdo ) {
		if( !(pdo instanceof IdftRecord) ) {
			System.out.println( "IDFT processRecord() : Expecting IdftRecord object instead of: "+
					pdo.getClass().getName() );
			return null;
		}
		
		IDFTRscDataObj idftRscDataObj = getIDFTRscDataObj( (IdftRecord)pdo );

		if( idftRscData == null ) {
			return new IDFTRscDataObj[0];
		}
		else {
			return new IDFTRscDataObj[]{ idftRscDataObj };
		}
	}
	
	public void paintFrame( AbstractFrameData frameData, IGraphicsTarget grphTarget, PaintProperties paintProps) throws VizException {
		FrameData currFrameData = (FrameData)frameData;
       	RGB pointColorRGB = idftRscData.getPointColor();
    	java.awt.Color pointColor = new java.awt.Color(pointColorRGB.red,pointColorRGB.green,pointColorRGB.blue);
    	
    	RGB    arrowColorRGB = idftRscData.getArrowColor();
    	java.awt.Color arrowColor = new java.awt.Color(arrowColorRGB.red,arrowColorRGB.green,arrowColorRGB.blue);
    	
    	DisplayElementFactory df = new DisplayElementFactory( grphTarget, this.descriptor );
    	
    	for( IDFTRscDataObj idftData : currFrameData.idftDataList ){
        	IExtent extent = paintProps.getView().getExtent();
        	double maxX = (extent.getMaxX() < 0 ? 0 : extent.getMaxX() );
        	double minX = (extent.getMinX() < 0 ? 0 : extent.getMinX() );
        	double maxY = (extent.getMaxY() < 0 ? 0 : extent.getMaxY() );
        	double minY = (extent.getMinY() < 0 ? 0 : extent.getMinY() );
        	maxX = ( maxX > 19999 ? 19999 : maxX);
        	minX = ( minX > 19999 ? 19999 : minX);
        	maxY = ( maxY >  9999 ?  9999 : maxY);
        	minY = ( minY >  9999 ?  9999 : minY);

        	PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY, maxY);
        	
        	idftData.pointSymbol.setColors(new java.awt.Color[]{ pointColor });
        	
        	idftData.vector.setSizeScale( idftRscData.getArrowLength() );
        	idftData.vector.setLineWidth( idftRscData.getArrowLineWidth().floatValue() );
        	idftData.vector.setColors(new java.awt.Color[]{ arrowColor });
        	
        	double[] zonePix = this.descriptor.worldToPixel(new double[]{(double) idftData.lon,(double) idftData.lat});
        	if( zonePix != null && correctedExtent.contains( zonePix[0], zonePix[1] ) ) {

        		//TODO:  Suggest refactor to aggregate into List<IVector> here, then (after loop)
        		//       call df.createDisplayElements( List<IVector>, PaintProperties )
        		//       for faster performance.
        		ArrayList<IDisplayable> displayElsArrow = df.createDisplayElements( (IVector)idftData.vector , paintProps );
        		for ( IDisplayable each : displayElsArrow ) {
        			each.draw(grphTarget, paintProps);
        			each.dispose();
        		}

        		//TODO:  Suggest refactor to create SymbolLocationSet (for identical
        		//       symbols at many locations), then (after loop)
        		//       call df.createDisplayElements ( SymbolLocationSet, PaintProperties )
        		//       for faster performance.
        		ArrayList<IDisplayable> displayElsPoint = df.createDisplayElements( idftData.pointSymbol , paintProps );
        		for ( IDisplayable each : displayElsPoint ) {
        			each.draw(grphTarget, paintProps);
        			each.dispose();
        		}

        		grphTarget.drawString( font, idftData.distanceInTenthString, 
        				zonePix[0], zonePix[1], 0.0, TextStyle.NORMAL, 
        				idftRscData.getDistanceColor(),
        				HorizontalAlignment.CENTER, 0.0 );

    		} 
    	}
	}
	
	public void disposeInternal() {  
		super.disposeInternal();
		if( font != null ) {
			font.dispose();
		}    
	}
	
	public void resourceAttrsModified() {
		// don't need to do anything
	}
	
	protected AbstractFrameData createNewFrame(DataTime frameTime, int timeInt) {
		return new FrameData(frameTime,timeInt);
	}
	
	@Override
	public void initResource(IGraphicsTarget grphTarget) throws VizException {

		//  Request from the DB all IDFT data corresponding to the
		//  selected cycle (initial) time, and use the returned PDOs to
		//  fill up the newRscDataObjsQueue (with RDOs) for later use.
		
		//  [ TODO:  Note modeled after FcstPlotResource.initResource().  Is there
		//           a more efficient way to do this (in the uEngine), i.e., construct
		//           a single query to match all data keyed to a given reference time,
		//           rather than filtering the times here, and then doing a separate
		//           query for each forecast hour? ]
		
		//  First, query all of the times in the DB...
		
		ResourceName rscName = getResourceData().getResourceName();
		DataTime   cycleTime = rscName.getCycleTime();

		//  ("Latest" should already be resolved here)
		if( cycleTime == null || rscName.isLatestCycleTime() ) { 
			return;
		}

		HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
				resourceData.getMetadataMap());

		LayerProperty property = new LayerProperty();
		property.setDesiredProduct( ResourceType.PLAN_VIEW );
		DataTime[] availableTimes;

		try {
			property.setEntryQueryParameters( queryList );
			availableTimes = property.getEntryTimes();
		}
		catch( VizException e) {
			throw e;
		}

		//  ...then loop through all the available times in the DB and if the
		//  reference time matches the cycle time for this resource and if it
		//  hasn't already been added, add it to the list of matching DataTime.
		
		for( DataTime dt : availableTimes ) {
			// create a dataTime without a possible validPeriod.
			DataTime availTime = new DataTime( dt.getRefTime(), dt.getFcstTime() );
			DataTime refTime = new DataTime( dt.getRefTime() );

			if( cycleTime.equals( refTime ) ) {
				if( !dataTimes.contains( availTime ) ) {
					dataTimes.add( availTime );
				}
			}
		}

		//  Now, dataTimes is limited to those that apply to the selected cycle time.
		//  For each, query the DB for matching records (PDOs), process into RDOs, and
		//  add to newRscDataObjsQueue.

		for( DataTime dt : dataTimes ) {
			RequestConstraint timeConstraint = new RequestConstraint( dt.toString() );
			queryList.put("dataTime", timeConstraint );
			LayerProperty prop = new LayerProperty();
			prop.setDesiredProduct(ResourceType.PLAN_VIEW);
			prop.setEntryQueryParameters(queryList, false);
			prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap
			String script = null;
			script = ScriptCreator.createScript(prop);
			if (script == null)
				return;

			Object[] pdoList = Connector.getInstance().connect(script, null, 60000);

			for (Object pdo : pdoList) {
				for( IRscDataObject dataObject : processRecord( pdo ) )	{	
					newRscDataObjsQueue.add(dataObject);
				}
			}
		}

	}
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.idftDataList.size() == 0) {
			return legendString + "-No Data";
		}		
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
