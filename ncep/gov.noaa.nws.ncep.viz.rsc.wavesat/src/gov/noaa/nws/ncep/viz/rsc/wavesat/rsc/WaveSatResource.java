package gov.noaa.nws.ncep.viz.rsc.wavesat.rsc;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;

import java.awt.geom.Rectangle2D;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;
import java.util.TreeMap;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;


/**
 * WaveSatResource - Display SGWH data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  09/21/2011    #248     Greg Hull    Initial creation. 
 *  02/16/2012    #555     S. Gurung    Added call to setPopulated(true) in queryData().
 *  05/23/2012    #785     Q. Zhou      Added getName for legend
 *  12/19/2012    #960     Greg Hull    override propertiesChanged() to update colorBar.
 *
 * </pre>
 * 
 * @author ghull 
 * @version 1.0
 */
public class WaveSatResource extends AbstractNatlCntrsResource<WaveSatResourceData, MapDescriptor> 
	implements INatlCntrsResource {

	private WaveSatResourceData waveSatRscData;
	// TODO confirm the units are correct
	private Unit<?> WaveHeightUnits = SI.METER;  
	private Unit<?> WindSpeedUnits = SI.METERS_PER_SECOND;
//	private Unit<?> WaveHeightUnits = SI.METER;

    private boolean needsUpdate = true;

    private IFont font=null;
        
    protected ColorBarResource cbarResource;
    protected ResourcePair     cbarRscPair;
    
    private static String LAT_PARM = "CLATH";
    private static String LON_PARM = "CLONH";
    private static String REFTIME_PARAM = "REFTIME";
    private static String SAT_ID_PARAM = "SAID";
    private static String WAVE_HEIGHT_PARAM ="SGWH";
    private static String WIND_SPEED_PARM = "WS10G3R1";
    private static String FOSTSGWH_PARM = "FOSTSGWH";

    // This is public since WaveSatAlertParser is creating 
    // a WaveSatRscDataObj.
    static public class WaveSatRscDataObj implements IRscDataObject {        
        DataTime dataTime;
        double   lat;
        double   lon;
        
        // TODO : remove this when we are able to create a full
        // WaveSatRscDataObj object from the parser either by
        // re-creating or re-querying using the URI.
        // Til then this is a flag to indicate that the waveHeight
        // is not set and we will need to requery the data using
        // a PointDataRequest.
        boolean  createdFromAutoUpdate = false;

        String   satelliteId;
        double   waveHeight;
        double   wavePeriod;
        double   windSpeed;
        
        public WaveSatRscDataObj() {
        	
        }

		@Override
		public DataTime getDataTime() {
			return dataTime;
		}
    }

    private Double MissingValue = -9999998.0;
    
    private class FrameData extends AbstractFrameData {
    	// a list might be fine.
        public TreeMap<Long, WaveSatRscDataObj> waveSatDataMap;  
        
        // a list of WaveSatRscDataObj objects created from the AlertParser
        // these need to be re-queried since they don't have the 
        // waveHeights set in them.
        public List<WaveSatRscDataObj> autoUpdateList;
        
        private PixelExtent lastPixelExtent;

        // one wireframe per color interval
        private List<IWireframeShape> waveHeightWireframes;

        private IWireframeShape timeDisplayWireframe;
        
       protected float prevZoomLevel=2f;
        
		public FrameData( DataTime frameTime, int timeInt ) {
			super( frameTime, timeInt );
			waveSatDataMap = new TreeMap<Long,WaveSatRscDataObj>();
			autoUpdateList  = new ArrayList<WaveSatRscDataObj>();
			waveHeightWireframes = new ArrayList<IWireframeShape>();
    	}

		public void populateFrame() throws VizException {
			
			if( isPopulated() ) {
				return;
			}

			HashMap<String, RequestConstraint> reqConstraints = 
	        	 new HashMap<String, RequestConstraint>( waveSatRscData.getMetadataMap() );

	        RequestConstraint timeConstraint = new RequestConstraint();
	        String[] constraintList = { startTime.toString(), endTime.toString() };
	        timeConstraint.setBetweenValueList( constraintList );
	        timeConstraint.setConstraintType( RequestConstraint.ConstraintType.BETWEEN);

	        reqConstraints.put("dataTime", timeConstraint);

	        queryData( reqConstraints );
		}
		
		// call by populateFrame with the start/end time contraints and
		// by the updateFrameDataFromAutoUpdateList with the id constraints
		//
		public void queryData( HashMap<String, RequestConstraint> reqConstraints ) throws VizException {
	        
	        List<String> params = new ArrayList<String>();
	        params.add( LAT_PARM );
	        params.add( LON_PARM );
	        params.add( REFTIME_PARAM );
	        params.add( SAT_ID_PARAM );
	        params.add( WAVE_HEIGHT_PARAM );
	        params.add( WIND_SPEED_PARM );
	        params.add( FOSTSGWH_PARM );
	        
	        PointDataContainer pdc = null;
	            	            
	        try {
	        	// TODO : do we need to use the DataCubeContainer? 
	        	///PointDataContainer tempPdc = null;
//	        		DataCubeContainer
//	        			.getPointData( waveSatRscData.getPluginName(), 
//	        				params.toArray( new String[params.size()] ),
//	        				null, map); //levelKey

	        				// Datacube didn't have proper plugin; going directly
	        				// to the data store
	        	pdc = PointDataRequest.requestPointDataAllLevels(
	        						null, waveSatRscData.getPluginName(), 
	        						params.toArray( new String[params.size()] ),
	        						null, reqConstraints );

	        } catch (VizException e1) {
	        	System.out.println("Error querying for sgwh point data");
	        	throw e1;
	        }
	            
	        if( pdc == null ) {
	        	return;
	        }
	        else {
	        	pdc.setCurrentSz(pdc.getAllocatedSz());
	        }
	                                    	                    
	        for( int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++ ) {
	            PointDataView pdv = pdc.readRandom(uriCounter);
	            if( pdv == null ) { // ???
	            	continue;
	            }
	            
//	            if( timeMatch( new DataTime( new Date( (Long)pdv.getNumber( REFTIME_PARAM ) ) ) ) < 0 ) {
//	            	System.out.println("Sanity check : time constrained query doesn't match frame times??");
//	            	System.out.println("start/end "+ startTime + " " + endTime );
//	            	System.out.println("  "+ new DataTime( new Date( (Long)pdv.getNumber( REFTIME_PARAM ) ) ).toString() );
//	            }
				for( IRscDataObject dataObject : processRecord( pdv ) )	{	
					newRscDataObjsQueue.add(dataObject);
				}
			}
	        
	        setPopulated(true);
		}

		// query the data in the autoUpdateList and add results to the waveSatDataMap
		//
		public void updateFrameDataFromAutoUpdateList() {
			
			HashMap<String, RequestConstraint> reqConstraints = 
	        	 new HashMap<String, RequestConstraint>( waveSatRscData.getMetadataMap() );

	        RequestConstraint timeConstraint = new RequestConstraint();
	        timeConstraint.setConstraintType( ConstraintType.IN );

            while( !autoUpdateList.isEmpty() ) {
            	int i=0, maxQueryTimes = 500;

            	timeConstraint.setConstraintValue(
            			autoUpdateList.get(0).dataTime.toString() );
            	autoUpdateList.remove(0);
            	
            	while( !autoUpdateList.isEmpty() && i < maxQueryTimes ) {
            		timeConstraint.addToConstraintValueList(
            				autoUpdateList.get(0).dataTime.toString() );
                	autoUpdateList.remove(0);            		
            		i++;       
            	}
            	reqConstraints.put( "dataTime", timeConstraint );

            	try {
            		queryData( reqConstraints );
            	} catch (VizException e) {
            		System.out.println("Error requesting auto update waveSat data");
            	}
            }
		}
		
		// add the WaveSatRscDataObj to the list for this frame.
    	public boolean updateFrameData( IRscDataObject rscDataObj ) {
    		if( !(rscDataObj instanceof WaveSatRscDataObj) ) {
    			System.out.println("WaveSat:updateFrameData expecting WaveSatRscDataObj instead of: "+ 
    					rscDataObj.getClass().getName() );
    			return false;
    		}
    		
    		WaveSatRscDataObj waveSatRscDataObj = (WaveSatRscDataObj)rscDataObj;
    		
    		// if created from the AlertParser, the waveHeight is not set yet.
    		// This is a placeholder used to query the data. Remove this when the
    		// WaveSatAlertParser is able to create a valid Data object. 
    		if( waveSatRscDataObj.createdFromAutoUpdate ) {
    			autoUpdateList.add( waveSatRscDataObj );
    		}
    		else {
    			waveSatDataMap.put( waveSatRscDataObj.dataTime.getRefTime().getTime(),
    								 waveSatRscDataObj );
    		}
    		
			needsUpdate = true;

    		return true;
    	}

    	public void dispose() {
    		waveSatDataMap.clear();
    		
    		if( timeDisplayWireframe != null ) {
    			timeDisplayWireframe.dispose();	
    		}
    		
    		for( IWireframeShape shape : waveHeightWireframes ) {
    			shape.dispose();
    		}
    		waveHeightWireframes.clear();
    	}

    }

    
    public WaveSatResource(WaveSatResourceData ncresourceData,
			LoadProperties loadProperties) {
    	super(ncresourceData, loadProperties);
    	this.waveSatRscData = (WaveSatResourceData) resourceData;    	
    	
    	ncresourceData.setAlertParser( new WaveSatAlertParser() );
    }
    

	protected IRscDataObject[] processRecord( Object pdo ) {
		// if called from the AutoUpdate, this is the object created
		// by the WaveSatAlertParser which is a WaveSatRscDataObj.
		// Currently this object doesn't have all the data set (the sgwh, windSpeed...)
		// since the info is not in the URI and a URI-base query returns a SgwhRecord
		// without the hdf5 parameters set.)
		//    So for this reason we will ch
		if( pdo instanceof WaveSatRscDataObj ) {
			WaveSatRscDataObj waveSatData = (WaveSatRscDataObj)pdo;
			if( waveSatData.createdFromAutoUpdate == false ) {
				System.out.println( "WaveSat processRecord() : sanity check #1");
				return null;
			}
			return new WaveSatRscDataObj[]{ waveSatData };
		}
		if( !(pdo instanceof PointDataView) ) {
			System.out.println( "WaveSat processRecord() : Expecting PointDataView object instead of: "+					
					pdo.getClass().getName() );
			return new WaveSatRscDataObj[]{ };
		}
		
		WaveSatRscDataObj waveSatData = new WaveSatRscDataObj();
		PointDataView pdv = (PointDataView) pdo;
		
		if( pdv.getType(LAT_PARM) != Type.FLOAT ||
			pdv.getType(LON_PARM) != Type.FLOAT ||
			pdv.getType(REFTIME_PARAM) != Type.LONG || 
			pdv.getType(SAT_ID_PARAM) != Type.LONG || 
			pdv.getType(WAVE_HEIGHT_PARAM) != Type.FLOAT ||
			pdv.getType(WIND_SPEED_PARM) != Type.FLOAT ||
			pdv.getType(FOSTSGWH_PARM) != Type.LONG ) {
			
			System.out.println("SGWH parameter not found.");
//			throw new VizException("SGWH parameter not found.");
			return new WaveSatRscDataObj[]{ };
		}
				
		waveSatData.dataTime = new DataTime( new Date( (Long)pdv.getNumber( REFTIME_PARAM ) ) );
		
		waveSatData.satelliteId = Long.toString( (Long)pdv.getNumber( SAT_ID_PARAM ).longValue() );
		waveSatData.lat = pdv.getNumber( LAT_PARM ).doubleValue();
		waveSatData.lon = pdv.getNumber( LON_PARM ).doubleValue();
		waveSatData.waveHeight = pdv.getNumber( WAVE_HEIGHT_PARAM ).doubleValue();
		waveSatData.windSpeed = pdv.getNumber( WIND_SPEED_PARM ).doubleValue();
				
		return new WaveSatRscDataObj[]{ waveSatData };
	}
	    
    // 
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	
//    	font = grphTarget.initializeFont( waveSatRscData.getFontName(), 
//    			waveSatRscData.getFontSize(), null);    	
//    	font = grphTarget.getDef aultFont().deriveWithSize( waveSatRscData.getFontSize() );    	
		needsUpdate = true;

        cbarRscPair  = ResourcePair.constructSystemResourcePair( 
		           new ColorBarResourceData( waveSatRscData.getColorBar() ) );

        getDescriptor().getResourceList().add(  cbarRscPair );
        getDescriptor().getResourceList().instantiateResources( getDescriptor(), true );

        cbarResource = (ColorBarResource) cbarRscPair.getResource();

        // Most resources call queryRecords() here to populate the frames
        // but since this query may take a while whe postpone until a frame is 
        // painted and then populate just that frame. 
//    	queryRecords();    	
    }
    
    // use a PointDataContainer instead of requesting the entire record which has
    // lots of data we don't need.
//	public void queryRecords() throws VizException {
//        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
//        for( Entry<String, RequestConstraint> constraint : 
//        			waveSatRscData.getMetadataMap().entrySet() ) {
//        
//        	map.put( constraint.getKey(), constraint.getValue());
//        }
////        RequestConstraint rc = new RequestConstraint();
////        rc.setConstraintType( ConstraintType.IN );
////        String[] str = new String[stationQuery.size()];
////        
////        for (int z = 0; z < str.length; z++) {
////            str[z] = "" + stationQuery.get(z).id;
////        }
//        List<String> params = new ArrayList<String>();
//        params.add( LAT_PARM );
//        params.add( LON_PARM );
//        params.add( REFTIME_PARAM );
//        params.add( WAVE_HEIGHT_PARAM );
//        params.add( WIND_SPEED_PARM );
//        params.add( FOSTSGWH_PARM );
//        
//        PointDataContainer pdc = null;
//            
////        map.put("id", rc);
//            
//        try {
//        	// TODO : do we need to use the DataCubeContainer? 
//        	///PointDataContainer tempPdc = null;
////        		DataCubeContainer
////        			.getPointData( waveSatRscData.getPluginName(), 
////        				params.toArray( new String[params.size()] ),
////        				null, map); //levelKey
//
//        				// Datacube didn't have proper plugin; going directly
//        				// to the data store
//        	pdc = PointDataRequest.requestPointDataAllLevels(
//        						null, waveSatRscData.getPluginName(), 
//        						params.toArray( new String[params.size()] ),
//        						null, map );
//
//        } catch (VizException e1) {
//        	System.out.println("Error querying for sgwh point data");
//        	throw e1;
//        }
//            
//        if( pdc != null ) {
//        	pdc.setCurrentSz(pdc.getAllocatedSz());
//        }
//                                    	                    
//        for( int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++ ) {
//            PointDataView pdv = pdc.readRandom(uriCounter);
//            if( pdv == null ) { // ???
//            	continue;
//            }
//            
//			for( IRscDataObject dataObject : processRecord( pdv ) )	{	
//				newRscDataObjsQueue.add(dataObject);
//			}
//		}
//	}


    public void paintFrame( AbstractFrameData frameData, IGraphicsTarget grphTarget, PaintProperties paintProps) throws VizException {
    	FrameData currFrameData=(FrameData) frameData;

    	if( !currFrameData.isPopulated() ) {
    		currFrameData.populateFrame();
    	}
    	else if( !currFrameData.autoUpdateList.isEmpty() ) {
    		currFrameData.updateFrameDataFromAutoUpdateList();
    	}
    	
    	IExtent extent = paintProps.getView().getExtent();

    	ColorBar colorBar = waveSatRscData.getColorBar();
    	colorBar.setNumDecimals( 1 );
    	    	
    	// if we don't need to re-create the wireframes
    	// the isZooming check will limits the flickering effect by only recomputing 
    	// the values when the zoom is done.
        if( !needsUpdate &&
            !currFrameData.waveHeightWireframes.isEmpty() && 
            currFrameData.prevZoomLevel == paintProps.getZoomLevel() || 
            paintProps.isZooming() ) {        	 
        	// if we put this check in the wave heights will recompute on a pan which causes a flicker-like effect  
        	// that I don't think the user will like.
//             extent.equals( currFrameData.lastPixelExtent ) ) {

        	grphTarget.drawWireframeShape( currFrameData.timeDisplayWireframe, 
        			waveSatRscData.getTimeDisplayColor(), 1.0f, LineStyle.SOLID, font );
        	
        	for( int s= currFrameData.waveHeightWireframes.size()-1 ; s>=0 ; s-- ) {        		
        		grphTarget.drawWireframeShape( currFrameData.waveHeightWireframes.get(s),
        							waveSatRscData.getColorBar().getRGB(s), 
        							1.0f, LineStyle.SOLID, font );
        	}
        	
        } else {        	
            needsUpdate = false;
            
            currFrameData.prevZoomLevel = paintProps.getZoomLevel();
            
            if( font == null ) {
        		font = grphTarget.initializeFont( waveSatRscData.getFontName(), 
	    			waveSatRscData.getFontSize().floatValue( ), null);
        	}

            currFrameData.lastPixelExtent = (PixelExtent) extent.clone();

            // create or reuse the wireframe shapes for the times 
            // and for each color's waveHeight values.
            //
            if( currFrameData.timeDisplayWireframe == null ) {
            	currFrameData.timeDisplayWireframe = 
            		grphTarget.createWireframeShape( true, this.descriptor );
            }
            else {
            	currFrameData.timeDisplayWireframe.reset();
            }

            for( int i=0 ; i<waveSatRscData.getColorBar().getNumIntervals() ; i++ ) {            	
            	if( i >= currFrameData.waveHeightWireframes.size() ) {
            		currFrameData.waveHeightWireframes.add( 
            				grphTarget.createWireframeShape( true, this.descriptor ) );        			
            	}
            	else {	
            		currFrameData.waveHeightWireframes.get(i).reset();        			
        		}
        	}
                        
            // if intervals have been removed from the colorBar, delete wireframes
            //
            while( currFrameData.waveHeightWireframes.size() > 
            	   waveSatRscData.getColorBar().getNumIntervals() ) {
            	IWireframeShape wf = currFrameData.waveHeightWireframes.get( 
            			currFrameData.waveHeightWireframes.size()-1 );
            	wf.dispose();
            	currFrameData.waveHeightWireframes.remove( wf ); 
            }
                    	        	
            int cnt=0;
            Rectangle2D bnds = null;
            PixelExtent prevPixExtent=null;
            long prevDisplayedTime = 0; 
            long prevTime = 0;
            //
            for( WaveSatRscDataObj waveSatData : currFrameData.waveSatDataMap.values() ) {
            	
            	cnt++;
            	
            	double[] latLon = { waveSatData.lon, waveSatData.lat }; 
            	double[] waveHghtPixLoc = this.descriptor.worldToPixel( latLon );
            	
            	// get the pixel coord from the lat/lon and check if it is in the paint extents
            	if( waveHghtPixLoc != null /*&&
            			currFrameData.lastPixelExtent.contains( pixLoc[0], pixLoc[1] )*/ ) {
            		
            		Double waveHeight = waveSatData.waveHeight;

            		// round to the nearest tenth
            		waveHeight = (double)Math.round( waveHeight*10 ) / 10; 
            		String waveHeightStr = waveHeight.toString();

//					if displaying symbols then get the symbolName ... NOT Implemented             		
//            		if( waveSatRscData.getDisplayValues() ) {
//            			waveHeightStr = "X";
//            		}
            		
                    double scale = extent.getWidth() / paintProps.getCanvasBounds().width;

                    bnds = grphTarget.getStringBounds( font, waveHeightStr );
                    
            		// We have to have a line or the labels won't draw but this will get drawn 
            		// behind the label and so will not show up. 
            		double[][] line = new double[2][2];
        			line[0][0] = waveHghtPixLoc[0] - .1;
        			line[0][1] = waveHghtPixLoc[1];
        			line[1][0] = waveHghtPixLoc[0] + .1;
        			line[1][1] = waveHghtPixLoc[1];

        			// alignment set in addLabel is CENTER for x and BOTTOM for y and since we  
        			// currently can't change this, we have to adjust for the latitude.
        			waveHghtPixLoc[1] += scale* bnds.getHeight()/2;

                    double minX = waveHghtPixLoc[0] - scale* bnds.getWidth()/2;
            		double maxX = waveHghtPixLoc[0] + scale* bnds.getWidth()/2;
            		double minY = waveHghtPixLoc[1] - scale* bnds.getHeight()/2;
            		double maxY = waveHghtPixLoc[1] + scale* bnds.getHeight()/2;

            		PixelExtent pixExtent = new PixelExtent( minX, maxX, minY, maxY ); 

            		if( !waveHeight.equals( MissingValue ) ) { 
            			if( prevPixExtent == null ||
            					!prevPixExtent.intersect( pixExtent ) ) {

            				for( int i=0 ; i<colorBar.getNumIntervals() ; i++ ) {
            					if( colorBar.isValueInInterval( 
            							i, waveHeight.floatValue(), WaveHeightUnits ) ) {
            						IWireframeShape wf = currFrameData.waveHeightWireframes.get(i);
            						wf.addLineSegment(line);

            						wf.addLabel( waveHeightStr, waveHghtPixLoc );
                    				prevPixExtent = pixExtent;
            						break;
            					}
            				}
            			}
            		}
            		
            		DataTime waveTime = waveSatData.getDataTime();
            		
            		// 
                	if(// prevDisplayedTime == null ||
                		waveTime.getRefTime().getTime() - prevDisplayedTime >
                		  waveSatRscData.getTimeDisplayInterval()*1000*60 ) {
                		// create a time for this

                        SimpleDateFormat dateFormat = new SimpleDateFormat("MMdd/HHmm");
                        dateFormat.setTimeZone( TimeZone.getTimeZone("GMT") );
                        Calendar cal = waveTime.getValidTime();
                        String timeStr = dateFormat.format(cal.getTime() );
                           
                        Rectangle2D timeBnds = grphTarget.getStringBounds( font, timeStr );
                		
                        double[][] timeLine = new double[2][2];
                		timeLine[0][0] = pixExtent.getMaxX()+5*scale;
            			timeLine[0][1] = waveHghtPixLoc[1] - timeBnds.getHeight()/2*scale;
            			timeLine[1][0] = pixExtent.getMaxX() + 40*scale;
            			timeLine[1][1] = waveHghtPixLoc[1] - timeBnds.getHeight()/2*scale;

                		currFrameData.timeDisplayWireframe.addLineSegment( timeLine );

                		double[] timeLoc = new double[2];
                        timeLoc[0] = pixExtent.getMaxX() + scale*(timeBnds.getWidth()+40)/2; 
                        timeLoc[1] = waveHghtPixLoc[1]; // + scale*timeBnds.getHeight()/2;

                        currFrameData.timeDisplayWireframe.addLabel(
                				timeStr.toUpperCase().trim(), timeLoc );
                		
                		
                    	prevDisplayedTime = waveTime.getRefTime().getTime();                		
                	}
            	}
            }
            
            // draw the time first so it can't obscure any of the data
            //
            currFrameData.timeDisplayWireframe.compile();
            grphTarget.drawWireframeShape(
            		currFrameData.timeDisplayWireframe,
            		    waveSatRscData.getTimeDisplayColor(), 1.0f, LineStyle.SOLID, font );
            
        	for( int s= waveSatRscData.getColorBar().getNumIntervals()-1 ; s>=0 ; s-- ) {
            	IWireframeShape wf = currFrameData.waveHeightWireframes.get(s);
            	wf.compile();
            	grphTarget.drawWireframeShape( wf, 
            			waveSatRscData.getColorBar().getRGB(s), 
            			1.0f, LineStyle.SOLID, font );
        	}
    	}
    }                        

    public void disposeInternal() {
    	super.disposeInternal();
    	if( font != null ) {
    		font.dispose();
    		font = null;
    	}
    	if( cbarRscPair != null ) {
    		getDescriptor().getResourceList().remove( cbarRscPair );
    	}
    }


	public void resourceAttrsModified() {
		needsUpdate = true;
		// update the colorbarPainter with a possibly new colorbar
		cbarResource.setColorBar( waveSatRscData.getColorBar() );

		if( font != null ) {
			font.dispose();
			font = null;
		}
	}

	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return new FrameData(frameTime,timeInt);
	}	
	
	@Override
    public void propertiesChanged(ResourceProperties updatedProps) {
    	
    	if( cbarRscPair != null ) {
    		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
    	}
    }

	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.waveSatDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
