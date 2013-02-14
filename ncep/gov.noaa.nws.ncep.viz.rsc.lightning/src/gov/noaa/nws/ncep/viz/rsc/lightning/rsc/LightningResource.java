package gov.noaa.nws.ncep.viz.rsc.lightning.rsc;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;


/**
 * LigntningResource - Display Lightning data.
 *  *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  04/10/2010    #257     Greg Hull    Initial creation.
 *  04/10/2010    #259     Greg Hull    draw the colorBar
 *  10/27/2010    #307     Greg Hull    time match on strike data
 *  11/18/2010    #307     Greg Hull    newRscDataObjsList -> newRscDataObjsQueue
 *  04/22/2011    #439     Greg Hull    only query needed times, rm System.out, 
 *                                      show strike count in legend
 *  02/16/2012    #555     S. Gurung    Added call to setAllFramesAsPopulated() in queryRecords()
 *  05/23/12      785      Q. Zhou      Added getName for legend.
 *  12/19/2012    #960     Greg Hull   override propertiesChanged() to update colorBar.
 *  
 * </pre>
 * 
 * @author ghull 
 * @version 1.0
 */
public class LightningResource extends AbstractNatlCntrsResource<LightningResourceData, MapDescriptor> 
	implements INatlCntrsResource {
    
    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(LightningResource.class);

	private LightningResourceData ltngRscData;
    private boolean needsUpdate;
    private int displayedStrikeCount=0;
    private TimeRange queryTimes = null; // if null then all times are queried.
    
    // these objects are created when added to the strikeMap and then 
    // are reused/shared within the FrameData
    private class LtngStrikeDataObj implements IRscDataObject {
    	int intensity;
    	float lat, lon;
    	long  strikeTime;
    	//int strikeCount;
    	
    	public boolean isPositive() {
    		return intensity > 0;
    	}
    	
		@Override
		public DataTime getDataTime() {		
			return new DataTime( new Date( strikeTime ) );
		}
    }

    protected ColorBarResource cbarResource;
    protected ResourcePair     cbarRscPair;

    // 
    private class FrameData extends AbstractFrameData {
    	
        private List<LtngStrikeDataObj> ltngStrikes;        
        private PixelExtent lastPixelExtent;

        // There should be one wireFrame for each colorBarInterval
        //
        private List<IWireframeShape> ltngStrikeShapesList;

		public FrameData( DataTime frameTime, int timeInt ) {
			super( frameTime, timeInt );
			ltngStrikes = new ArrayList<LtngStrikeDataObj>();
			ltngStrikeShapesList = new ArrayList<IWireframeShape>();
    	}

		// all the records have been preprocessed and put into the 
		// 
    	public boolean updateFrameData( IRscDataObject rscDataObj ) {
    		if( !(rscDataObj instanceof LtngStrikeDataObj) ) {
    			System.out.println("LTNG:updateFrameData expecting LtngStrikeDataObj and not: "+
    					rscDataObj.getClass().getName() );
    			return false;
    		}
    		
//        	PluginDataObject pdo = ((DfltRecordRscDataObj)rscDataObj).getPDO();
//    		BinLightningRecord ltngRec = (BinLightningRecord) pdo;
//    		DataTime ltngTime = ltngRec.getDataTime();
//    		
//    		// look up the uri in the map created during the preprocessing pass.
//    		String uri = ltngRec.getDataURI();
//    		List<LtngStrikeDataObj> strikes = strikeMap.get(ltngRec.getDataURI()); 
//
//    		// TODO : Do we need to check if this uri/pdo has already been processed
//    		// and not put it in the list??
//    		for( LtngStrikeDataObj strike : strikes ) {    			
    			ltngStrikes.add( (LtngStrikeDataObj)rscDataObj );
    			needsUpdate = true;
//    		}
    		return true;
    	}
    	
    	// 
    	public void dispose() {
    		ltngStrikes.clear();
    		
    		for( IWireframeShape shape : ltngStrikeShapesList ) {
    			shape.dispose();
    		}
    		ltngStrikeShapesList.clear();
    	}
    }
    
    public LightningResource(LightningResourceData ncresourceData,
			LoadProperties loadProperties) {
    	super(ncresourceData, loadProperties);
    	this.ltngRscData = (LightningResourceData) resourceData;
    	
    	//ltngRscData.setAlertParser( new NcLtngAlertParser() );
    }

    // to show in the legend
    public Integer getNumDisplayedStrikes( ) {
    	return displayedStrikeCount;
//    	AbstractFrameData frmData = getCurrentFrame();
//    //	if( ((FrameData)frmData).ltngStrikes.size())
//    	// should we return the total number of strikes in the frame or 
//    	// only those displayed in the current extents?
//    	//
//    	return ((FrameData)frmData).ltngStrikes.size();
    }
    
    // 
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	  
    	// create a system resource for the colorBar and add it to the resource list.
    	//
        cbarRscPair  = ResourcePair.constructSystemResourcePair( 
        		           new ColorBarResourceData( ltngRscData.getColorBar() ) );
        
    	getDescriptor().getResourceList().add(  cbarRscPair );
        getDescriptor().getResourceList().instantiateResources( getDescriptor(), true );

        cbarResource = (ColorBarResource) cbarRscPair.getResource();
        
    	queryRecords();
    }

    // query the lightning records and then retrieve the actual lightning strikes from hdf5 to 
    // use as the IRscDataObjs. This could be done using the base queryRecords and the a processRecords()
    // method to convert the BinLightningRecord to a list of LtngStrikeDataObjs except that we would not
    // be able to cache the queries to DataStoreFactory.getDataStore.    
    //
    @Override
	public void queryRecords() throws VizException {
    	// set up contraints to only query the needed data. 
    	// If displaying by intensity then this will just be the start time of the 
		// first frame and the end time of the last frame.
    	//     	
		ArrayList<DataTime> frameTimes  = getFrameTimes();
		long frameIntrvlMs = (long)ltngRscData.getFrameSpan()*60*1000;
		
		long firstFrameTimeMs = frameTimes.get(0).getValidTime().getTimeInMillis();
		long lastFrameTimeMs = frameTimes.get(  frameTimes.size()-1 ).getValidTime().getTimeInMillis();
			
		queryTimes = new TimeRange();
		
    	if( ltngRscData.getColorByIntensity() ) {	
    		queryTimes = new TimeRange( firstFrameTimeMs - frameIntrvlMs, lastFrameTimeMs + frameIntrvlMs );
		}
    	else {
    		// if displaying by elapsed time then we will need to get the oldest time from the 
    		// colorbar and 
    		// NOTE: we will need to save off this time since the user may change the colorbar 
    		// later to request older data.
    		//
			ColorBar cbar = ltngRscData.getColorBar();
			Float mins = cbar.getIntervalMax( cbar.getNumIntervals()-1  );

			if( mins != Float.POSITIVE_INFINITY ) {
				long cbarRangeMs = (long) (60*1000*mins);
	    		queryTimes = new TimeRange( firstFrameTimeMs - cbarRangeMs, lastFrameTimeMs + frameIntrvlMs );
			}
			// else leave as queryTimes as null and don't constrain the times.
			else {
				queryTimes = null;
			}
    	}

    	// make a copy of the metadatamap
        HashMap<String, RequestConstraint> requestConstraints = new HashMap<String, RequestConstraint>(
                ltngRscData.getMetadataMap() );

        if( queryTimes != null ) {
        	// This doesn't work because the dataTimes in the DB are time Ranges 
//        	RequestConstraint timeConstraint = new RequestConstraint();
//        	String[] constraintList = { 
//        			new DataTime( queryTimes.getStart() ).toString(), 
//        			new DataTime( queryTimes.getEnd() ).toString() };
//        	timeConstraint.setBetweenValueList(constraintList);
//        	timeConstraint.setConstraintType(RequestConstraint.ConstraintType.BETWEEN);
//        	requestConstraints.put("dataTime", timeConstraint);
        }
        
		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(requestConstraints, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap this

		String script = null;
		script = ScriptCreator.createScript(prop);

		if (script == null)
			return;

		Object[] pdoList = Connector.getInstance().connect(script, null, 90000);
		ArrayList<Object> pdoArray = new ArrayList<Object>();
		
		for( Object pdo : pdoList ) {
			if( !(pdo instanceof PluginDataObject) ) { // ???
				continue;
			}
			
			DataTime pdoDataTime = ((PluginDataObject)pdo).getDataTime();
			
			if( queryTimes == null ||
				queryTimes.contains( pdoDataTime.getValidPeriod().getStart() ) ||
				queryTimes.contains( pdoDataTime.getValidPeriod().getEnd()   ) ) {
				pdoArray.add( pdo );
			}
		}

		List<LtngStrikeDataObj> strikeList = getStrikeDataFromHDF( pdoArray.toArray() );
		
		// add the strike info to the newRscDataObjsList so that the abstract class
		// can time match it.
		if( strikeList != null ) {
			newRscDataObjsQueue.addAll( strikeList );
		}
		
		needsUpdate = true;
		
		setAllFramesAsPopulated();
    }
    
    
    private ArrayList<LtngStrikeDataObj> getStrikeDataFromHDF( Object[]  pdoList ) {

        HashMap<File, List<BinLightningRecord>> hdf5FileMap = null;         
    	hdf5FileMap = new HashMap<File, List<BinLightningRecord>>();

		// loop thru all of the records in the list and add their hdf5 to 
		// a map to let us bin up all the requests to the hdf5 files.  
		for( Object pdo : pdoList ) {
			if( pdo == null ) {
				continue;
			}
			BinLightningRecord litRec = (BinLightningRecord)pdo;
			
			File f = HDF5Util.findHDF5Location(litRec);
			
			List<BinLightningRecord> recList = hdf5FileMap.get(f);
			if( recList == null ) {
				recList = new ArrayList<BinLightningRecord>();
				hdf5FileMap.put( f, recList );
			}
			recList.add(litRec);
		}

//		ArrayList<LtngStrikeDataObj> ltngStrikesList = new ArrayList<LtngStrikeDataObj>();
		long t0 = System.currentTimeMillis();
    	long strikeCount = 0;
    	long dsTime = 0;
    	long latestStrike = 0;

		HashMap<String, List<LtngStrikeDataObj>> strikeMap = new HashMap<String, List<LtngStrikeDataObj>>(); 

    	// loop thru each of the hdf files 
    	for( File hdf5File : hdf5FileMap.keySet() ) {
    		List<BinLightningRecord> recListForFile = hdf5FileMap.get(hdf5File);
    		
    		String[] uris = new String[recListForFile.size()]; 
    		
    		for( int i=0 ; i < recListForFile.size() ; i++ ) {
    			uris[i] = recListForFile.get(i).getDataURI();
    		}

    		// get the data from the hdf5 file
    		try {
    			long tDS0 = System.currentTimeMillis();
    			IDataStore ds = DataStoreFactory.getDataStore( hdf5File );
    			// the uris are the groups for the datastore
    			IDataRecord[] hdf5Recs = ds.retrieveGroups( uris, Request.ALL );
    	        // a map from the uri to a list of strikes for that uri

    			long tDS1 = System.currentTimeMillis();
    			dsTime += (tDS1 - tDS0);
    			
    			// loop thru the URIs and find the hdf5 lat,lon and intensity records for this
    			// URI and then fill in the strikeMap with all of the strikes for this URI.
    			for( String uri : uris ) {
        			for( IDataRecord hdf5Rec : hdf5Recs ) {
        				if( hdf5Rec.getGroup().equals( uri ) ) {
    						List<LtngStrikeDataObj> strikeList = strikeMap.get(uri);

        					if( strikeList == null ) {
        						strikeList = new ArrayList<LtngStrikeDataObj>();
        						strikeMap.put(uri, strikeList);
        						
        						// init with the number of strikes for this record.
        						// NOTE: all of the records for this uri must(will) have the same 
        						// number of strikes in them.
        						for( int s=0 ; s<hdf5Rec.getSizes()[0] ; s++ ) {
        							strikeList.add( new LtngStrikeDataObj() );
        							
        							strikeCount++;
        						}
        					}
        					if( hdf5Rec.getSizes()[0] != strikeList.size() ) {
        						System.out.println("HDF5 Warning: ");
        					}
        					
        					for( int s=0 ; s<hdf5Rec.getSizes()[0] ; s++ ) {
        						LtngStrikeDataObj strikeInfo = strikeList.get(s);
        						if( hdf5Rec.getName().equals("intensity") ) {
        						    strikeInfo.intensity = ((IntegerDataRecord) hdf5Rec).getIntData()[s];
        						} else if( hdf5Rec.getName().equals("latitude") ) {
        						    strikeInfo.lat = ((FloatDataRecord) hdf5Rec).getFloatData()[s];
        						} else if( hdf5Rec.getName().equals("longitude") ) {
        							strikeInfo.lon = ((FloatDataRecord) hdf5Rec).getFloatData()[s];
        						} else if( hdf5Rec.getName().equals("obsTime") ) {
        							strikeInfo.strikeTime = ((LongDataRecord) hdf5Rec).getLongData()[s];
//        			                if( strikeInfo.obsTime > latestStrike ) {
//        			                	latestStrike = strikeInfo.obsTime;
//        			                }
//        						} else if( hdf5Rec.getName().equals("strikeCount") ) {
//        							strikeInfo.lon = ((FloatDataRecord) hdf5Rec).getFloatData()[s];      							
        						} else if( hdf5Rec.getName().equals("msgType") ) {
        							// ignore
        						}
    						}    				
        				}
        			}    				
    			}
    			
    		} catch (StorageException e) {
    		    statusHandler.handle(Priority.PROBLEM, 
    					"Storage error retrieving lightning data", e);
    		} catch (FileNotFoundException e) {
    		    statusHandler.handle(Priority.PROBLEM, 
    					"Unable to open lightning file", e);
    		}
    	}

    	long t1 = System.currentTimeMillis();
//		DataTime dt = new DataTime(new Date( latestStrike ));
//		System.out.println("latest strike is at " + dt.toString() );
		
    	System.out.println("Decoded: " + strikeCount + " strikes in "
    			+ (t1 - t0) + " (" + dsTime + ")");    	
		
		// return a list of all the strikes for all the uris.
    	ArrayList<LtngStrikeDataObj> strikeList = new ArrayList<LtngStrikeDataObj>();
		for( String uri : strikeMap.keySet() ) {
			strikeList.addAll( strikeMap.get( uri ) );
		}
		
		return strikeList;
	}

    // in the case when the objects are from the initial query they have already been
    // converted into the LtngRscDataObjs so we just return the input object, 
    // otherwise the objects are from an auto update and we will have to retrieve 
    // the strike info from hdf5.
    // 
    protected IRscDataObject[] processRecord( Object pdo ) {
    	if( pdo instanceof LtngStrikeDataObj ) {
    		return new LtngStrikeDataObj[]{ (LtngStrikeDataObj)pdo };	
    	}
    	else if( pdo instanceof BinLightningRecord ) {
    		return getStrikeDataFromHDF(new Object[]{ pdo } ).toArray( new LtngStrikeDataObj[0] );
    	}
    	else {
    		System.out.println("LTNG:processRecord expecting LtngStrikeDataObj or "+
    				"BinLightningRecord instead of: "+ pdo.getClass().getName() );
    		return new IRscDataObject[0];
    	}		
    }

    
    public void paintFrame( AbstractFrameData frameData, IGraphicsTarget grphTarget, PaintProperties paintProps) throws VizException {
    	
    	FrameData currFrameData=(FrameData) frameData;
    	
    	IExtent extent = paintProps.getView().getExtent();
        int posCount=0, negCount=0, noIntensityCount=0;

        double zoomLevel = paintProps.getZoomLevel();

        // if we don't need to re-create the wireframes
        if( !needsUpdate &&
            !currFrameData.ltngStrikeShapesList.isEmpty() && 
             extent.equals( currFrameData.lastPixelExtent ) ) {

        	// draw the wireframes in reverse order so that the new strikes are displayed last.
        	for( int s= currFrameData.ltngStrikeShapesList.size()-1 ; s>=0 ; s-- ) {        		
        		grphTarget.drawWireframeShape( currFrameData.ltngStrikeShapesList.get(s),
        							ltngRscData.getColorBar().getRGB(s), 
        							ltngRscData.getLineWidth() );
        	}
        	
        } else {
        	
            needsUpdate = false;
            // determine the length of the lines based on the symbol size. 3 is 'normal'
            double xLength = (extent.getMaxX() - extent.getMinX()) / 200.0;
            double posSymSize = (xLength * ltngRscData.getPositiveSymbolSize())/3;
            double negSymSize = (xLength * ltngRscData.getNegativeSymbolSize())/3;

            
            // create or reuse the wireframe shapes
            for( int i=0 ; i<ltngRscData.getColorBar().getNumIntervals() ; i++ ) {            	
            	if( i >= currFrameData.ltngStrikeShapesList.size() ) {
            		currFrameData.ltngStrikeShapesList.add( 
            				grphTarget.createWireframeShape( true, this.descriptor ) );        			
            	}
            	else {	
            		currFrameData.ltngStrikeShapesList.get(i).reset();        			
        		}
        	}
            
            displayedStrikeCount = 0;
            
            // if intervals have been removed from the colorBar, delete wireframes
            //
            while( currFrameData.ltngStrikeShapesList.size() > 
            	   ltngRscData.getColorBar().getNumIntervals() ) {
            	IWireframeShape wf = currFrameData.ltngStrikeShapesList.get( 
            			currFrameData.ltngStrikeShapesList.size()-1 );
            	wf.dispose();
            	currFrameData.ltngStrikeShapesList.remove( wf ); 
            }
            
//            int maxInt = 0, minInt=0;
            
            // loop thru all the strikes in this frame and compile the wireframes for the
            // strike histories. 
            for( LtngStrikeDataObj strike : currFrameData.ltngStrikes ) {
        		
//            	if( strike.intensity > maxInt ) maxInt = strike.intensity;
//            	if( strike.intensity < minInt ) minInt = strike.intensity;
//            	System.out.println("Intensity = "+ Integer.toString( strike.intensity ) );
            	
            	if( (!ltngRscData.getEnablePositiveStrikes() && strike.isPositive() ) ||
            		(!ltngRscData.getEnableNegativeStrikes() && !strike.isPositive() ) ) {
            		continue;
            	}
            	
                double[] worldLoc = this.descriptor.worldToPixel( 
                		                new double[] { strike.lon, strike.lat } );
                 
                if( extent.contains( worldLoc ) ) {

                	// determine the interval that this strike belongs in based on the
                	// colorBarInterval and then get the shape and the color  
                	//   

                	// times are computed in seconds. The colorbar intervals are in mins.  
                	// (the number of mins in the past that a strike occurred)
                	long strikeTime = strike.strikeTime / 1000;
                	                	
                	// the minvalue of the first interval really should be 0 to indicate 
                	// a strike at the current time.
                	long endTime = currFrameData.getFrameTime().getRefTime().getTime()/1000 -
                					(long)(ltngRscData.getColorBar().getIntervalMin(0) * 60);

                    	
                    for( int i=0 ; i<ltngRscData.getColorBar().getNumIntervals() ; i++ ) {
                    
                    	long beginIntervalTime = 
                    		endTime - (long)(ltngRscData.getColorBar().getIntervalMax(i)*60);
                    	
                    	if( ltngRscData.getColorBar().getIntervalMax(i) == Float.POSITIVE_INFINITY ) {
                    		beginIntervalTime = 0;
                    	}
                    	
                    	double minIntensity = ltngRscData.getColorBar().getIntervalMin(i);
                        double maxIntensity = ltngRscData.getColorBar().getIntervalMax(i);
                        
                    	// determine if this strike belongs in the shape for this colorInterval.
                    	// if we are coloring by history then we will check if the strike is within
                    	// the given time interval and if we are coloring by intensity then we will
                    	// check if the intensity is in this interval.
                    	if( (!ltngRscData.getColorByIntensity() &&
                    		 strikeTime <= endTime && strikeTime > beginIntervalTime ) 
                    		      ||
                    		(ltngRscData.getColorByIntensity() && 
                    		 strike.intensity > minIntensity && strike.intensity <= maxIntensity ) ) {
                    		
                    		IWireframeShape strikeShape = currFrameData.ltngStrikeShapesList.get(i);

                    		// only for debug since this kill performance
//                        	System.out.println("Strike time is "+ strike.getDataTime().toString() );                        	
//                            System.out.println("is in Interval " + Integer.toString(i) + " : " +
//                            		new DataTime( new Date(beginIntervalTime*1000) ).toString() +" to " +
//                            		new DataTime( new Date(endTime*1000) ) );

                    		// positive : draw a '+'
                    		if( strike.isPositive() ) {
                    			double[][] plusLine = new double[2][2];
                    			plusLine[0][0] = worldLoc[0] - posSymSize;
                    			plusLine[0][1] = worldLoc[1];
                    			plusLine[1][0] = worldLoc[0] + posSymSize;
                    			plusLine[1][1] = worldLoc[1];

                    			strikeShape.addLineSegment( plusLine );

                    			plusLine[0][0] = worldLoc[0];
                    			plusLine[0][1] = worldLoc[1] - posSymSize;
                    			plusLine[1][0] = worldLoc[0];
                    			plusLine[1][1] = worldLoc[1] + posSymSize;

                    			strikeShape.addLineSegment( plusLine );

                    			posCount++;
                    		}           
                    		else { // if negative ( 0 is considered negative
                    			double[][] minusLine = new double[2][2];
                    			minusLine[0][0] = worldLoc[0] - negSymSize;
                    			minusLine[0][1] = worldLoc[1];
                    			minusLine[1][0] = worldLoc[0] + negSymSize;
                    			minusLine[1][1] = worldLoc[1];

                    			strikeShape.addLineSegment( minusLine );

                    			negCount++;
                    		}
//                    		else { // TODO: what to do with a zero intensity strike??
//                    			double[][] minusLine = new double[2][2];
//                    			minusLine[0][0] = worldLoc[0] - strikeLength/2;
//                    			minusLine[0][1] = worldLoc[1];
//                    			minusLine[1][0] = worldLoc[0] + strikeLength/2;
//                    			minusLine[1][1] = worldLoc[1];
//
//                    			strikeShape.addLineSegment( minusLine );
//                    			noIntensityCount++;                    			
//                    		}

                    		endTime = (long)(endTime-ltngRscData.getColorBar().getIntervalMin(i)*60);
                    		break;
                    	} // end if this strike in this colorInterval
                    } // end loop thru color intervals
                } // end if pixel in extent
            } // end loop thru strikes

            displayedStrikeCount = posCount+negCount;
            
//            System.out.println("Max Intensity = "+ Integer.toString( maxInt ) );
//            System.out.println("Min Intensity = "+ Integer.toString( minInt ) );

            
        	for( int s= ltngRscData.getColorBar().getNumIntervals()-1 ; s>=0 ; s-- ) {
            	IWireframeShape strikeShape = currFrameData.ltngStrikeShapesList.get(s);
            	strikeShape.compile();
            	grphTarget.drawWireframeShape( strikeShape, 
            								   ltngRscData.getColorBar().getRGB(s), 
            			                       ltngRscData.getLineWidth() );
        	}

            currFrameData.lastPixelExtent = (PixelExtent) extent.clone();
        }

// code from d2d's lightningResource to draw the strike counts in the upper left corner.
//
//        if( ltngRscData.getEnablePositiveStrikes() ) {
//        	grphTarget.drawString(null, posCount + " + Strikes", extent.getMinX()
//                    + (1500 * zoomLevel), extent.getMinY() + (650 * zoomLevel),
//                    0.0, TextStyle.BLANKED, ltngRscData.getColor(),
//                    HorizontalAlignment.RIGHT, null);
//        }
//        
//        if( ltngRscData.getEnableNegativeStrikes() ) {
//        	grphTarget.drawString(null, negCount + " - Strikes", extent.getMinX()
//                    + (1500 * zoomLevel),
//                    extent.getMinY() + (1000 * zoomLevel), 0.0,
//                    TextStyle.BLANKED, ltngRscData.getColor(), HorizontalAlignment.RIGHT, null);
//        }
//
//        if( ltngRscData.getEnableNegativeStrikes() ) {
//        	grphTarget.drawString(null, noIntensityCount + " 0 Strikes", extent.getMinX()
//                    + (1500 * zoomLevel),
//                    extent.getMinY() + (1000 * zoomLevel), 0.0,
//                    TextStyle.BLANKED, ltngRscData.getColor(), HorizontalAlignment.RIGHT, null);
//        }
    }                        

    public void disposeInternal() {
    	super.disposeInternal();

        getDescriptor().getResourceList().remove( cbarRscPair );
    }

	public void resourceAttrsModified() {
		needsUpdate = true;

		// update the colorbarPainter with a possibly new colorbar
		cbarResource.setColorBar( ltngRscData.getColorBar() );

		// check if we need to requery the data
		// 
		
		// either we already querying all data or the colorbar doesn't affect the query times 
		if( queryTimes == null || 
			ltngRscData.getColorByIntensity() ) {	
			return;
		}
		
		ColorBar cbar = ltngRscData.getColorBar();
		Float mins = cbar.getIntervalMax( cbar.getNumIntervals()-1 );

		long cbarRangeMs = (long) (60*1000*mins);
		long firstFrameTimeMs = getFrameTimes().get(0).getValidTime().getTimeInMillis();
		Date startTime = new Date(firstFrameTimeMs - cbarRangeMs);

		// if the color bar is now infinite or if 
		if( mins == Float.POSITIVE_INFINITY  ||
				queryTimes.getStart().compareTo( startTime ) > 0 ) {

			queryTimes.setStart( startTime );

			clearFrames();

			initialized = false;
			try {
				queryRecords();
			} catch (VizException e) {
			}
			// end time should not have changed
		}
	}

	@Override
    public void propertiesChanged(ResourceProperties updatedProps) {
    	
    	if( cbarRscPair != null ) {
    		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
    	}
    }

	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return new FrameData(frameTime,timeInt);
	}	
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.ltngStrikeShapesList.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
