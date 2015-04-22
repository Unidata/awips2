package gov.noaa.nws.ncep.viz.rsc.wcp.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpLatlons;
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpRecord;
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpSevrln;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;


/**
 * WCPResource - Display WCP data.
 * 
 *  This code has been developed by the NOAA/NCEP/NCO/SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  04/30/2009    96        M. Li    	Initial creation.
 *  05/13/2009    96    	M. Li		Add WCP editting
 *  06/02/2009   115        Greg Hull	Integrate with AbstractNatlCntrsResouce and rework 
 *                                      attribute getting/setting to work with ResourceAttrSets.
 *  09/14/2009              Greg Hull   Migrate to to11  / AbstractNatlCntrsresourc
 *  12/14/2009              B. Hebbard  Migrate to11d3->d6                                    
 *  10/01/10      #307      Greg Hull   implement processRecords and change to 
 *                                      process WCPData as the IRscDataObj
 *  10/05/11                X. Guo      Make changes to display WCP
 *  05/23/2012     785      Q. Zhou    Added getName for legend.                                    
 * </pre>
 * 
 * @author mli 
 * @version 1.0
 */
public class WCPResource extends AbstractNatlCntrsResource<WCPResourceData, NCMapDescriptor>
 				implements INatlCntrsResource {
	private  final long SECONDS_IN_90MIN = 5400;

	private WCPResourceData wcpResourceData;

//    private String sourceName;
//    private RGB color; // Resource legend color
    private IFont font;
    
    private class WcpRscDataObj implements IRscDataObject {
    	String	 watchType;
    	DataTime issueTime;
    	DataTime eventTime;
    	DataTime endTime;
    	String 	 watchNumber;
    	int		 numPnts;
    	float[]	 lat;
    	float[]  lon;
    	
		@Override
		public DataTime getDataTime() {
			return eventTime;
		}
    }

    
    protected class FrameData extends AbstractFrameData {

        HashMap<String, WcpRscDataObj> wcpDataMap;

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
        	wcpDataMap = new HashMap<String,WcpRscDataObj>();
    	}

		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof WcpRscDataObj ) ) {
				System.out.println("WcpResource.updateFrameData: expecting objects "+
								   " of type WcpRscDataObj???");
				return false;
			}

		    WcpRscDataObj wcpRscData = (WcpRscDataObj)rscDataObj;
		    WcpRscDataObj existingWcpData = wcpDataMap.get( wcpRscData.watchNumber );

		    // if watchNumber is not in the list or if the ref time is newer then add the data to the list
		    if( existingWcpData == null ) {
		    	wcpDataMap.put( wcpRscData.watchNumber, wcpRscData );
		    }
		    else if	 ( wcpRscData.issueTime.greaterThan( existingWcpData.issueTime ) ) {
		    	wcpDataMap.remove(wcpRscData.watchNumber);
		    	wcpDataMap.put( wcpRscData.watchNumber, wcpRscData );
		    }

		    return true;
		}
    }
	
	@Override
	protected AbstractFrameData createNewFrame(DataTime frameTime,
			int timeInterval) {
		return new FrameData( frameTime, timeInterval );
	}

    /**
     * Create a WCP resource
     * 
     * @throws VizException
     */
    public WCPResource( WCPResourceData resourceData,
    		LoadProperties loadProperties ) throws VizException {
    	super(resourceData, loadProperties);
    	wcpResourceData = (WCPResourceData) resourceData ;
    }

	// turn the db record into a list WcpRscDataObj objects which will be 
    // individually timeMatched and added to one or more of the FrameData's.
    //
    @Override
	public IRscDataObject[] processRecord( Object pdo ) {
	    // process any records that have been stored either from readWcpData or since the last update call  
	    // and add them to the proper Frame.
	    WcpRecord wcpRec = (WcpRecord) pdo;
	    DataTime wcpTime = wcpRec.getDataTime();

	    ArrayList<WcpRscDataObj> wcpRscDataObjsList = new ArrayList<WcpRscDataObj>();
	    
	    Set<WcpSevrln> wcpSevrlnRecs = wcpRec.getWcpSevrLn();

	    for( WcpSevrln wcpSevr : wcpSevrlnRecs ) {
	    	WcpRscDataObj wcpRscDataObj = getWCPData( wcpTime, wcpSevr );

	    	if( wcpRscDataObj != null ) {
	    		wcpRscDataObjsList.add( wcpRscDataObj );
	    	}
	    }
		
		return wcpRscDataObjsList.toArray( new WcpRscDataObj[0] );
	}

    
    private WcpRscDataObj getWCPData( DataTime rTime, WcpSevrln wcpP ) {
    	
    	String wtchNumber = wcpP.getWatchNumber();
    	if (wtchNumber == null) return null;
    	
    	WcpRscDataObj wcpData  = new WcpRscDataObj();
    	wcpData.issueTime = rTime;
    	wcpData.watchType = wcpP.getEventType();
    	wcpData.eventTime = new DataTime( wcpP.getStartTime(),
    			              new TimeRange( wcpP.getStartTime(),
    			            		 		 wcpP.getEndTime() ));
    	wcpData.endTime   = new DataTime(wcpP.getEndTime());
    	wcpData.watchNumber = wcpP.getWatchNumber();
    	wcpData.numPnts = wcpP.getNumPnts();
    	
    	if (wcpData.numPnts > 0) {
    		wcpData.lat = new float[wcpData.numPnts];
    		wcpData.lon = new float[wcpData.numPnts];
    		
    		for(WcpLatlons latlon: wcpP.getWcpLatLon()) {
    			int index = latlon.getIndex();
    			if (index > 0) {
    				wcpData.lat[index-1] = latlon.getLat();
    				wcpData.lon[index-1] = latlon.getLon();
    			}
    		}
    	}
    	
        return wcpData;
    }

    public void initResource( IGraphicsTarget grphTarget) throws VizException {
    	font = grphTarget.initializeFont("Monospace", 
    			     14, new IFont.Style[] { IFont.Style.BOLD });
    	queryRecords();
    }


    public void paintFrame( AbstractFrameData frameData, 
    		IGraphicsTarget grphTarget, PaintProperties paintProps) throws VizException {

    	FrameData currFrameData = (FrameData) frameData;

    	Collection<WcpRscDataObj> wcpDataValues = currFrameData.wcpDataMap.values();

    	for( WcpRscDataObj wcpData : wcpDataValues ) {
    		//    			// Check for invalid time range
    		//    			if( currFrameTime.compareTo(wcpData.startTime) < 0 ||
    		//    					currFrameTime.compareTo(wcpData.endTime) >= 0) continue;

    		int idx = Integer.valueOf(wcpData.watchNumber.trim()) % 10;
    		if( !wcpResourceData.toDraw(idx, wcpData.watchType) ) 
    			continue;

    		RGB color = wcpResourceData.getDrawColor(idx, wcpData.watchType);;

    		// Set line style
    		LineStyle lineStyle = LineStyle.SOLID;
    		long timeDiff = (wcpData.endTime.getRefTime().getTime() -
    			      currFrameTime.getRefTime().getTime())/1000;

    		if( timeDiff <= SECONDS_IN_90MIN) 
    			lineStyle = LineStyle.DASHED;

    		// Draw watch boxes
    		double minLat = wcpData.lat[0];
    		double maxLat = wcpData.lat[0];
    		int minLatIndex = 0,maxLatIndex = 0;
    		for(int i = 0; i < wcpData.numPnts;i++) {
    			double[] latLon1 = { wcpData.lon[i], wcpData.lat[i] }; 
    			double[] p1 = descriptor.worldToPixel( latLon1 );
    			int idx2 = (i == wcpData.numPnts - 1) ? 0 : i+1;
    			double[] latLon2 = { wcpData.lon[idx2], wcpData.lat[idx2] }; 
    			double[] p2 = descriptor.worldToPixel( latLon2 );

    			if( p1 != null && p2 != null ) {
    				grphTarget.drawLine(p1[0], p1[1], 0.0, p2[0], p2[1], 0.0, 
    						color, wcpResourceData.getLineWidth(), lineStyle );
    			}

    			if (minLat > wcpData.lat[i]) {
    				minLat = wcpData.lat[i];
    				minLatIndex = i;
    			}
    			if ( maxLat < wcpData.lat[i] ) {
    				maxLat = wcpData.lat[i];
    				maxLatIndex = i;
    			}
    		}

    		// Draw labels
    		double lat = (wcpData.lat[minLatIndex] + wcpData.lat[maxLatIndex])/2;
    		double lon = (wcpData.lon[minLatIndex] + wcpData.lon[maxLatIndex])/2;
    		if ( wcpData.lon[maxLatIndex] > wcpData.lon[minLatIndex] ) {
    			lon += 0.5;
    		}
    		else {
    			lon -= 0.5;
    		}
    		double[] labelLatLon = { lon, lat }; 
    		double[] labelPix = descriptor.worldToPixel( labelLatLon );
    		if( labelPix != null ) { 
    			String[] text = new String[2];
    			if (wcpData.watchNumber.startsWith("0")) {
    				text[0] = wcpData.watchNumber.substring(1);
    			} else {
    				text[0] = wcpData.watchNumber;
    			}
    			text[1] = wcpData.eventTime.toString().substring(11, 16)
    			+ "-" + wcpData.endTime.toString().substring(11, 16);

    			// Label ON or OFF
    			if( !wcpResourceData.getTimeLabelEnable() && 
    					!wcpResourceData.getWatchNumberLabelEnable()) {
    				continue;
    			}

    			// Draw watch number label only
    			else if( !wcpResourceData.getTimeLabelEnable() && 
    					wcpResourceData.getWatchNumberLabelEnable() ) {
    				text[1] = "";
    			}
    			// Draw time label only
    			else if( wcpResourceData.getTimeLabelEnable() && 
    					!wcpResourceData.getWatchNumberLabelEnable() ) {
    				text[0] = text[1];
    				text[1] = "";
    			}

    			grphTarget.drawStrings(font, text,   
    					labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
    					new RGB[] {color, color},
    					HorizontalAlignment.LEFT, 
    					VerticalAlignment.MIDDLE );
    		}
    	}
    }
                
    
    @Override
    public void disposeInternal() {
    	super.disposeInternal();
//    	if( font != null ) {
//    		font.dispose();
//    	}
    }
    
    @Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.wcpDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}	
