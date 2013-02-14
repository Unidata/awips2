package gov.noaa.nws.ncep.viz.rsc.ffg.rsc;

import gov.noaa.nws.ncep.common.dataplugin.ffg.FfgPrecip;
import gov.noaa.nws.ncep.common.dataplugin.ffg.FfgRecord;
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.rsc.ffg.rsc.FFGResourceData.FfgParam;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;


/**
 * FFGResource - Display FFG data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  04/01/2009             Greg Hull    Initial creation.
 *  06/18/2009    115      Greg Hull    Integrate with AbstractNatlCntrsResource
 *  08/12/2009			   M. Li		Migrate from TO10 to To11
 *  08/17/2009    145      Greg Hull    Integrate with new TO11 AbstractNatlCntrsResource
 *  11/30/2009             Greg Hull    migrate to to11d6
 *  03/24/2010    259      Greg Hull    Add colorBar
 *  10/27/2010    307      Greg Hull    override processRecords and use FFGData as IRscDataObj
 *  11/15/2010    307      Greg Hull    update existing data with best timeMatch
 *  07/11/2011             Greg Hull    ColorBarResource                               
 *  07/28/2011    450      Greg Hull    NcPathManager
 *  05/23/12      785      Q. Zhou      Added getName for legend.
 *  12/19/2012   #960      Greg Hull    override propertiesChanged() to update colorBar.
 *  
 * </pre>
 * 
 * @author ghull 
 * @version 1.0
 */
public class FFGResource extends AbstractNatlCntrsResource<FFGResourceData, MapDescriptor> 
	implements INatlCntrsResource, IStationField {

	private FFGResourceData ffgRscData;

	private Unit<?> ffgUnits = NonSI.INCH;

    private StationTable stationTable;

    private IFont font=null;
        
    protected ColorBarResource cbarResource;
    protected ResourcePair     cbarRscPair;
    
    private class FFGRscDataObj implements IRscDataObject {
        String   zoneId;
        DataTime dataTime;
        double   lat;
        double   lon;
        
        float    ff01, ff03, ff06, ff12, ff24;
        
		@Override
		public DataTime getDataTime() {
			return dataTime;
		}
    }

    
    // ffgData is a map from the zoneId to the data values and is used to avoid displaying multiple values 
    // from the same zone. 
    private class FrameData extends AbstractFrameData {
        HashMap<String, FFGRscDataObj> ffgDataList;  

		public FrameData( DataTime frameTime, int timeInt ) {
			super( frameTime, timeInt );
        	ffgDataList = new HashMap<String,FFGRscDataObj>();
    	}

    	public boolean updateFrameData( IRscDataObject rscDataObj ) {
    		if( !(rscDataObj instanceof FFGRscDataObj) ) {
    			System.out.println("FFG:updateFrameData expecting FFGRscDataObj instead of: "+ 
    					rscDataObj.getClass().getName() );
    			return false;
    		}
    		
    		// for each precip record check of the zone already has a record
    		// for this frame and if so either update it or discard the record
    		FFGRscDataObj ffgRscData = (FFGRscDataObj)rscDataObj;

    		FFGRscDataObj existingFfgData = ffgDataList.get( ffgRscData.zoneId );
    				
    		// if zone is not in the list or if the new data is a better timeMatch
    		//
    		if( existingFfgData == null ) {
    			ffgDataList.put( ffgRscData.zoneId, ffgRscData );
    		}
    		else if( timeMatch( rscDataObj.getDataTime() ) < 
    				 timeMatch( existingFfgData.getDataTime() ) ) {
    			ffgDataList.put( ffgRscData.zoneId, ffgRscData );
    		}

    		return true;
    	}
    }

    
    /**
     * Create a FFG resource.
     * 
     * @param target
     *            The graphic target to draw to
     * @param refTime
     *            The reference time to request data against
     * @throws VizException
     */
    public FFGResource(FFGResourceData ncresourceData,
			LoadProperties loadProperties) {
    	super(ncresourceData, loadProperties);
    	this.ffgRscData = (FFGResourceData) resourceData;    	
    }
    
    private FFGRscDataObj getFFGData( DataTime rTime, FfgPrecip ffgP ) {
    	
    	String zoneIdStr = ffgP.getZoneID();
        Station station = stationTable.getStation(StationField.STID, zoneIdStr);
        if (station == null) return null;

    	
        FFGRscDataObj ffgData = new FFGRscDataObj();
    	ffgData.dataTime = rTime;
    	ffgData.zoneId = ffgP.getZoneID();
    	ffgData.lat  = station.getLatitude();
        ffgData.lon  = station.getLongitude();
    	ffgData.ff01 = ffgP.getFf01();
    	ffgData.ff03 = ffgP.getFf03();
    	ffgData.ff06 = ffgP.getFf06();
    	ffgData.ff12 = ffgP.getFf12();
    	ffgData.ff24 = ffgP.getFf24();

        return ffgData;
    }

    
	protected IRscDataObject[] processRecord( Object pdo ) {
		if( !(pdo instanceof FfgRecord) ) {
			System.out.println( "FFG processRecord() : Expecting FfgRecord object instead of: "+					
					pdo.getClass().getName() );
			return null;
		}
		
		FfgRecord ffgRec = (FfgRecord) pdo;
		DataTime ffgTime = ffgRec.getDataTime();

		Set<FfgPrecip> ffgPrecipRecs = ffgRec.getFfgP();
		ArrayList<FFGRscDataObj> ffgRscDataObjs = new ArrayList<FFGRscDataObj>();

		// for each precip record check of the zone already has a record
		// for this frame and if so either update it or discard the record
		for( FfgPrecip ffgPrec : ffgPrecipRecs ) {
			FFGRscDataObj ffgRscData = getFFGData( ffgTime, ffgPrec );

			if( ffgRscData != null ) {
				ffgRscDataObjs.add( ffgRscData );
			}
		}
		
		return  ffgRscDataObjs.toArray( new FFGRscDataObj[0] );
	}
	    
    // 
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	
    	stationTable = new StationTable(
    			NcPathManager.getInstance().getStaticFile( 
    					NcPathConstants.FFG_ZONES_STN_TBL).getAbsolutePath() );

    	font = null; // use default for now

        cbarRscPair  = ResourcePair.constructSystemResourcePair( 
		           new ColorBarResourceData( ffgRscData.getColorBar() ) );

        getDescriptor().getResourceList().add(  cbarRscPair );
        getDescriptor().getResourceList().instantiateResources( getDescriptor(), true );

        cbarResource = (ColorBarResource) cbarRscPair.getResource();

    	queryRecords();    	
    }

    public void paintFrame( AbstractFrameData frameData, IGraphicsTarget grphTarget, PaintProperties paintProps) throws VizException {
    	FrameData currFrameData=(FrameData) frameData;
    	IExtent extent = paintProps.getView().getExtent();

    	ColorBar colorBar = ffgRscData.getColorBar();
    	colorBar.setNumDecimals( 1 );
    	    	
    	double maxX = (extent.getMaxX() < 0 ? 0 : extent.getMaxX() );
    	double minX = (extent.getMinX() < 0 ? 0 : extent.getMinX() );
    	double maxY = (extent.getMaxY() < 0 ? 0 : extent.getMaxY() );
    	double minY = (extent.getMinY() < 0 ? 0 : extent.getMinY() );
    	maxX = ( maxX > 19999 ? 19999 : maxX);
    	minX = ( minX > 19999 ? 19999 : minX);
    	maxY = ( maxY >  9999 ?  9999 : maxY);
    	minY = ( minY >  9999 ?  9999 : minY);

    	PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY, maxY);
    	
    //	cBarPainter.paintColorBar( grphTarget, colorBar );
    	
    	// int displayWidth = (int) (descriptor.getMapWidth() * paintProps.getZoomLevel());
    	// double kmPerPixel = (displayWidth / paintProps.getCanvasBounds().width) / 1000.0;
    	// get plotWidth

    	// loop thru the ffg records
    	// (This should be fast.)
    	Collection<FFGRscDataObj> ffgDataValues = currFrameData.ffgDataList.values();

    	for( FFGRscDataObj ffgData : ffgDataValues ) {

    		double[] zoneLatLon = { ffgData.lon, ffgData.lat }; 
    		double[] zonePix = this.descriptor.worldToPixel( zoneLatLon );

    		// get the pixel coord from the lat/lon and check if it is in the paint extents
    		// TODO : display more than one parameter.
    		//
    		if( zonePix != null &&
    				correctedExtent.contains( zonePix[0], zonePix[1] ) ) 
    		{
    			Float paramVal=0.0f;

    			FfgParam ffgParam = ffgRscData.getFfgParam();

    			if( ffgParam == FfgParam.FF01 ) {
    				paramVal = ffgData.ff01;
    			}
    			else if( ffgParam == FfgParam.FF03 ) {
    				paramVal = ffgData.ff03;
    			}
    			else if( ffgParam == FfgParam.FF06 ) {
    				paramVal = ffgData.ff06;
    			}
    			else if( ffgParam == FfgParam.FF12 ) {
    				paramVal = ffgData.ff12;
    			}
    			else if( ffgParam == FfgParam.FF24 ) {
    				paramVal = ffgData.ff24;
    			}
    			else {
    				continue;
    			}
    			
    			// TODO : ? display a missing value symbol?
    			if( paramVal < 0.0 ) { // missing values are -9999
    				continue; 
    			}

    			// round to the nearest tenth
    			paramVal = (float)Math.round( paramVal*10 ) / 10; 
    			
    			for( int i=0 ; i<colorBar.getNumIntervals() ; i++ ) {
    				if( colorBar.isValueInInterval( i, paramVal, ffgUnits ) ) {
    					RGB drawColor = colorBar.getRGB(i);
    				
    					//this.screenToWorldRatio = paintProps.getCanvasBounds().width
    					//                        / paintProps.getView().getExtent().getWidth();
    					// get the adjusted location
    					//      double scaleValue = (this.plotWidth / 2.0) / screenToWorldRatio;
    					if( ffgRscData.getDisplayValues() ) {
    						grphTarget.drawString( font, Float.toString(paramVal), // ffgData.zoneId,  
    								zonePix[0], zonePix[1], 0.0, TextStyle.NORMAL, drawColor, 
    								HorizontalAlignment.CENTER, 0.0	);
    					}
    					// TODO : Draw the selected Symbol. 
    					else {
    						grphTarget.drawString( font, "X",  
    								zonePix[0], zonePix[1], 0.0, TextStyle.NORMAL, drawColor, 
    								HorizontalAlignment.CENTER, 0.0	);
    					}
    				}
    			}
    		}
    	}    	
    }                        

    public void disposeInternal() {
    	super.disposeInternal();
    	if( font != null ) {
    		font.dispose();
    	}
    	
        getDescriptor().getResourceList().remove( cbarRscPair );
    }

    public void propertiesChanged(ResourceProperties updatedProps) {
    	
    	if( cbarRscPair != null ) {
    		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
    	}
    }

	public void resourceAttrsModified() {
		// update the colorbarPainter with a possibly new colorbar
		cbarResource.setColorBar( ffgRscData.getColorBar() );
	}

	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return new FrameData(frameTime,timeInt);
	}	
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.ffgDataList.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
