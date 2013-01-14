package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.HashMap;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasMapCoverage;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.PredefinedAreasMngr;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider;

/**
 * Resource data for satellite data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 10, 2009           mgao		   Initial creation
 * July 31, 2009           ghull       to11 migration
 * Aug  26, 2009           ghull       Integrate with AbstractNatlCntrsResource
 * Apr  15, 2010   #259    ghull       Added ColorBar
 * Nov  20, 2012   #630    ghull       implement IGridGeometryProvider
 * 
 * This class is copied from com.raytheon.viz.satellite.rsc.SatResourceData
 * for To 11 integration
 * 
 * </pre>
 * 
 * @author mgao
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NcSatelliteResourceData")
public class SatelliteResourceData extends AbstractNatlCntrsRequestableResourceData
								   implements IGridGeometryProvider {

	enum SatelliteType {
		GINI, MCIDAS
	}
	
	@XmlElement
	private Float alpha;

	@XmlElement
	private Float brightness;
	
	@XmlElement
	private Float contrast;
	
	@XmlElement
    private String colorMapName;
	
	@XmlElement
    private ColorBarFromColormap colorBar;

	@XmlElement
	private SatelliteType satelliteType;
	
	@XmlElement
	private String displayUnitStr;
	
	private Unit<?> displayUnit;
	
	private AbstractSatelliteResource satRsc = null;
	
	private GeneralGridGeometry gridGeom=null;
	
    public SatelliteResourceData() {
        super();
        
        displayUnit = null;
        displayUnitStr = null;
        
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
            	return ((AbstractSatelliteResource)resource).getLegendString();
            	
//            	String satName = metadataMap.get("satelliteName").getConstraintValue();
//            	String imgType="";
//            	if( satType == SatelliteType.GINI ) {
//            		imgType = metadataMap.get("physicalElement").getConstraintValue();
//            	}
//            	else if( satType == SatelliteType.MCIDAS ) {
//            		imgType = metadataMap.get("imageType").getConstraintValue();
//            	}
//            	String areaName = metadataMap.get("areaName").getConstraintValue();
//            	
//            	return satName+"-"+imgType +" ("+satType.toString() +")";            	
            }
        };
        
    }

	
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
//        records = new SatelliteRecord[objects.length];
//        for (int i = 0; i < objects.length; i++) {
//            records[i] = (SatelliteRecord) objects[i];
//        }
    	if( satelliteType == SatelliteType.GINI ) {
    		satRsc = new GiniSatResource(this, loadProperties); 
            return satRsc;
    	}
    	else if( satelliteType == SatelliteType.MCIDAS ) {
    		satRsc = new McidasSatResource(this, loadProperties);
    		return satRsc;
    	}
    	else {
    		System.out.println("Unrecognized satellite type: "+satelliteType.toString() );
    		return null;
    	}
    
    }

	public AbstractVizResource<?, ?> getResource() {
		return satRsc;
	}


	public Unit<?> getDisplayUnit() {
		if( displayUnit == null ) {
			setDisplayUnitStr( displayUnitStr );
		}
		return displayUnit;
	}
	
	public String getDisplayUnitStr() {
		return displayUnitStr;
	}

	public void setDisplayUnitStr(String dispUnitStr) {
		this.displayUnitStr = dispUnitStr;

		if( displayUnit == null ) {
			if( displayUnitStr != null ) {
	            try {
	            	displayUnit = UnitFormat.getUCUMInstance().parseSingleUnit(
	            			 displayUnitStr, new ParsePosition(0));
	            } catch (ParseException e) {
	                System.out.println("Unable parse display units : " + displayUnitStr );
	            }
			}
		}
	}
	
	public String getColorMapName() {
		return colorMapName;
	}

	public void setColorMapName(String cmapName) {
		colorMapName = cmapName;
	}
	
    public ColorBarFromColormap getColorBar() {
		return colorBar;
	}

	public void setColorBar(ColorBarFromColormap cBar) {
		this.colorBar = cBar;
	}
	
    public Float getAlpha() {
		return alpha;
	}


	public void setAlpha(Float alpha) {
		this.alpha = alpha;
	}


	public Float getBrightness() {
		return brightness;
	}


	public void setBrightness(Float brightness) {
		this.brightness = brightness;
	}


	public Float getContrast() {
		return contrast;
	}

	public void setContrast(Float contrast) {
		this.contrast = contrast;
	}


	@Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }
        if (obj instanceof SatelliteResourceData == false) {
            return false;
        }

        SatelliteResourceData other = (SatelliteResourceData) obj;

        if (this.colorMapName != null && other.colorMapName == null) {
            return false;
        } else if (this.colorMapName == null && other.colorMapName != null) {
            return false;
        } else if (this.colorMapName != null
                && this.colorMapName.equals(other.colorMapName) == false) {
            return false;
        }
        
        if (this.displayUnitStr != null && other.displayUnitStr == null) {
            return false;
        } else if (this.displayUnitStr == null && other.displayUnitStr != null) {
            return false;
        } else if (this.displayUnitStr != null
                && this.displayUnitStr.equals(other.displayUnitStr) == false) {
            return false;
        }
        
        if( this.satelliteType != other.satelliteType ) {
        	return false;
        }
        
        if ( (this.alpha != null && other.alpha == null)
        		|| (this.alpha == null && other.alpha != null)
        		|| (this.alpha != null && this.alpha.equals(other.alpha) == false)){
        	return false;
        	
	    }
        	
        if ( (this.brightness != null && other.brightness == null)
        		|| (this.brightness == null && other.brightness != null)
        		|| (this.brightness != null && this.brightness.equals(other.brightness) == false)){
        	return false;
        	
	    }
        
        if ( (this.contrast != null && other.contrast == null)
        		|| (this.contrast == null && other.contrast != null)
        		|| (this.contrast != null && this.contrast.equals(other.contrast) == false)){
        	return false;
        	
	    }
        
        return true;
    }

	// Only the AbstractSatelliteResource can actually get the coverage area from the
	// loaded data, but we will need to test the ResourceData object for IGridCoverageCapable
	// before the data is loaded. 
	@Override
	public GeneralGridGeometry getGridGeometry() {

//        if( satRsc != null ) {
//        	gridGeom = satRsc.getGridGeometry();
//		}
//		else {
			try {
				HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
						getMetadataMap());

				LayerProperty prop = new LayerProperty();
				prop.setDesiredProduct(ResourceType.PLAN_VIEW);
				prop.setEntryQueryParameters(queryList, false);
				prop.setNumberOfImages(1); // just need 1 record
				String script = null;
				script = ScriptCreator.createScript(prop);

				Object[] satRecList = Connector.getInstance().connect(script, null, 10000);

				if( satRecList != null && satRecList.length > 0 ) {
					if( satRecList[0] instanceof McidasRecord ) {
						McidasRecord satRec = (McidasRecord)satRecList[0];
	
	                    if( satRec.getProjection().equalsIgnoreCase("STR") ||
	                    	satRec.getProjection().equalsIgnoreCase("MER") ||
	                    	satRec.getProjection().equalsIgnoreCase("LCC")) {
	                        	
	                    	// for remapped projections such as MER, LCC, STR
	                    	gridGeom = MapUtil.getGridGeometry( satRec.getSpatialObject() );
	                    } 
	                    else {
	                		McidasMapCoverage coverage = satRec.getCoverage();
	                		
	                	    GeneralEnvelope env = new GeneralEnvelope(2);
	                	    env.setCoordinateReferenceSystem( satRec.getCoverage().getCrs() );
	                	    
	                	    int minX = coverage.getUpperLeftElement();
	                	    int maxX = coverage.getUpperLeftElement() + ( coverage.getNx() * coverage.getElementRes() );
	                	    int minY = coverage.getUpperLeftLine() + ( coverage.getNy() * coverage.getLineRes() );
	                	    minY = -minY;
	                	    int maxY = -1 * coverage.getUpperLeftLine();
	                	    env.setRange(0, minX, maxX);
	                	    env.setRange(1, minY, maxY);
	                	    
	                	    gridGeom = new GridGeometry2D(
	                	    	new GeneralGridEnvelope(new int[] {
	                                   0, 0 }, new int[] { coverage.getNx(), 
	                	    							   coverage.getNy() }, false), env);
	                    }
					}
					else if( satRecList[0] instanceof SatelliteRecord ) {
						SatelliteRecord satRec = (SatelliteRecord)satRecList[0];
						
						int proj = satRec.getCoverage().getProjection();

						if( proj == 1 || proj == 3 || proj == 5 ) { // MER, LCC or STR 
	                    	// for remapped projections such as MER, LCC, STR
	                    	gridGeom = MapUtil.getGridGeometry( satRec.getSpatialObject() );
						}
						else {
							System.out.println("Unable to get Coverage for projection "+
									proj+ "." );							
						}
					}
					else {
						System.out.println("Unknown Satellite Record " );
					}
				}
						
			} catch (VizException e) {
			}

			// return something meaningful
			if( gridGeom == null ) {
				try {
					gridGeom = PredefinedAreasMngr.getPredefinedArea(  
							NmapCommon.getDefaultMap() ).getGridGeometry();
				} catch (VizException e) {
				}
			}
//		}

        return gridGeom;
	}
	
	public String getProviderName() {
		return getResourceName().getAbbreviatedName();
	}
	
	//  this 
	public double[] getMapCenter() {
		return null;		
	}
	
	public String getZoomLevel() {
		return IGridGeometryProvider.ZoomLevelStrings.FitToScreen.toString();
	}
	
	public void setZoomLevel( String zl ) {
	}	
}
