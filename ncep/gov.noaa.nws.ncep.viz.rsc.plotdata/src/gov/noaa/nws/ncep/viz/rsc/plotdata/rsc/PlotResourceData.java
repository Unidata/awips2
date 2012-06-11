
package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import java.util.ArrayList;
import java.util.HashSet;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilterMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.PlotModelMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.pointdata.rsc.retrieve.AbstractDbPlotInfoRetriever;

/**
 * Resource data for plots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009            njensen     Initial creation
 * Apr 09, 2010   259      ghull       move legendColor to Abstract
 * 07/26/2010    T285      qzhou       Added bufrua. Added Man. level to legend on screen
 * 08/11/2010    #273      ghull       remove getResourceName(), add isSurfaceOnly(),
 *                                     get plotModel cat from resourceName, get plotModelName
 *                                     from bundle file.
 * 10/13/2010    #307      ghull       create FcstPlotResource for bufrmos
 * 03/07/2011     migration ghull     use AbstractDbPlotInfoRetriever
 * 03/04/2011              ghull       change plotModel category to plugin name
 * 05/12/2011    #441      ghull       remove upper/lower limit
 * 09/03/2011              ghull       rmove reportTypeKey; add ncscd 
 * 09/14/2011    #457      sgurung     Renamed h5 to nc
 * 09/22/2011    #459      ghull       added modelsounding, ncairep and ncpirep
 * 10/14/2011              ghull       added ncpafm, nctaf
 * 10/18/2011              sgurung     Modified setReportType() to set constrainttype as ConstraintType.IN 
 * 10/19/2011              ghull       add TafPlotResource
 * 11/01/2011    #482      ghull       added plotDensity, comment out unimplemented plugins
 * 04/09/2012    #615      sgurung     Added conditionalFilterName and conditionalFilter
 *                           
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-PlotResourceData")
public class PlotResourceData extends AbstractNatlCntrsRequestableResourceData implements
INatlCntrsResourceData {

	protected int pixelSizeHint = 80;

	// used in metadata query. For bufrmos this is the bufrmos type.
	@XmlElement
	protected String reportType = null;

	@XmlElement
	protected String legendString = null;
	
	@XmlElement
	protected String spiFile = null;

	// The name of the plotModel is the name of the resource prm file.
	@XmlElement
	protected String plotModelName = null;

	// if the plotModel has not been edited then these values will be from the 
	// plotModelName. Otherwise this will contain the edited plotModel values.
	@XmlElement
	protected PlotModel plotModel = null;

	@XmlElement
    protected String levelKey = null;

	@XmlElement
    protected Integer plotDensity = 10;	
	
// This is for the 'range' plotMode which we currently aren't. We'll need similar 
// functionality in the future but this will need to be on a per parameter basis and
// not set by the resource.
//	@XmlElement
//    protected double lowerLimit = -9999.0;
//
//	@XmlElement
//    protected double upperLimit = 10000000.0;

	@XmlElement
    protected boolean plotMissingData = false;

    @XmlElement
    protected AbstractDbPlotInfoRetriever plotInfoRetriever;
    
    @XmlElement
	protected String conditionalFilterName = null;
	
    //@XmlElement
	protected ConditionalFilter conditionalFilter = null;

    private static HashSet<String> pluginNames = new HashSet<String>();
    
    private static ArrayList<String> sfcPlugins = new ArrayList<String>();
    
    private static ArrayList<String> fcstPlugins = new ArrayList<String>();

    static {
//        pluginNames.add("goessounding");
//        pluginNames.add("poessounding");
        pluginNames.add("obs");
//        pluginNames.add("bufrssmi");
//        pluginNames.add("bufrquikscat");
//        pluginNames.add("bufrascat");
//        pluginNames.add("radar");
//        pluginNames.add("bufrhdw");
//        pluginNames.add("lsr");
        pluginNames.add("sfcobs");
//        pluginNames.add("tcg");
//        pluginNames.add("svrwx");
//        pluginNames.add("ldadmesonet");
        pluginNames.add("ncuair");
        pluginNames.add("ncscd");
//        pluginNames.add("scd");
        pluginNames.add("ncairep");
        pluginNames.add("ncpirep");
        pluginNames.add("nctaf");
        pluginNames.add("ncpafm");
        pluginNames.add("modelsounding");
        pluginNames.add("bufrmosLAMP");
        pluginNames.add("bufrmosAVN");
        pluginNames.add("bufrmosETA");
        pluginNames.add("bufrmosGFS");
        pluginNames.add("bufrmosNAM");
        pluginNames.add("bufrmosHPC");
        pluginNames.add("bufrmosMRF");
        
        // for the ModelGridPlotResource 
        // TODO : is this ncgrib, or a new plugin just for the Model Plot Data?
//        pluginNames.add("ncgrib");
        
        // We could key off of levelKey.equals("Surface") but for airep and pirep 
        // levelKey won't be surface.
        sfcPlugins.add("obs");
        sfcPlugins.add("sfcobs");
        sfcPlugins.add("ncscd");
        sfcPlugins.add("ncairep");
        sfcPlugins.add("ncpirep");
        sfcPlugins.add("nctaf");
        sfcPlugins.add("ncpafm");
        sfcPlugins.add("bufrmosLAMP");
        sfcPlugins.add("bufrmosAVN");
        sfcPlugins.add("bufrmosETA");
        sfcPlugins.add("bufrmosGFS");
        sfcPlugins.add("bufrmosNAM");
        sfcPlugins.add("bufrmosHPC");
        sfcPlugins.add("bufrmosMRF");
    
        fcstPlugins.add("bufrmosLAMP");
        fcstPlugins.add("bufrmosAVN");
        fcstPlugins.add("bufrmosETA");
        fcstPlugins.add("bufrmosGFS");
        fcstPlugins.add("bufrmosNAM");
        fcstPlugins.add("bufrmosHPC");
        fcstPlugins.add("bufrmosMRF");
        fcstPlugins.add("ncgrib");
        fcstPlugins.add("modelsounding");
        fcstPlugins.add("nctaf");
        fcstPlugins.add("ncpafm");
    }

    // taf is an exception to the rule that forecast resources have cycle times.
    //
    @Override
	public boolean isForecastResource() {
		if( getPluginName().equals("nctaf") ) {
			return true;
		}
		else { // or could base off of fcstPlugins...same result
			return super.isForecastResource();
		}
	}

	public PlotResourceData( ) {
		super();
		this.nameGenerator = new AbstractNameGenerator() {
			@Override
			public String getName(AbstractVizResource<?, ?> resource) {
				if( isSurfaceOnly() ) {
					return ( legendString != null ? legendString : "Plot Data" );					
				}
				else {
					return ( legendString != null ? legendString + " " + getLevelKey() + " mb" : "Plot Data" );
				}
			}
		};
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
	 * constructResource(com.raytheon.uf.viz.core.comm.LoadProperties,
	 * com.raytheon.edex.db.objects.PluginDataObject[])
	 */
	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects) {
		
		// Force the reportType to be set in the metadata map for non-bufrmos plugins
		// 
		if( !getPluginName().startsWith("bufrmos") ) {		
			setReportType( getReportType() );
		}
		
		String pluginName = this.metadataMap.get("pluginName").getConstraintValue();
		
        if (pluginNames.contains(pluginName)) {
        	if( pluginName.equals("ncgrib") ) {
        		return new ModelGridPlotResource(this, loadProperties );
        	}        	
        	else if( pluginName.equals("nctaf") ) {
        		return new TafPlotResource(this, loadProperties);
        	}
        	else if( fcstPlugins.contains( pluginName)) {
        		return new FcstPlotResource(this, loadProperties);
        	}
        	else {
        		return new PlotResource2(this, loadProperties);
        	}
		}
		else 
			System.out.println("Plugin "+ pluginName + " not supported by PlotResource2");
			return null; //new PlotResource( this, loadProperties );
	}
		
	public String getReportType() {
		return reportType;
	}

	public void setReportType(String rType) {
		reportType = rType;
		
		RequestConstraint req = new RequestConstraint(
				reportType, ConstraintType.IN );

		getMetadataMap().put( "reportType", req );
	}


	public String getLegendString() {
		return legendString;
	}

	public void setLegendString(String legendString) {
		this.legendString = legendString;
	}

	public int getPixelSizeHint() {
		return pixelSizeHint;
	}

	public String getSpiFile() {
		return spiFile;
	}

	public void setSpiFile(String spiFile) {
		this.spiFile = spiFile;
	}

	public String getPlotModelName() {
		return plotModelName;
	}

	public void setPlotModelName(String name) {
		this.plotModelName = name;
	}

	public void setPlotModel( PlotModel pm ) {
		plotModel = pm;    
	}

	public PlotModel getPlotModel( ) {
		// if the plotModel has not been set yet (either from xml in the bundle file
		// or from the plotModelName attribute) then get it from the manager
		//
		if( plotModel == null ) {			
			plotModel = PlotModelMngr.getInstance().getPlotModel( getPluginName(), getPlotModelName() );
			
			if( plotModel == null ) {
				System.out.println("Unable to find plotModel for plugin '"+getPluginName()+
						            "' for '"+ getPlotModelName() +"'.");
				plotModel = PlotModelMngr.getInstance().getDefaultPlotModel();
				plotModel.setName( getPlotModelName() );
				plotModel.setPlugin( getPluginName() );
				return plotModel;
			}
		}
		return new PlotModel( plotModel );
	}

    public boolean isPlotMissingData() {
        return plotMissingData;
    }

    public void setPlotMissingData(boolean plotMissingData) {
        this.plotMissingData = plotMissingData;
    }

    public AbstractDbPlotInfoRetriever getPlotInfoRetriever() {
        return plotInfoRetriever;
    }

    public void setPlotInfoRetriever(AbstractDbPlotInfoRetriever plotInfoRetriever) {
        this.plotInfoRetriever = plotInfoRetriever;
    }

    public String getLevelKey() {
        return levelKey;
    }

    public void setLevelKey(String levelKey) {
        this.levelKey = levelKey;
    }
    
	public Integer getPlotDensity() {
		return plotDensity;
	}

	public void setPlotDensity(Integer plotDensity) {
		this.plotDensity = plotDensity;
	}

    public boolean isSurfaceOnly() {
		return sfcPlugins.contains( getPluginName() );
	}    
    
    public String getConditionalFilterName() {
		return conditionalFilterName;
	}

	public void setConditionalFilterName(String name) {
		this.conditionalFilterName = name;
	}

    public ConditionalFilter getConditionalFilter( ) {
		// if the conditionalFilter has not been set yet (from xml file
		// or from the conditionalFilterName attribute) then get it from the manager
		if( conditionalFilter == null ) {			
			conditionalFilter = ConditionalFilterMngr.getInstance().getConditionalFilter( getPluginName(), getConditionalFilterName() );
			
			if( conditionalFilter == null ) {
				System.out.println("Unable to find ConditionalFilter for plugin '"+getPluginName()+
						            "' for '"+ getConditionalFilterName() +"'.");
				conditionalFilter = ConditionalFilterMngr.getInstance().getDefaultConditionalFilter();
				conditionalFilter.setName( getConditionalFilterName() );
				conditionalFilter.setPlugin( getPluginName() );
				conditionalFilter.setDescription( "" );
				conditionalFilter.getConditionalFilterElements();
				return conditionalFilter;
			}
		}
		return new ConditionalFilter( conditionalFilter );
	}
    
	public void setConditionalFilter(ConditionalFilter conds) {
		this.conditionalFilter = conds;	
	}    
	
    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof PlotResourceData == false) {
            return false;
        }

        PlotResourceData other = (PlotResourceData) obj;

        if (this.spiFile != null && other.spiFile == null) {
            return false;
        } else if (this.spiFile == null && other.spiFile != null) {
            return false;
        } else if (this.spiFile != null
                && this.spiFile.equals(other.spiFile) == false) {
            return false;
        }

        if (this.levelKey != null && other.levelKey == null) {
            return false;
        } else if (this.levelKey == null && other.levelKey != null) {
            return false;
        } else if (this.levelKey != null
                && this.levelKey.equals(other.levelKey) == false) {
            return false;
        }

        if (this.plotDensity != null && other.plotDensity == null) {
            return false;
        } else if (this.plotDensity == null && other.plotDensity != null) {
            return false;
        } else if (this.plotDensity != null
                && this.plotDensity.equals(other.plotDensity) == false) {
            return false;
        }

        if (this.plotInfoRetriever != null && other.plotInfoRetriever == null) {
            return false;
        } else if (this.plotInfoRetriever == null
                && other.plotInfoRetriever != null) {
            return false;
        } else if (this.plotInfoRetriever != null
                && this.plotInfoRetriever.equals(other.plotInfoRetriever) == false) {
            return false;
        }

        return (this.pixelSizeHint == other.pixelSizeHint );
//                && this.lowerLimit == other.lowerLimit && this.upperLimit == other.upperLimit);
    }
}
