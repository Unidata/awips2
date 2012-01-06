
package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import java.util.ArrayList;
import java.util.HashSet;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.PlotModelMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
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

	protected int pixelSizeHint = 90;

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

    private static HashSet<String> pluginNames = new HashSet<String>();
    
    private static ArrayList<String> sfcPlugins = new ArrayList<String>();
    
    private static ArrayList<String> fcstPlugins = new ArrayList<String>();

    static {
        pluginNames.add("bufrmos");
        pluginNames.add("goessounding");
        pluginNames.add("poessounding");
        pluginNames.add("obs");
        pluginNames.add("bufrssmi");
        pluginNames.add("bufrquikscat");
        pluginNames.add("bufrascat");
        pluginNames.add("radar");
        pluginNames.add("bufrhdw");
        pluginNames.add("lsr");
        pluginNames.add("sfcobs");
        pluginNames.add("tcg");
        pluginNames.add("svrwx");
        pluginNames.add("ldadmesonet");
        pluginNames.add("h5uair");
        pluginNames.add("scd");//scd added
        
        // for the ModelGridPlotResource 
        // TODO : is this ncgrib, or a new plugin just for the Model Plot Data?
        pluginNames.add("ncgrib");
        
        sfcPlugins.add("obs");
        sfcPlugins.add("sfcobs");
        sfcPlugins.add("bufrmos");        
    
        fcstPlugins.add("bufrmos");
        fcstPlugins.add("ncgrib");
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
		//        if (this.metadataMap.get("pluginName").getConstraintValue().equals(
		//                "bufrmos")
		//                || this.metadataMap.get("pluginName").getConstraintValue()
		//                        .equals("goessounding")) {
		//            return new PlotResource2(this, loadProperties);
		//        }
		//  
		//              if( isEdited && plotModel != null ) {
		//                      // should we go thru the mngr to create this so we can store it?
		//              }
		//              else {
		//                      getPlotModel();
		//              }
		//    
		String pluginName = this.metadataMap.get("pluginName").getConstraintValue();
		
        if (pluginNames.contains(pluginName)) {
        	if( pluginName.equals("ncgrib") ) {
        		return new ModelGridPlotResource(this, loadProperties );
        	}
        	else if( fcstPlugins.contains(pluginName)) {
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
    
    public boolean isSurfaceOnly() {
		return sfcPlugins.contains( getPluginName() );
	}
    
    public String getPluginName() {
        return metadataMap.get("pluginName").getConstraintValue();
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
