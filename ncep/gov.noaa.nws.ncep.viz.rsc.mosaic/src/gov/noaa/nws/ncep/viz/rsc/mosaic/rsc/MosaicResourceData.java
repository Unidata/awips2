package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;

import gov.noaa.nws.ncep.edex.plugin.mosaic.util.MosaicInfoDict;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Provide Radar Mosaic raster rendering support 
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer     Description
 *  ------------ ----------  -----------  --------------------------
 *  01/2010	  	   204 	 	  M. Li       Initial Creation.
 *  03/2010                   B. Hebbard  Port TO11D6->TO11DR3; add localization
 *  04/2010        259        Greg Hull   Added Colorbar
 *  09/2010        307        Greg Hull   set legendString once from the productCode
 *                                        in the metadataMap.
 *  07/2011        450        Greg Hull   NcPathManager
 *                                         
 * </pre>
 * 
 * @author mli
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-MosaicResourceData")
public class MosaicResourceData extends AbstractNatlCntrsRequestableResourceData {
 
	@XmlElement
    private String colorMapName;

	@XmlElement
    private ColorBarFromColormap colorBar;
	
	@XmlElement
	private Float alpha;

	@XmlElement
	private Float brightness;
	
	@XmlElement
	private Float contrast;

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

	// Both the productCode and the productName should be given in the metadataMap
	// but only one is really needed. 
	// TODO write code to make only one of these necessary and set productCode from
	// the productName if not set.
    private Integer productCode=0;
    
    private static MosaicInfoDict infoDict = null;
    
    // The legend is set from MosaicInfoDict based on 
    // the product code which is set in the metadataMap.
    private String legendString = null; 
    
	private static void loadRadarInfo() {
    	String radarDirname = 
        		NcPathManager.getInstance().getStaticFile(
        				NcPathConstants.MOSAIC_INFO ).getParent();
    	
    	// Note: MosaicInfoDict actually ignores the file and uses 
    	// the directory to find the mosaicInfo.txt file.    	
    	infoDict = MosaicInfoDict.getInstance(radarDirname);
    }

    public MosaicResourceData() {
        this.nameGenerator = new AbstractNameGenerator() {

        	// This method is not called since MosaicResource is overriding
        	// getName()
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
            	if( legendString != null ) {
            		return legendString;
            	}
            	
            	if( infoDict == null ) {
            		loadRadarInfo();
                	if( infoDict == null ) {	
                		legendString = new String( "Radar Mosaic: Product Code "+
                				Integer.toString( getProductCode()) );
                		return legendString;
                	}
            	}
            	
        		String prodName = infoDict.getInfo(getProductCode()).getName();
        		String unitName = infoDict.getInfo(getProductCode()).getUnit();
        		if (unitName == null) {
        			unitName = " ";
        		} else {
        			if (!unitName.contains("(")) {
        				String temp = " (";
        				temp += unitName + ") ";
        				unitName = temp;
        			}
        			if (unitName.contains("/10")) {
        				unitName = unitName.replace("/10", "");
        			}
        			if (unitName.contains("*1000")) {
        				unitName = unitName.replace("*1000", "");
        			}
        		}

        		legendString = new String( prodName + unitName
        					+ (int) (Math.log(infoDict.getInfo(productCode)
        								.getNumLevels()) / Math.log(2)) + "-bit " );
        		return legendString;
//                if (productName.equals("NO DATA")) {
//                	MosaicRecord currRec = null;
//
//                    if (currRec != null) {
//                        if (infoDict == null) {
//                            loadRadarInfo();
//                        }
//
//                        MosaicInfo info = infoDict.getInfo(currRec
//                                .getProductCode());
//                        productName = info.getName() + " "
//                                + currRec.getDataTime().getLegendString();
//                    }
//                }
//                return productName;
            }

        };
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {

    	return new MosaicResource(this, loadProperties);
    	
    }

    public Integer getProductCode() {
    	if( productCode == 0 && 
    		metadataMap.containsKey("productCode") ) {
    		RequestConstraint reqCon = metadataMap.get("productCode");
    		productCode = Integer.parseInt( reqCon.getConstraintValue() ); 
    	}
		return productCode;
	}

	public void setProductCode(Integer productCode) {
		this.productCode = productCode;
	}

    @Override
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof MosaicResourceData == false) {
            return false;
        }

        MosaicResourceData other = (MosaicResourceData) obj;
        
        if (this.getProductCode() != other.getProductCode() ) {
            return false;
        } 
        
        return true;
    }

	public String getColorMapName() {
		return colorMapName;
	}

	public void setColorMapName(String colorMapName) {
		this.colorMapName = colorMapName;
	}
	
	
    public ColorBarFromColormap getColorBar() {
		return colorBar;
	}

	public void setColorBar(ColorBarFromColormap cBar) {
		this.colorBar = cBar;
	}	
}
