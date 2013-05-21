package gov.noaa.nws.ncep.viz.overlays.resources;


import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 20, 2009           mgao        Initial creation
 * Aug 03, 2009            ghull       rm 'Attr' getter methods
 * Aug 06, 2009            ghull       construct() -> constructResource()
 * Nov 19, 2009            ghull       Incorporate to11d6 changes (equals(), toString)
 * Apr 14  2010    #259    ghull       set legendColor from color
 * 
 * </pre>
 * 
 * This class is copied over from com.raytheon.viz.core.rsc.DbMapResourceData
 * 
 * @author mgao
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-dbOverlayResourceData")
public class DbOverlayResourceData extends AbstractNatlCntrsResourceData 
                implements INatlCntrsResourceData { 

    /** The human readable name */
    @XmlElement(required = true)
    private String mapName = null;//uma
    
    @XmlElement(required = true)
    private String dbName = "maps";

    @XmlElement(name = "table", required = true)
    private String[] tables;

    @XmlElement(name = "column", required = true)
    private String[] columns;

    @XmlElement(name = "constraint")
    private String[] constraints;

    @XmlElement()
    private String geomField = "the_geom";

    @XmlElement
    private String labelField;

    @XmlElement
    private String displayLabelField;
    
    @XmlElement
    private String shadingField;

    @XmlElement
    private String ncShadingField;
    
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = new RGB(155, 155, 155);

    @XmlElement
	private int lineWidth = 1;

    private boolean isOutlineOn = true;  

    @XmlElement
    private LineStyle lineStyle = LineStyle.SOLID;

    
    public DbOverlayResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }

        };
    }

	@Override
	public NcDisplayType[] getSupportedDisplayTypes() {
		return new NcDisplayType[] { NcDisplayType.NMAP_DISPLAY };
	}

    @SuppressWarnings("unchecked")
	@Override
    public DbOverlayResource constructResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new DbOverlayResource(this, loadProperties);
    }

    /**
     * @return the mapName
     */
    public String getMapName() {
        return mapName;
    }

    /**
     * @param mapName
     *            the mapName to set
     */
    public void setMapName(String mapName) {
        this.mapName = mapName;
    }

    /**
     * @return the constraints
     */
    public String[] getConstraints() {
        return constraints;
    }

    /**
     * @param constraints
     *            the constraints to set
     */
    public void setConstraints(String[] constraints) {
        this.constraints = constraints;
    }

    /**
     * @return the labelField
     */
    public String getLabelField() {
        return labelField;
    }

    /**
     * @param labelField
     *            the labelField to set
     */
    public void setLabelField(String labelField) {
        this.labelField = labelField;
    }

    /**
     * @return true only if the instance field displayLabelField
     * is "yes" or "true" 
     */
    public boolean isLabeled() {
    	boolean isLabeled = false; 
    	if(displayLabelField == null)
    		return isLabeled; 
    	if("yes".equalsIgnoreCase(displayLabelField) || 
    			"true".equalsIgnoreCase(displayLabelField))
    		isLabeled = true; 
    	return isLabeled; 
    }
    
    /**
     * @return the shadingField
     */
    public String getShadingField() {
        if (shadingField == null) {
            return labelField;
        }
        return shadingField;
    }

    /**
     * @return true only if the instance field shadingField
     * is "yes" or "true" 
     */
    public boolean isShaded() {
    	boolean isShaded = false; 
    	if(ncShadingField == null)
    		return isShaded; 
    	if("yes".equalsIgnoreCase(ncShadingField) || 
    			"true".equalsIgnoreCase(ncShadingField))
    		isShaded = true; 
    	return isShaded; 
    }
    
    /**
     * @param shadingField
     *            the shadingField to set
     */
    public void setShadingField(String shadingField) {
        this.shadingField = shadingField;
    }

    /**
     * @return the tables
     */
    public String[] getTables() {
        return tables;
    }

    /**
     * @param tables
     *            the tables to set
     */
    public void setTables(String[] tables) {
        this.tables = tables;
    }

    /**
     * @return the columns
     */
    public String[] getColumns() {
        return columns;
    }

    /**
     * @param columns
     *            the columns to set
     */
    public void setColumns(String[] columns) {
        this.columns = columns;
    }

    /**
     * @return the geomField
     */
    public String getGeomField() {
        return geomField;
    }

    /**
     * @param geomField
     *            the geomField to set
     */
    public void setGeomField(String geomField) {
        this.geomField = geomField;
    }

    public RGB getColor() {
		return color;
	}

	public void setColor(RGB _color) {
		this.color = _color;
		this.legendColor = color;
	}

	public int getLineWidth() {
		return lineWidth;
	}

	public void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}

	public boolean isOutlineOn() {
		return isOutlineOn;
	}

	public void setOutlineOn(boolean isOutlineOn) {
		this.isOutlineOn = isOutlineOn;
	}

	public LineStyle getLineStyle() {
		return lineStyle;
	}

	public void setLineStyle(LineStyle lineStyle) {
		this.lineStyle = lineStyle;
	}

    @Override
    public boolean equals(Object obj) {
		if (!super.equals(obj)) {
			return false;
		}

		DbOverlayResourceData other = (DbOverlayResourceData) obj;

        if (this.columns != null && other.columns == null) {
            return false;
        } else if (this.columns == null && other.columns != null) {
            return false;
        } else if (this.columns != null
                && this.columns.equals(other.columns) == false) {
            return false;
        }

        if (this.constraints != null && other.constraints == null) {
            return false;
        } else if (this.constraints == null && other.constraints != null) {
            return false;
        } else if (this.constraints != null
                && this.constraints.equals(other.constraints) == false) {
            return false;
        }

        if (this.geomField != null && other.geomField == null) {
            return false;
        } else if (this.geomField == null && other.geomField != null) {
            return false;
        } else if (this.geomField != null
                && this.geomField.equals(other.geomField) == false) {
            return false;
        }

        if (this.labelField != null && other.labelField == null) {
            return false;
        } else if (this.labelField == null && other.labelField != null) {
            return false;
        } else if (this.labelField != null
                && this.labelField.equals(other.labelField) == false) {
            return false;
        }

        if (this.mapName != null && other.mapName == null) {
            return false;
        } else if (this.mapName == null && other.mapName != null) {
            return false;
        } else if (this.mapName != null
                && this.mapName.equals(other.mapName) == false) {
            return false;
        }

        if (this.shadingField != null && other.shadingField == null) {
            return false;
        } else if (this.shadingField == null && other.shadingField != null) {
            return false;
        } else if (this.shadingField != null
                && this.shadingField.equals(other.shadingField) == false) {
            return false;
        }

        if (this.tables != null || other.tables != null) {
            if (this.tables == null || other.tables == null) {            	
            	return false;
            }
            else if( this.tables.length != other.tables.length ) {
            	return false;
            }
        	else {
        		for( int i=0 ; i<this.tables.length ; i++ ) {
        			if( !this.tables[i].equals( other.tables[i] )) {
        	            return false;		
        			}
        		}
        	}            
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.mapName;
    }

	public String getDbName() {              //uma
		return dbName;
	}

	public void setDbName(String dbName) {
		this.dbName = dbName;
	}
    
}
