package gov.noaa.nws.ncep.viz.rsc.wcp.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;

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
 *  12/14/2009              B. Hebbard  Migrate TO11D3->D6 (import changes only)
 * 
 * </pre>
 * 
 * @author mli 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-WCPResourceData")
public class WCPResourceData extends AbstractNatlCntrsRequestableResourceData 
               implements IMiscResourceData, INatlCntrsResourceData {

	public static final String[] WCP_WATCH_TYPE = {"WT", "WS"};
    
	@XmlElement
	protected String sourceName;

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB color; // Resource legend color
        
    // indexes into the WCPAttr list for each watch.
    //
    private final static int thunderstormWatchIndx = 0;
    private final static int tornadoWatchIndx = 1;
    private final static int watch0Indx = 2;
    private final static int watch1Indx = 3;
    private final static int watch2Indx = 4;
    private final static int watch3Indx = 5;
    private final static int watch4Indx = 6;
    private final static int watch5Indx = 7;
    private final static int watch6Indx = 8;
    private final static int watch7Indx = 9;
    private final static int watch8Indx = 10;
    private final static int watch9Indx = 11;
    private final static int watchCount = 12;    
    
    @XmlElement
    protected String timeLabelName;
    @XmlElement
    protected boolean timeLabelEnable;
    @XmlElement
    protected String watchNumberLabelName;
    @XmlElement
    protected boolean watchNumberLabelEnable;
    @XmlElement
    protected String colorCodeName;
    @XmlElement
    protected boolean colorCodeEnable;
    @XmlElement
    protected int lineWidth;
    
    // See below for more XmlElements which are defined on the methods since the values are 
    // actually stored in the wcpAttrs array instead of separate fields.
    
    
    // Attribute array for watch name/enable/color attributes
    // attrName is the prefix for all of the attribute names expected in the rscAttrSet. So the WCPAttr with
    // attrName ='thunderstorm' will get/set its values with the "thunderstormName", "thunderstormEnabled",
    // and "thunderstormColor" attributes from the rscAttrSet object from AbstractNatlCntrsResource
    //  
	// no longer need to copy the attributes from the wcpAttrs array to the member variables because
	// the member variables are now get/set directly into the wcpAttrs array.
    private class WCPAttr {
    	
    	String attrName; // ex. 'tornado'; the base for the names of the attributes for color, enabled
    	String label;    // the 'Name' of the wcpAttr on the GUI, ex "Thunderstorm" 
    	boolean enable;
    	RGB color;
    	
    	public WCPAttr( String wcpAttrName ) { //, ResourceAttrSet ras ) {
    		attrName = wcpAttrName;
    		label = wcpAttrName; // default value
    		color = new RGB(255,255,255);
    		enable = false;
    	}
    	
		public String getAttrName() {
			return attrName + "Name"; // ex. tornadoName;
		}
		public String getEnabledAttrName() {
			return attrName + "Enable"; // ex tornadoEnabled;
		}
		public String getColorAttrName() {
			return attrName + "Color"; // colorAttrName;
		}
		public String getLabel() {
			return label;
		}
		public void setLabel(String name) {
			this.label = name;
		}
		public RGB getColor() {
			return color;
		}
		public void setColor(RGB color) {
			this.color = color;
		}
		public boolean isEnable() {
			return enable;
		}
		public void setEnable(boolean enable) {
			this.enable = enable;
		}
    }

    // this label/enable/color attribute values in this array are initialized by the set methods 
    // for the individual attributes (ex. setTornadoEnable)
    //
    private WCPAttr[] wcpAttrs = null;
    
    /**
     * Create a WCP resource
     * 
     * @throws VizException
     */
     
    public WCPResourceData() throws VizException {
        super();
    	wcpAttrs = new WCPAttr[watchCount];
    	createWcpAttrsArray();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "WCP";
            }

        };
        color = new RGB(155, 155, 155);;
    }

    @Override
    public boolean isEventResource() {
    	return true;
    }
    
    private void createWcpAttrsArray() {
    	    	
    	wcpAttrs[thunderstormWatchIndx] = new WCPAttr( "thunderstorm" );
    	wcpAttrs[tornadoWatchIndx] = new WCPAttr( "tornado" );
    	wcpAttrs[watch0Indx] = new WCPAttr( "watch0" );
    	wcpAttrs[watch1Indx] = new WCPAttr( "watch1" );
    	wcpAttrs[watch2Indx] = new WCPAttr( "watch2" );
    	wcpAttrs[watch3Indx] = new WCPAttr( "watch3" );
    	wcpAttrs[watch4Indx] = new WCPAttr( "watch4" );
    	wcpAttrs[watch5Indx] = new WCPAttr( "watch5" );
    	wcpAttrs[watch6Indx] = new WCPAttr( "watch6" );
    	wcpAttrs[watch7Indx] = new WCPAttr( "watch7" );
    	wcpAttrs[watch8Indx] = new WCPAttr( "watch8" );
    	wcpAttrs[watch9Indx] = new WCPAttr( "watch9" );
    }


    // If or not to draw specific watch number or event type
    protected boolean toDraw(int watchNum, String eventType) {
    	if( watchNumberLabelEnable ) {// !wcpAttrs.get(wcpAttrs.size()-2).isEnable()) { 
    		return wcpAttrs[watchNum+2].isEnable(); // +2 skips past the thunderstorm & tornado entries
    	}
    	else { 
    		if (eventType.equals(WCP_WATCH_TYPE[0])) {
    			return wcpAttrs[tornadoWatchIndx].isEnable();
    		}
    		else {
    			return wcpAttrs[thunderstormWatchIndx].isEnable();
    		}
    	}
    }
    
    // Get color for specific watch number or event type
    protected RGB getDrawColor(int watchNum, String eventType) {
    	
    	if( colorCodeEnable ) { // wcpAttrs.get(wcpAttrs.size()-2).isEnable()) { // use color code
    		return wcpAttrs[watchNum+2].getColor();
    	} 
    	else {
    		if (eventType.equals(WCP_WATCH_TYPE[0])) {
    			return wcpAttrs[tornadoWatchIndx].getColor();
    		}
    		else { 
    			return wcpAttrs[thunderstormWatchIndx].getColor();
    		}
    	}
    }
        
    public void setSourceName(String sn) {
        this.sourceName = sn;
    }

    public String getSourceName() {
        return this.sourceName;
    }
    
    public RGB getColor() {
        return this.color;
    }

    public void setColor(RGB color) {
        this.color = color;
    }
   
	public int getLineWidth() {
		return lineWidth;
	}

	public void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}
	
	// The following get/set methods define JaxB properties which define XmlElements. This is
	// done instead of using a member variable as other resource attributes are defined.
	
	@XmlElement(name="thunderstormName")
	public String getThunderstormName() {
		return wcpAttrs[thunderstormWatchIndx].getLabel();// thunderstormName;
	}

	public void setThunderstormName(String tn) {
		wcpAttrs[thunderstormWatchIndx].setLabel( tn );// thunderstormName;
	}

	@XmlElement(name="thunderstormEnable")
	public boolean getThunderstormEnable() {
		return wcpAttrs[thunderstormWatchIndx].isEnable();
	}

	public void setThunderstormEnable(boolean te) {
		wcpAttrs[thunderstormWatchIndx].setEnable(te);
	}

	@XmlElement(name="thunderstormColor")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getThunderstormColor() {
		return wcpAttrs[thunderstormWatchIndx].getColor();
	}

	public void setThunderstormColor(RGB tc) {
		wcpAttrs[thunderstormWatchIndx].setColor(tc);
	}
	
	@XmlElement(name="tornadoName")
	public String getTornadoName() {
		return wcpAttrs[tornadoWatchIndx].getLabel();// thunderstormName;
	}

	public void setTornadoName(String tn) {
		wcpAttrs[tornadoWatchIndx].setLabel( tn );// thunderstormName;
	}

	@XmlElement(name="tornadoEnable")
	public boolean getTornadoEnable() {
		return wcpAttrs[tornadoWatchIndx].isEnable();
	}

	public void setTornadoEnable(boolean te) {
		wcpAttrs[tornadoWatchIndx].setEnable(te);
	}

	@XmlElement(name="tornadoColor")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getTornadoColor() {
		return wcpAttrs[tornadoWatchIndx].getColor();
	}

	public void setTornadoColor(RGB tc) {
		wcpAttrs[tornadoWatchIndx].setColor(tc);
	}
	
	@XmlElement(name="watch0Name")
	public String getWatch0Name() {
		return wcpAttrs[watch0Indx].getLabel();
	}

	public void setWatch0Name(String n) {
		wcpAttrs[watch0Indx].setLabel( n );
	}

	@XmlElement(name="watch0Enable")
	public boolean getWatch0Enable() {
		return wcpAttrs[watch0Indx].isEnable();
	}

	public void setWatch0Enable(boolean e) {
		wcpAttrs[watch0Indx].setEnable(e);
	}

	@XmlElement(name="watch0Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch0Color() {
		return wcpAttrs[watch0Indx].getColor();
	}

	public void setWatch0Color(RGB c) {
		wcpAttrs[watch0Indx].setColor(c);
	}
	
	@XmlElement(name="watch1Name")
	public String getWatch1Name() {
		return wcpAttrs[watch1Indx].getLabel();
	}

	public void setWatch1Name(String n) {
		wcpAttrs[watch1Indx].setLabel( n );
	}

	@XmlElement(name="watch1Enable")
	public boolean getWatch1Enable() {
		return wcpAttrs[watch1Indx].isEnable();
	}

	public void setWatch1Enable(boolean e) {
		wcpAttrs[watch1Indx].setEnable(e);
	}

	@XmlElement(name="watch1Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch1Color() {
		return wcpAttrs[watch1Indx].getColor();
	}

	public void setWatch1Color(RGB c) {
		wcpAttrs[watch1Indx].setColor(c);
	}
	
	@XmlElement(name="watch2Name")
	public String getWatch2Name() {
		return wcpAttrs[watch2Indx].getLabel();
	}

	public void setWatch2Name(String n) {
		wcpAttrs[watch2Indx].setLabel( n );
	}

	@XmlElement(name="watch2Enable")
	public boolean getWatch2Enable() {
		return wcpAttrs[watch2Indx].isEnable();
	}

	public void setWatch2Enable(boolean e) {
		wcpAttrs[watch2Indx].setEnable(e);
	}

	@XmlElement(name="watch2Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch2Color() {
		return wcpAttrs[watch2Indx].getColor();
	}

	public void setWatch2Color(RGB c) {
		wcpAttrs[watch2Indx].setColor(c);
	}
	
	@XmlElement(name="watch3Name")
	public String getWatch3Name() {
		return wcpAttrs[watch3Indx].getLabel();
	}

	public void setWatch3Name(String n) {
		wcpAttrs[watch3Indx].setLabel( n );
	}

	@XmlElement(name="watch3Enable")
	public boolean getWatch3Enable() {
		return wcpAttrs[watch3Indx].isEnable();
	}

	public void setWatch3Enable(boolean e) {
		wcpAttrs[watch3Indx].setEnable(e);
	}

	@XmlElement(name="watch3Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch3Color() {
		return wcpAttrs[watch3Indx].getColor();
	}

	public void setWatch3Color(RGB c) {
		wcpAttrs[watch3Indx].setColor(c);
	}
	
	@XmlElement(name="watch4Name")
	public String getWatch4Name() {
		return wcpAttrs[watch4Indx].getLabel();
	}

	public void setWatch4Name(String n) {
		wcpAttrs[watch4Indx].setLabel( n );
	}

	@XmlElement(name="watch4Enable")
	public boolean getWatch4Enable() {
		return wcpAttrs[watch4Indx].isEnable();
	}

	public void setWatch4Enable(boolean e) {
		wcpAttrs[watch4Indx].setEnable(e);
	}

	@XmlElement(name="watch4Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch4Color() {
		return wcpAttrs[watch4Indx].getColor();
	}

	public void setWatch4Color(RGB c) {
		wcpAttrs[watch4Indx].setColor(c);
	}
	
	@XmlElement(name="watch5Name")
	public String getWatch5Name() {
		return wcpAttrs[watch5Indx].getLabel();
	}

	public void setWatch5Name(String n) {
		wcpAttrs[watch5Indx].setLabel( n );
	}

	@XmlElement(name="watch5Enable")
	public boolean getWatch5Enable() {
		return wcpAttrs[watch5Indx].isEnable();
	}

	public void setWatch5Enable(boolean e) {
		wcpAttrs[watch5Indx].setEnable(e);
	}

	@XmlElement(name="watch5Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch5Color() {
		return wcpAttrs[watch5Indx].getColor();
	}

	public void setWatch5Color(RGB c) {
		wcpAttrs[watch5Indx].setColor(c);
	}
	
	@XmlElement(name="watch6Name")
	public String getWatch6Name() {
		return wcpAttrs[watch6Indx].getLabel();
	}

	public void setWatch6Name(String n) {
		wcpAttrs[watch6Indx].setLabel( n );
	}

	@XmlElement(name="watch6Enable")
	public boolean getWatch6Enable() {
		return wcpAttrs[watch6Indx].isEnable();
	}

	public void setWatch6Enable(boolean e) {
		wcpAttrs[watch6Indx].setEnable(e);
	}

	@XmlElement(name="watch6Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch6Color() {
		return wcpAttrs[watch6Indx].getColor();
	}

	public void setWatch6Color(RGB c) {
		wcpAttrs[watch6Indx].setColor(c);
	}
	
	@XmlElement(name="watch7Name")
	public String getWatch7Name() {
		return wcpAttrs[watch7Indx].getLabel();
	}

	public void setWatch7Name(String n) {
		wcpAttrs[watch7Indx].setLabel( n );
	}

	@XmlElement(name="watch7Enable")
	public boolean getWatch7Enable() {
		return wcpAttrs[watch7Indx].isEnable();
	}

	public void setWatch7Enable(boolean e) {
		wcpAttrs[watch7Indx].setEnable(e);
	}

	@XmlElement(name="watch7Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch7Color() {
		return wcpAttrs[watch7Indx].getColor();
	}

	public void setWatch7Color(RGB c) {
		wcpAttrs[watch7Indx].setColor(c);
	}
	
	@XmlElement(name="watch8Name")
	public String getWatch8Name() {
		return wcpAttrs[watch8Indx].getLabel();
	}

	public void setWatch8Name(String n) {
		wcpAttrs[watch8Indx].setLabel( n );
	}

	@XmlElement(name="watch8Enable")
	public boolean getWatch8Enable() {
		return wcpAttrs[watch8Indx].isEnable();
	}

	public void setWatch8Enable(boolean e) {
		wcpAttrs[watch8Indx].setEnable(e);
	}

	@XmlElement(name="watch8Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch8Color() {
		return wcpAttrs[watch8Indx].getColor();
	}

	public void setWatch8Color(RGB c) {
		wcpAttrs[watch8Indx].setColor(c);
	}
	
	@XmlElement(name="watch9Name")
	public String getWatch9Name() {
		return wcpAttrs[watch9Indx].getLabel();
	}

	public void setWatch9Name(String n) {
		wcpAttrs[watch9Indx].setLabel( n );
	}

	@XmlElement(name="watch9Enable")
	public boolean getWatch9Enable() {
		return wcpAttrs[watch9Indx].isEnable();
	}

	public void setWatch9Enable(boolean e) {
		wcpAttrs[watch9Indx].setEnable(e);
	}

	@XmlElement(name="watch9Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getWatch9Color() {
		return wcpAttrs[watch9Indx].getColor();
	}

	public void setWatch9Color(RGB c) {
		wcpAttrs[watch9Indx].setColor(c);
	}
	
	public String getWatchNumberLabelName() {
		return watchNumberLabelName;
	}

	public void setWatchNumberLabelName(String watchNumberLabelName) {
		this.watchNumberLabelName = watchNumberLabelName;
	}

	public boolean getWatchNumberLabelEnable() {
		return watchNumberLabelEnable;
	}

	public void setWatchNumberLabelEnable(boolean watchNumberLabelEnable) {
		this.watchNumberLabelEnable = watchNumberLabelEnable;
	}

	public String getColorCodeName() {
		return colorCodeName;
	}

	public void setColorCodeName(String colorCodeName) {
		this.colorCodeName = colorCodeName;
	}

	public boolean getColorCodeEnable() {
		return colorCodeEnable;
	}

	public void setColorCodeEnable(boolean colorCodeEnable) {
		this.colorCodeEnable = colorCodeEnable;
	}

	public String getTimeLabelName() {
		return timeLabelName;
	}

	public void setTimeLabelName(String timeLabelName) {
		this.timeLabelName = timeLabelName;
	}

	public boolean getTimeLabelEnable() {
		return timeLabelEnable;
	}

	public void setTimeLabelEnable(boolean timeLabelEnable) {
		this.timeLabelEnable = timeLabelEnable;
	}

	// create an array of MiscRscAttrs used to create the Edit Attributes GUI. The values in this 
	// array will point to the values from the rscAttrSet 
	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 2 );
		
		for( WCPAttr wcpAttr : wcpAttrs ) {
			attrs.addAttr( new MiscResourceAttr( wcpAttr.getEnabledAttrName(),
					wcpAttr.getLabel(), EditElement.CHECK_BOX, 1 ));			
			attrs.addAttr( new MiscResourceAttr( wcpAttr.getColorAttrName(),
					"", EditElement.COLOR_SELECTOR, 2 ));
		}	
		
		attrs.addAttr( new MiscResourceAttr( null, null, EditElement.SEPARATOR, 1 ));
				
		attrs.addAttr( new MiscResourceAttr( "timeLabelEnable", 
				timeLabelName, EditElement.CHECK_BOX, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "watchNumberLabelEnable",
				watchNumberLabelName, EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "colorCodeEnable",
				colorCodeName, EditElement.CHECK_BOX, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "lineWidth", "Line Width",
				EditElement.SPINNER, 1 ));

		return attrs;
	}

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new WCPResource( this, loadProperties );
	}
}	
