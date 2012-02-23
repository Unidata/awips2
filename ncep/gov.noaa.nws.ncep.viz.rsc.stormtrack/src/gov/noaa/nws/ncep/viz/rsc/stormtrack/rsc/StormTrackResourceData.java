/**
 * StormTrackResourceData
 * Date created: October 11, 2011
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscRscAttrs;
/**
 * <pre>
 * StormTrackResourceData - Creates and updates the elements of the Storm Track edit dialog. 
 * 
  * SOFTWARE HISTORY
 *    Date                Ticket#     Engineer         Description
 * ------------          ------------   ------------- --------------------------
 * 11-Oct-2011        ---        sgilbert          Initial creation.
 * 25 Oct 2011                   bhebbard          Add TD/Gale/TS/Hurricane attributes
 * 
 * </pre>
 * @author sgilbert
 *
 */
@SuppressWarnings("unused")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "StormTrackResourceData")
public class StormTrackResourceData  extends	AbstractNatlCntrsRequestableResourceData implements IMiscResourceData,
INatlCntrsResourceData{

	@XmlElement
	protected String legendName;
	
//	--------Trop Depression attributes-----------------------------------
	@XmlElement
	protected boolean tropDepressionEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB tropDepressionColor;

	@XmlElement
	protected float tropDepressionUpperLimit;
	
//	--------Gale attributes-----------------------------------
	@XmlElement
	protected boolean galeEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB galeColor;

	@XmlElement
	protected float galeUpperLimit;
	
//	--------Trop Storm attributes-----------------------------------
	@XmlElement
	protected boolean tropStormEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB tropStormColor;

	@XmlElement
	protected float tropStormUpperLimit;
	
//	--------Hurricane attributes-----------------------------------
	@XmlElement
	protected boolean hurricaneEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB hurricaneColor;

	@XmlElement
	protected float hurricaneUpperLimit;
	
//	--------GFSO attributes-----------------------------------
	@XmlElement
	protected boolean gfsoEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB gfsoColor;

	@XmlElement
	protected int gfsoLineWidth;

	@XmlElement
	protected int gfsoSymbolWidth;

	@XmlElement
	protected float gfsoSymbolSize;
	
	
//---------------NAM Attributes-------------------
	
	@XmlElement
	protected boolean namEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB namColor;

	@XmlElement
	protected int namLineWidth;

	@XmlElement
	protected int namSymbolWidth;

	@XmlElement
	protected float namSymbolSize;
	
//------------------UKX---------------------
	@XmlElement
	protected boolean ukxEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ukxColor;

	@XmlElement
	protected int ukxLineWidth;

	@XmlElement
	protected int ukxSymbolWidth;

	@XmlElement
	protected float ukxSymbolSize;
	
//-------------NGX-----------------------
	@XmlElement
	protected boolean ngxEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ngxColor;

	@XmlElement
	protected int ngxLineWidth;

	@XmlElement
	protected int ngxSymbolWidth;

	@XmlElement
	protected float ngxSymbolSize;
	
//----------------EC00-------------------
	@XmlElement
	protected boolean ec00Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ec00Color;

	@XmlElement
	protected int ec00LineWidth;

	@XmlElement
	protected int ec00SymbolWidth;

	@XmlElement
	protected float ec00SymbolSize;
	//----------------EP01----------------------
	@XmlElement
	protected boolean ep01Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep01Color;

	@XmlElement
	protected int ep01LineWidth;

	@XmlElement
	protected int ep01SymbolWidth;

	@XmlElement
	protected float ep01SymbolSize;
	//----------------EP02------------------
	@XmlElement
	protected boolean ep02Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep02Color;

	@XmlElement
	protected int ep02LineWidth;

	@XmlElement
	protected int ep02SymbolWidth;

	@XmlElement
	protected float ep02SymbolSize;	
	
	//-----------EP03----------------
	@XmlElement
	protected boolean ep03Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep03Color;

	@XmlElement
	protected int ep03LineWidth;

	@XmlElement
	protected int ep03SymbolWidth;

	@XmlElement
	protected float ep03SymbolSize;
	
	//------------EP04----------------
	@XmlElement
	protected boolean ep04Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep04Color;

	@XmlElement
	protected int ep04LineWidth;

	@XmlElement
	protected int ep04SymbolWidth;

	@XmlElement
	protected float ep04SymbolSize;
	
	//-------------EP05---------------
	@XmlElement
	protected boolean ep05Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep05Color;

	@XmlElement
	protected int ep05LineWidth;

	@XmlElement
	protected int ep05SymbolWidth;

	@XmlElement
	protected float ep05SymbolSize;
	
	//-------------EP06--------------
	@XmlElement
	protected boolean ep06Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep06Color;

	@XmlElement
	protected int ep06LineWidth;

	@XmlElement
	protected int ep06SymbolWidth;

	@XmlElement
	protected float ep06SymbolSize;
	
	//------------------EP07------------------
	@XmlElement
	protected boolean ep07Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep07Color;

	@XmlElement
	protected int ep07LineWidth;

	@XmlElement
	protected int ep07SymbolWidth;

	@XmlElement
	protected float ep07SymbolSize;
	
	//-----------------EP08-------------------
	@XmlElement
	protected boolean ep08Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep08Color;

	@XmlElement
	protected int ep08LineWidth;

	@XmlElement
	protected int ep08SymbolWidth;

	@XmlElement
	protected float ep08SymbolSize;
	
	//-----------------EP09-------------------
	@XmlElement
	protected boolean ep09Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep09Color;

	@XmlElement
	protected int ep09LineWidth;

	@XmlElement
	protected int ep09SymbolWidth;

	@XmlElement
	protected float ep09SymbolSize;
	
	//-----------------EP10-------------------
	@XmlElement
	protected boolean ep10Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep10Color;

	@XmlElement
	protected int ep10LineWidth;

	@XmlElement
	protected int ep10SymbolWidth;

	@XmlElement
	protected float ep10SymbolSize;
	
	//----------------EP11-------------------
	@XmlElement
	protected boolean ep11Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep11Color;

	@XmlElement
	protected int ep11LineWidth;

	@XmlElement
	protected int ep11SymbolWidth;

	@XmlElement
	protected float ep11SymbolSize;
	
	//-----------------EP12-----------------
	@XmlElement
	protected boolean ep12Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep12Color;

	@XmlElement
	protected int ep12LineWidth;

	@XmlElement
	protected int ep12SymbolWidth;

	@XmlElement
	protected float ep12SymbolSize;
	
	//-----------------EP13------------------
	@XmlElement
	protected boolean ep13Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep13Color;

	@XmlElement
	protected int ep13LineWidth;

	@XmlElement
	protected int ep13SymbolWidth;

	@XmlElement
	protected float ep13SymbolSize;
	
	//-----------------EP14-------------------
	@XmlElement
	protected boolean ep14Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ep14Color;

	@XmlElement
	protected int ep14LineWidth;

	@XmlElement
	protected int ep14SymbolWidth;

	@XmlElement
	protected float ep14SymbolSize;
	
	//------------------------------------------------------
	
	@XmlElement
	protected boolean dateTimeEnable;

	@XmlElement
	protected boolean pressureEnable;
	
	@XmlElement
	protected boolean colorCodeEnable; 

	@XmlElement
	protected boolean markerEnable;

    /**
	 * Default Constructor
	 * 
	 * @throws VizException
	 */
	public StormTrackResourceData() throws VizException {
		super();
//		System.out.println("Default StormTrackResource constructor - after invoking base class constructor");
		this.nameGenerator = new AbstractNameGenerator() {
			@Override
			public String getName(AbstractVizResource<?, ?> resource) {
				if (legendName != null) {
					return legendName;
				}
				return "Storm Track";
			}
		};
	}	
	
//	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			PluginDataObject[] objects){
		StormTrackResource thisResource = new StormTrackResource(this,loadProperties);
//		System.out.println("StormTrackResourceData constructor - after creating StormTrackResource Object");
		return thisResource;
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs(11);

		/*
		attrs.addAttr(new MiscResourceAttr("tropDepressionEnable",
				"Trop Depression", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropDepressionColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("tropDepressionUpperLimit",
				"Upper Limit (kt)", EditElement.SPINNER, 3));

		attrs.addAttr(new MiscResourceAttr("galeEnable",
				"Gale", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("galeColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("galeUpperLimit",
				"Upper Limit (kt)", EditElement.SPINNER, 3));

		attrs.addAttr(new MiscResourceAttr("tropStormEnable",
				"Trop Storm", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropStormColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("tropStormUpperLimit",
				"Upper Limit (kt)", EditElement.SPINNER, 3));

		attrs.addAttr(new MiscResourceAttr("hurricaneEnable",
				"Hurricane", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("hurricaneColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("hurricaneUpperLimit",
				"Upper Limit (kt)", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr(null, null,
				EditElement.SEPARATOR, 1));	
*/		

		attrs.addAttr(new MiscResourceAttr("gfsoEnable",
				"GFSO", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("gfsoColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("gfsoLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("gfsoSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("gfsoSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));
		attrs.addAttr(new MiscResourceAttr("ep08Enable",
				"EP08", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ep08Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ep08LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ep08SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ep08SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
		
		attrs.addAttr(new MiscResourceAttr("namEnable",
				"NAM", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("namColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("namLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("namSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("namSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));
		attrs.addAttr(new MiscResourceAttr("ep09Enable",
				"EP09", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ep09Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ep09LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ep09SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ep09SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
		
		attrs.addAttr(new MiscResourceAttr("ukxEnable",
				"UKX", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ukxColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ukxLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ukxSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ukxSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));
		attrs.addAttr(new MiscResourceAttr("ep10Enable",
				"EP10", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ep10Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ep10LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ep10SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ep10SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));

		
		attrs.addAttr(new MiscResourceAttr("ngxEnable",
				"NGX", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ngxColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ngxLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ngxSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ngxSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));
		attrs.addAttr(new MiscResourceAttr("ep11Enable",
				"EP11", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ep11Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ep11LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ep11SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ep11SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
		
		attrs.addAttr(new MiscResourceAttr("ec00Enable",
				"EC00", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ec00Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ec00LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ec00SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ec00SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));
		attrs.addAttr(new MiscResourceAttr("ep12Enable",
				"EP12", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ep12Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ep12LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ep12SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ep12SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
	
		
		attrs.addAttr(new MiscResourceAttr("ep01Enable",
				"EP01", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ep01Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ep01LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ep01SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ep01SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));
		attrs.addAttr(new MiscResourceAttr("ep13Enable",
				"EP13", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ep13Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ep13LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ep13SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ep13SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
		
		attrs.addAttr(new MiscResourceAttr("ep02Enable",
				"EP02", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ep02Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ep02LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ep02SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ep02SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));
		attrs.addAttr(new MiscResourceAttr("ep14Enable",
				"EP14", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ep14Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ep14LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ep14SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ep14SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
		
		attrs.addAttr(new MiscResourceAttr("ep03Enable",
				"EP03", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ep03Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ep03LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ep03SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ep03SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr("ep04Enable",
				"EP04", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ep04Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ep04LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ep04SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ep04SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr("ep05Enable",
				"EP05", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ep05Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ep05LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ep05SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ep05SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr("ep06Enable",
				"EP06", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ep06Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ep06LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ep06SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ep06SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr("ep07Enable",
				"EP07", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ep07Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ep07LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ep07SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ep07SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null, null,
				EditElement.SEPARATOR, 1));	
		
		attrs.addAttr(new MiscResourceAttr("dateTimeEnable", "Date/Time",
				EditElement.CHECK_BOX, 1));

		attrs.addAttr(new MiscResourceAttr("pressureEnable", "Pressure",
				EditElement.CHECK_BOX, 1));	
		
		attrs.addAttr(new MiscResourceAttr("markerEnable", "Marker",
				EditElement.CHECK_BOX, 1));			

		attrs.addAttr(new MiscResourceAttr("colorCodeEnable", "ColorCode",
				EditElement.CHECK_BOX, 1));

		return attrs;
	}

   public String getLegendName() {
		return legendName;
	}

	public void setLegendName(String legendName) {
		this.legendName = legendName;
	}

	public boolean isTropDepressionEnable() {
		return tropDepressionEnable;
	}

	public void setTropDepressionEnable(boolean tropDepressionEnable) {
		this.tropDepressionEnable = tropDepressionEnable;
	}

	public RGB getTropDepressionColor() {
		return tropDepressionColor;
	}

	public void setTropDepressionColor(RGB tropDepressionColor) {
		this.tropDepressionColor = tropDepressionColor;
	}

	public float getTropDepressionUpperLimit() {
		return tropDepressionUpperLimit;
	}

	public void setTropDepressionUpperLimit(float tropDepressionUpperLimit) {
		this.tropDepressionUpperLimit = tropDepressionUpperLimit;
	}

	public boolean isGaleEnable() {
		return galeEnable;
	}

	public void setGaleEnable(boolean galeEnable) {
		this.galeEnable = galeEnable;
	}

	public RGB getGaleColor() {
		return galeColor;
	}

	public void setGaleColor(RGB galeColor) {
		this.galeColor = galeColor;
	}

	public float getGaleUpperLimit() {
		return galeUpperLimit;
	}

	public void setGaleUpperLimit(float galeUpperLimit) {
		this.galeUpperLimit = galeUpperLimit;
	}

	public boolean isTropStormEnable() {
		return tropStormEnable;
	}

	public void setTropStormEnable(boolean tropStormEnable) {
		this.tropStormEnable = tropStormEnable;
	}

	public RGB getTropStormColor() {
		return tropStormColor;
	}

	public void setTropStormColor(RGB tropStormColor) {
		this.tropStormColor = tropStormColor;
	}

	public float getTropStormUpperLimit() {
		return tropStormUpperLimit;
	}

	public void setTropStormUpperLimit(float tropStormUpperLimit) {
		this.tropStormUpperLimit = tropStormUpperLimit;
	}

	public boolean isHurricaneEnable() {
		return hurricaneEnable;
	}

	public void setHurricaneEnable(boolean hurricaneEnable) {
		this.hurricaneEnable = hurricaneEnable;
	}

	public RGB getHurricaneColor() {
		return hurricaneColor;
	}

	public void setHurricaneColor(RGB hurricaneColor) {
		this.hurricaneColor = hurricaneColor;
	}

	public float getHurricaneUpperLimit() {
		return hurricaneUpperLimit;
	}

	public void setHurricaneUpperLimit(float hurricaneUpperLimit) {
		this.hurricaneUpperLimit = hurricaneUpperLimit;
	}

	public boolean getGfsoEnable() {
		return gfsoEnable;
	}

	public void setGfsoEnable(boolean gfsoEnable) {
		this.gfsoEnable = gfsoEnable;
	}

	public RGB getGfsoColor() {
		return gfsoColor;
	}

	public void setGfsoColor(RGB gfsoColor) {
		this.gfsoColor = gfsoColor;
	}

	public int getGfsoLineWidth() {
		return gfsoLineWidth;
	}

	public void setGfsoLineWidth(int gfsoLineWidth) {
		this.gfsoLineWidth = gfsoLineWidth;
	}

	public int getGfsoSymbolWidth() {
		return gfsoSymbolWidth;
	}

	public void setGfsoSymbolWidth(int gfsoSymbolWidth) {
		this.gfsoSymbolWidth = gfsoSymbolWidth;
	}

	public float getGfsoSymbolSize() {
		return gfsoSymbolSize;
	}

	public void setGfsoSymbolSize(float gfsoSymbolSize) {
		this.gfsoSymbolSize = gfsoSymbolSize;
	}

	public boolean getNamEnable() {
		return namEnable;
	}

	public void setNamEnable(boolean namEnable) {
		this.namEnable = namEnable;
	}

	public RGB getNamColor() {
		return namColor;
	}

	public void setNamColor(RGB namColor) {
		this.namColor = namColor;
	}

	public int getNamLineWidth() {
		return namLineWidth;
	}

	public void setNamLineWidth(int namLineWidth) {
		this.namLineWidth = namLineWidth;
	}

	public int getNamSymbolWidth() {
		return namSymbolWidth;
	}

	public void setNamSymbolWidth(int namSymbolWidth) {
		this.namSymbolWidth = namSymbolWidth;
	}

	public float getNamSymbolSize() {
		return namSymbolSize;
	}

	public void setNamSymbolSize(float namSymbolSize) {
		this.namSymbolSize = namSymbolSize;
	}

	public boolean getUkxEnable() {
		return ukxEnable;
	}

	public void setUkxEnable(boolean ukxEnable) {
		this.ukxEnable = ukxEnable;
	}

	public RGB getUkxColor() {
		return ukxColor;
	}

	public void setUkxColor(RGB ukxColor) {
		this.ukxColor = ukxColor;
	}

	public int getUkxLineWidth() {
		return ukxLineWidth;
	}

	public void setUkxLineWidth(int ukxLineWidth) {
		this.ukxLineWidth = ukxLineWidth;
	}

	public int getUkxSymbolWidth() {
		return ukxSymbolWidth;
	}

	public void setUkxSymbolWidth(int ukxSymbolWidth) {
		this.ukxSymbolWidth = ukxSymbolWidth;
	}

	public float getUkxSymbolSize() {
		return ukxSymbolSize;
	}

	public void setUkxSymbolSize(float ukxSymbolSize) {
		this.ukxSymbolSize = ukxSymbolSize;
	}

	public boolean getNgxEnable() {
		return ngxEnable;
	}

	public void setNgxEnable(boolean ngxEnable) {
		this.ngxEnable = ngxEnable;
	}

	public RGB getNgxColor() {
		return ngxColor;
	}

	public void setNgxColor(RGB ngxColor) {
		this.ngxColor = ngxColor;
	}

	public int getNgxLineWidth() {
		return ngxLineWidth;
	}

	public void setNgxLineWidth(int ngxLineWidth) {
		this.ngxLineWidth = ngxLineWidth;
	}

	public int getNgxSymbolWidth() {
		return ngxSymbolWidth;
	}

	public void setNgxSymbolWidth(int ngxSymbolWidth) {
		this.ngxSymbolWidth = ngxSymbolWidth;
	}

	public float getNgxSymbolSize() {
		return ngxSymbolSize;
	}

	public void setNgxSymbolSize(float ngxSymbolSize) {
		this.ngxSymbolSize = ngxSymbolSize;
	}

	public boolean getEc00Enable() {
		return ec00Enable;
	}

	public void setEc00Enable(boolean ec00Enable) {
		this.ec00Enable = ec00Enable;
	}

	public RGB getEc00Color() {
		return ec00Color;
	}

	public void setEc00Color(RGB ec00Color) {
		this.ec00Color = ec00Color;
	}

	public int getEc00LineWidth() {
		return ec00LineWidth;
	}

	public void setEc00LineWidth(int ec00LineWidth) {
		this.ec00LineWidth = ec00LineWidth;
	}

	public int getEc00SymbolWidth() {
		return ec00SymbolWidth;
	}

	public void setEc00SymbolWidth(int ec00SymbolWidth) {
		this.ec00SymbolWidth = ec00SymbolWidth;
	}

	public float getEc00SymbolSize() {
		return ec00SymbolSize;
	}

	public void setEc00SymbolSize(float ec00SymbolSize) {
		this.ec00SymbolSize = ec00SymbolSize;
	}

	public boolean getEp01Enable() {
		return ep01Enable;
	}

	public void setEp01Enable(boolean ep01Enable) {
		this.ep01Enable = ep01Enable;
	}

	public RGB getEp01Color() {
		return ep01Color;
	}

	public void setEp01Color(RGB ep01Color) {
		this.ep01Color = ep01Color;
	}

	public int getEp01LineWidth() {
		return ep01LineWidth;
	}

	public void setEp01LineWidth(int ep01LineWidth) {
		this.ep01LineWidth = ep01LineWidth;
	}

	public int getEp01SymbolWidth() {
		return ep01SymbolWidth;
	}

	public void setEp01SymbolWidth(int ep01SymbolWidth) {
		this.ep01SymbolWidth = ep01SymbolWidth;
	}

	public float getEp01SymbolSize() {
		return ep01SymbolSize;
	}

	public void setEp01SymbolSize(float ep01SymbolSize) {
		this.ep01SymbolSize = ep01SymbolSize;
	}

	public boolean getEp02Enable() {
		return ep02Enable;
	}

	public void setEp02Enable(boolean ep02Enable) {
		this.ep02Enable = ep02Enable;
	}

	public RGB getEp02Color() {
		return ep02Color;
	}

	public void setEp02Color(RGB ep02Color) {
		this.ep02Color = ep02Color;
	}

	public int getEp02LineWidth() {
		return ep02LineWidth;
	}

	public void setEp02LineWidth(int ep02LineWidth) {
		this.ep02LineWidth = ep02LineWidth;
	}

	public int getEp02SymbolWidth() {
		return ep02SymbolWidth;
	}

	public void setEp02SymbolWidth(int ep02SymbolWidth) {
		this.ep02SymbolWidth = ep02SymbolWidth;
	}

	public float getEp02SymbolSize() {
		return ep02SymbolSize;
	}

	public void setEp02SymbolSize(float ep02SymbolSize) {
		this.ep02SymbolSize = ep02SymbolSize;
	}

	public boolean getEp03Enable() {
		return ep03Enable;
	}

	public void setEp03Enable(boolean ep03Enable) {
		this.ep03Enable = ep03Enable;
	}

	public RGB getEp03Color() {
		return ep03Color;
	}

	public void setEp03Color(RGB ep03Color) {
		this.ep03Color = ep03Color;
	}

	public int getEp03LineWidth() {
		return ep03LineWidth;
	}

	public void setEp03LineWidth(int ep03LineWidth) {
		this.ep03LineWidth = ep03LineWidth;
	}

	public int getEp03SymbolWidth() {
		return ep03SymbolWidth;
	}

	public void setEp03SymbolWidth(int ep03SymbolWidth) {
		this.ep03SymbolWidth = ep03SymbolWidth;
	}

	public float getEp03SymbolSize() {
		return ep03SymbolSize;
	}

	public void setEp03SymbolSize(float ep03SymbolSize) {
		this.ep03SymbolSize = ep03SymbolSize;
	}

	public boolean getEp04Enable() {
		return ep04Enable;
	}

	public void setEp04Enable(boolean ep04Enable) {
		this.ep04Enable = ep04Enable;
	}

	public RGB getEp04Color() {
		return ep04Color;
	}

	public void setEp04Color(RGB ep04Color) {
		this.ep04Color = ep04Color;
	}

	public int getEp04LineWidth() {
		return ep04LineWidth;
	}

	public void setEp04LineWidth(int ep04LineWidth) {
		this.ep04LineWidth = ep04LineWidth;
	}

	public int getEp04SymbolWidth() {
		return ep04SymbolWidth;
	}

	public void setEp04SymbolWidth(int ep04SymbolWidth) {
		this.ep04SymbolWidth = ep04SymbolWidth;
	}

	public float getEp04SymbolSize() {
		return ep04SymbolSize;
	}

	public void setEp04SymbolSize(float ep04SymbolSize) {
		this.ep04SymbolSize = ep04SymbolSize;
	}

	public boolean getEp05Enable() {
		return ep05Enable;
	}

	public void setEp05Enable(boolean ep05Enable) {
		this.ep05Enable = ep05Enable;
	}

	public RGB getEp05Color() {
		return ep05Color;
	}

	public void setEp05Color(RGB ep05Color) {
		this.ep05Color = ep05Color;
	}

	public int getEp05LineWidth() {
		return ep05LineWidth;
	}

	public void setEp05LineWidth(int ep05LineWidth) {
		this.ep05LineWidth = ep05LineWidth;
	}

	public int getEp05SymbolWidth() {
		return ep05SymbolWidth;
	}

	public void setEp05SymbolWidth(int ep05SymbolWidth) {
		this.ep05SymbolWidth = ep05SymbolWidth;
	}

	public float getEp05SymbolSize() {
		return ep05SymbolSize;
	}

	public void setEp05SymbolSize(float ep05SymbolSize) {
		this.ep05SymbolSize = ep05SymbolSize;
	}

	public boolean getEp06Enable() {
		return ep06Enable;
	}

	public void setEp06Enable(boolean ep06Enable) {
		this.ep06Enable = ep06Enable;
	}

	public RGB getEp06Color() {
		return ep06Color;
	}

	public void setEp06Color(RGB ep06Color) {
		this.ep06Color = ep06Color;
	}

	public int getEp06LineWidth() {
		return ep06LineWidth;
	}

	public void setEp06LineWidth(int ep06LineWidth) {
		this.ep06LineWidth = ep06LineWidth;
	}

	public int getEp06SymbolWidth() {
		return ep06SymbolWidth;
	}

	public void setEp06SymbolWidth(int ep06SymbolWidth) {
		this.ep06SymbolWidth = ep06SymbolWidth;
	}

	public float getEp06SymbolSize() {
		return ep06SymbolSize;
	}

	public void setEp06SymbolSize(float ep06SymbolSize) {
		this.ep06SymbolSize = ep06SymbolSize;
	}

	public boolean getEp07Enable() {
		return ep07Enable;
	}

	public void setEp07Enable(boolean ep07Enable) {
		this.ep07Enable = ep07Enable;
	}

	public RGB getEp07Color() {
		return ep07Color;
	}

	public void setEp07Color(RGB ep07Color) {
		this.ep07Color = ep07Color;
	}

	public int getEp07LineWidth() {
		return ep07LineWidth;
	}

	public void setEp07LineWidth(int ep07LineWidth) {
		this.ep07LineWidth = ep07LineWidth;
	}

	public int getEp07SymbolWidth() {
		return ep07SymbolWidth;
	}

	public void setEp07SymbolWidth(int ep07SymbolWidth) {
		this.ep07SymbolWidth = ep07SymbolWidth;
	}

	public float getEp07SymbolSize() {
		return ep07SymbolSize;
	}

	public void setEp07SymbolSize(float ep07SymbolSize) {
		this.ep07SymbolSize = ep07SymbolSize;
	}

	public boolean getEp08Enable() {
		return ep08Enable;
	}

	public void setEp08Enable(boolean ep08Enable) {
		this.ep08Enable = ep08Enable;
	}

	public RGB getEp08Color() {
		return ep08Color;
	}

	public void setEp08Color(RGB ep08Color) {
		this.ep08Color = ep08Color;
	}

	public int getEp08LineWidth() {
		return ep08LineWidth;
	}

	public void setEp08LineWidth(int ep08LineWidth) {
		this.ep08LineWidth = ep08LineWidth;
	}

	public int getEp08SymbolWidth() {
		return ep08SymbolWidth;
	}

	public void setEp08SymbolWidth(int ep08SymbolWidth) {
		this.ep08SymbolWidth = ep08SymbolWidth;
	}

	public float getEp08SymbolSize() {
		return ep08SymbolSize;
	}

	public void setEp08SymbolSize(float ep08SymbolSize) {
		this.ep08SymbolSize = ep08SymbolSize;
	}

	public boolean getEp09Enable() {
		return ep09Enable;
	}

	public void setEp09Enable(boolean ep09Enable) {
		this.ep09Enable = ep09Enable;
	}

	public RGB getEp09Color() {
		return ep09Color;
	}

	public void setEp09Color(RGB ep09Color) {
		this.ep09Color = ep09Color;
	}

	public int getEp09LineWidth() {
		return ep09LineWidth;
	}

	public void setEp09LineWidth(int ep09LineWidth) {
		this.ep09LineWidth = ep09LineWidth;
	}

	public int getEp09SymbolWidth() {
		return ep09SymbolWidth;
	}

	public void setEp09SymbolWidth(int ep09SymbolWidth) {
		this.ep09SymbolWidth = ep09SymbolWidth;
	}

	public float getEp09SymbolSize() {
		return ep09SymbolSize;
	}

	public void setEp09SymbolSize(float ep09SymbolSize) {
		this.ep09SymbolSize = ep09SymbolSize;
	}

	public boolean getEp10Enable() {
		return ep10Enable;
	}

	public void setEp10Enable(boolean ep10Enable) {
		this.ep10Enable = ep10Enable;
	}

	public RGB getEp10Color() {
		return ep10Color;
	}

	public void setEp10Color(RGB ep10Color) {
		this.ep10Color = ep10Color;
	}

	public int getEp10LineWidth() {
		return ep10LineWidth;
	}

	public void setEp10LineWidth(int ep10LineWidth) {
		this.ep10LineWidth = ep10LineWidth;
	}

	public int getEp10SymbolWidth() {
		return ep10SymbolWidth;
	}

	public void setEp10SymbolWidth(int ep10SymbolWidth) {
		this.ep10SymbolWidth = ep10SymbolWidth;
	}

	public float getEp10SymbolSize() {
		return ep10SymbolSize;
	}

	public void setEp10SymbolSize(float ep10SymbolSize) {
		this.ep10SymbolSize = ep10SymbolSize;
	}

	public boolean getEp11Enable() {
		return ep11Enable;
	}

	public void setEp11Enable(boolean ep11Enable) {
		this.ep11Enable = ep11Enable;
	}

	public RGB getEp11Color() {
		return ep11Color;
	}

	public void setEp11Color(RGB ep11Color) {
		this.ep11Color = ep11Color;
	}

	public int getEp11LineWidth() {
		return ep11LineWidth;
	}

	public void setEp11LineWidth(int ep11LineWidth) {
		this.ep11LineWidth = ep11LineWidth;
	}

	public int getEp11SymbolWidth() {
		return ep11SymbolWidth;
	}

	public void setEp11SymbolWidth(int ep11SymbolWidth) {
		this.ep11SymbolWidth = ep11SymbolWidth;
	}

	public float getEp11SymbolSize() {
		return ep11SymbolSize;
	}

	public void setEp11SymbolSize(float ep11SymbolSize) {
		this.ep11SymbolSize = ep11SymbolSize;
	}

	public boolean getEp12Enable() {
		return ep12Enable;
	}

	public void setEp12Enable(boolean ep12Enable) {
		this.ep12Enable = ep12Enable;
	}

	public RGB getEp12Color() {
		return ep12Color;
	}

	public void setEp12Color(RGB ep12Color) {
		this.ep12Color = ep12Color;
	}

	public int getEp12LineWidth() {
		return ep12LineWidth;
	}

	public void setEp12LineWidth(int ep12LineWidth) {
		this.ep12LineWidth = ep12LineWidth;
	}

	public int getEp12SymbolWidth() {
		return ep12SymbolWidth;
	}

	public void setEp12SymbolWidth(int ep12SymbolWidth) {
		this.ep12SymbolWidth = ep12SymbolWidth;
	}

	public float getEp12SymbolSize() {
		return ep12SymbolSize;
	}

	public void setEp12SymbolSize(float ep12SymbolSize) {
		this.ep12SymbolSize = ep12SymbolSize;
	}

	public boolean getEp13Enable() {
		return ep13Enable;
	}

	public void setEp13Enable(boolean ep13Enable) {
		this.ep13Enable = ep13Enable;
	}

	public RGB getEp13Color() {
		return ep13Color;
	}

	public void setEp13Color(RGB ep13Color) {
		this.ep13Color = ep13Color;
	}

	public int getEp13LineWidth() {
		return ep13LineWidth;
	}

	public void setEp13LineWidth(int ep13LineWidth) {
		this.ep13LineWidth = ep13LineWidth;
	}

	public int getEp13SymbolWidth() {
		return ep13SymbolWidth;
	}

	public void setEp13SymbolWidth(int ep13SymbolWidth) {
		this.ep13SymbolWidth = ep13SymbolWidth;
	}

	public float getEp13SymbolSize() {
		return ep13SymbolSize;
	}

	public void setEp13SymbolSize(float ep13SymbolSize) {
		this.ep13SymbolSize = ep13SymbolSize;
	}

	public boolean getEp14Enable() {
		return ep14Enable;
	}

	public void setEp14Enable(boolean ep14Enable) {
		this.ep14Enable = ep14Enable;
	}

	public RGB getEp14Color() {
		return ep14Color;
	}

	public void setEp14Color(RGB ep14Color) {
		this.ep14Color = ep14Color;
	}

	public int getEp14LineWidth() {
		return ep14LineWidth;
	}

	public void setEp14LineWidth(int ep14LineWidth) {
		this.ep14LineWidth = ep14LineWidth;
	}

	public int getEp14SymbolWidth() {
		return ep14SymbolWidth;
	}

	public void setEp14SymbolWidth(int ep14SymbolWidth) {
		this.ep14SymbolWidth = ep14SymbolWidth;
	}

	public float getEp14SymbolSize() {
		return ep14SymbolSize;
	}

	public void setEp14SymbolSize(float ep14SymbolSize) {
		this.ep14SymbolSize = ep14SymbolSize;
	}

	public boolean getDateTimeEnable() {
		return dateTimeEnable;
	}

	public void setDateTimeEnable(boolean dateTimeEnable) {
		this.dateTimeEnable = dateTimeEnable;
	}

	public boolean getPressureEnable() {
		return pressureEnable;
	}

	public void setPressureEnable(boolean pressureEnable) {
		this.pressureEnable = pressureEnable;
	}

	public boolean getColorCodeEnable() {
		return colorCodeEnable;
	}

	public void setColorCodeEnable(boolean colorCodeEnable) {
		this.colorCodeEnable = colorCodeEnable;
	}

	public boolean getMarkerEnable() {
		return markerEnable;
	}

	public void setMarkerEnable(boolean markerEnable) {
		this.markerEnable = markerEnable;
	}

}
