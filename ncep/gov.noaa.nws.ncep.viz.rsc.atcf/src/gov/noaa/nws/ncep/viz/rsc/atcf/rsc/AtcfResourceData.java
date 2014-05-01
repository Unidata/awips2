/**
 * AtcfResourceData
 * Date created: August 9, 2010
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.atcf.rsc;

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
 * AtcfResourceData - Creates and updates the elements of the ATCF edit dialog. 
 * 
  * SOFTWARE HISTORY
 *    Date                Ticket#     Engineer         Description
 * ------------          ------------   ------------- --------------------------
 * 09-Aug-2010        284        Archana          Initial creation.
 * 
 * </pre>
 * @author archana
 *
 */
@SuppressWarnings("unused")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "AtcfResourceData")
public class AtcfResourceData  extends	AbstractNatlCntrsRequestableResourceData implements IMiscResourceData,
INatlCntrsResourceData{

	@XmlElement
	protected String legendName;
	
//	--------OHPC attributes-----------------------------------
	@XmlElement
	protected boolean ohpcEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ohpcColor;

	@XmlElement
	protected int ohpcLineWidth;

	@XmlElement
	protected int ohpcSymbolWidth;

	@XmlElement
	protected float ohpcSymbolSize;
	
	
//---------------CLIPAttributes-------------------
	
	@XmlElement
	protected boolean clipEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB clipColor;

	@XmlElement
	protected int clipLineWidth;

	@XmlElement
	protected int clipSymbolWidth;

	@XmlElement
	protected float clipSymbolSize;
	
//------------------AVNO---------------------
	@XmlElement
	protected boolean avnoEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB avnoColor;

	@XmlElement
	protected int avnoLineWidth;

	@XmlElement
	protected int avnoSymbolWidth;

	@XmlElement
	protected float avnoSymbolSize;
	
//-------------MRFO-----------------------
	@XmlElement
	protected boolean mrfoEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB mrfoColor;

	@XmlElement
	protected int mrfoLineWidth;

	@XmlElement
	protected int mrfoSymbolWidth;

	@XmlElement
	protected float mrfoSymbolSize;
	
//----------------UKM-------------------
	@XmlElement
	protected boolean ukmEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ukmColor;

	@XmlElement
	protected int ukmLineWidth;

	@XmlElement
	protected int ukmSymbolWidth;

	@XmlElement
	protected float ukmSymbolSize;
	//----------------AVNI----------------------
	@XmlElement
	protected boolean avniEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB avniColor;

	@XmlElement
	protected int avniLineWidth;

	@XmlElement
	protected int avniSymbolWidth;

	@XmlElement
	protected float avniSymbolSize;
	//----------------NAMI------------------
	@XmlElement
	protected boolean namiEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB namiColor;

	@XmlElement
	protected int namiLineWidth;

	@XmlElement
	protected int namiSymbolWidth;

	@XmlElement
	protected float namiSymbolSize;	
	
	//-----------EMX2----------------
	@XmlElement
	protected boolean emx2Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB emx2Color;

	@XmlElement
	protected int emx2LineWidth;

	@XmlElement
	protected int emx2SymbolWidth;

	@XmlElement
	protected float emx2SymbolSize;
	
	//------------EGRI----------------
	@XmlElement
	protected boolean egriEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB egriColor;

	@XmlElement
	protected int egriLineWidth;

	@XmlElement
	protected int egriSymbolWidth;

	@XmlElement
	protected float egriSymbolSize;
	
	//-------------NGPI---------------
	@XmlElement
	protected boolean ngpiEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ngpiColor;

	@XmlElement
	protected int ngpiLineWidth;

	@XmlElement
	protected int ngpiSymbolWidth;

	@XmlElement
	protected float ngpiSymbolSize;
	
	//-------------UKM2--------------
	@XmlElement
	protected boolean ukm2Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ukm2Color;

	@XmlElement
	protected int ukm2LineWidth;

	@XmlElement
	protected int ukm2SymbolWidth;

	@XmlElement
	protected float ukm2SymbolSize;
	
	//------------------HWFI------------------
	@XmlElement
	protected boolean hwfiEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB hwfiColor;

	@XmlElement
	protected int hwfiLineWidth;

	@XmlElement
	protected int hwfiSymbolWidth;

	@XmlElement
	protected float hwfiSymbolSize;
	
	//-----------------GHMI-------------------
	@XmlElement
	protected boolean ghmiEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ghmiColor;

	@XmlElement
	protected int ghmiLineWidth;

	@XmlElement
	protected int ghmiSymbolWidth;

	@XmlElement
	protected float ghmiSymbolSize;
	
	//-----------------GFNI-------------------
	@XmlElement
	protected boolean gfniEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB gfniColor;

	@XmlElement
	protected int gfniLineWidth;

	@XmlElement
	protected int gfniSymbolWidth;

	@XmlElement
	protected float gfniSymbolSize;
	
	//-----------------AEMI-------------------
	@XmlElement
	protected boolean aemiEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB aemiColor;

	@XmlElement
	protected int aemiLineWidth;

	@XmlElement
	protected int aemiSymbolWidth;

	@XmlElement
	protected float aemiSymbolSize;
	
	//----------------TCON-------------------
	@XmlElement
	protected boolean tconEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB tconColor;

	@XmlElement
	protected int tconLineWidth;

	@XmlElement
	protected int tconSymbolWidth;

	@XmlElement
	protected float tconSymbolSize;
	
	//-----------------GUNA-----------------
	@XmlElement
	protected boolean gunaEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB gunaColor;

	@XmlElement
	protected int gunaLineWidth;

	@XmlElement
	protected int gunaSymbolWidth;

	@XmlElement
	protected float gunaSymbolSize;
	
	//-----------------TVCN------------------
	@XmlElement
	protected boolean tvcnEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB tvcnColor;

	@XmlElement
	protected int tvcnLineWidth;

	@XmlElement
	protected int tvcnSymbolWidth;

	@XmlElement
	protected float tvcnSymbolSize;
	
	//-----------------EMXI-------------------
	@XmlElement
	protected boolean emxiEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB emxiColor;

	@XmlElement
	protected int emxiLineWidth;

	@XmlElement
	protected int emxiSymbolWidth;

	@XmlElement
	protected float emxiSymbolSize;
	
	//-------------FSSE----------------
	@XmlElement
	protected boolean fsseEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB fsseColor;

	@XmlElement
	protected int fsseLineWidth;

	@XmlElement
	protected int fsseSymbolWidth;

	@XmlElement
	protected float fsseSymbolSize;
	
	//--------------UKMI---------------
	@XmlElement
	protected boolean ukmiEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB ukmiColor;

	@XmlElement
	protected int ukmiLineWidth;

	@XmlElement
	protected int ukmiSymbolWidth;

	@XmlElement
	protected float ukmiSymbolSize;
	
	//-------------BAMM--------------
	
	@XmlElement
	protected boolean bammEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB bammColor;

	@XmlElement
	protected int bammLineWidth;

	@XmlElement
	protected int bammSymbolWidth;

	@XmlElement
	protected float bammSymbolSize;
	
	//-------------BAMD--------------
	
	@XmlElement
	protected boolean bamdEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB bamdColor;

	@XmlElement
	protected int bamdLineWidth;

	@XmlElement
	protected int bamdSymbolWidth;

	@XmlElement
	protected float bamdSymbolSize;
	
	//------------BAMS--------------
	
	@XmlElement
	protected boolean bamsEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB bamsColor;

	@XmlElement
	protected int bamsLineWidth;

	@XmlElement
	protected int bamsSymbolWidth;

	@XmlElement
	protected float bamsSymbolSize;
	
	//------------EGR2--------------
	
	@XmlElement
	protected boolean egr2Enable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB egr2Color;

	@XmlElement
	protected int egr2LineWidth;

	@XmlElement
	protected int egr2SymbolWidth;

	@XmlElement
	protected float egr2SymbolSize;
	
	//------------------------------------------------------
	
	@XmlElement
	protected boolean timeEnable;

	@XmlElement
	protected boolean nameEnable;
	
	@XmlElement
	protected boolean speedEnable; 

	@XmlElement
	protected boolean markerEnable;

    /**
	 * Default Constructor
	 * 
	 * @throws VizException
	 */
	public AtcfResourceData() throws VizException {
		super();
//		System.out.println("Default AtcfResource constructor - after invoking base class constructor");
		this.nameGenerator = new AbstractNameGenerator() {
			@Override
			public String getName(AbstractVizResource<?, ?> resource) {
				if (legendName != null) {
					return legendName;
				}
				return "ATCF";
			}
		};
	}	
	
//	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			PluginDataObject[] objects){
		AtcfResource thisAtcfResource = new AtcfResource(this,loadProperties);
//		System.out.println("AtcfResourceData constructor - after creating AtcfResource Object");
		return thisAtcfResource;
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs(11);

		attrs.addAttr(new MiscResourceAttr("ohpcEnable",
				"01OHPC", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ohpcColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ohpcLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ohpcSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ohpcSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("ghmiEnable",
				"13GHMI", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ghmiColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ghmiLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ghmiSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ghmiSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
		
		attrs.addAttr(new MiscResourceAttr("clipEnable",
				"02CLIP", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("clipColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("clipLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("clipSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("clipSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("gfniEnable",
				"14GFNI", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("gfniColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("gfniLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("gfniSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("gfniSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));
		
		attrs.addAttr(new MiscResourceAttr("avnoEnable",
				"03AVNO", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("avnoColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("avnoLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("avnoSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("avnoSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("aemiEnable",
				"15AEMI", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("aemiColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("aemiLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("aemiSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("aemiSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));		

		
		attrs.addAttr(new MiscResourceAttr("mrfoEnable",
				"04MRFO", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("mrfoColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("mrfoLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("mrfoSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("mrfoSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("tconEnable",
				"16TCON", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("tconColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("tconLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("tconSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("tconSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));		
		
		attrs.addAttr(new MiscResourceAttr("ukmEnable",
				"05 UKM", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ukmColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ukmLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ukmSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ukmSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("gunaEnable",
				"17GUNA", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("gunaColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("gunaLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("gunaSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("gunaSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));				
	
		
		attrs.addAttr(new MiscResourceAttr("avniEnable",
				"06AVNI", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("avniColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("avniLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("avniSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("avniSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("tvcnEnable",
				"18TVCN", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("tvcnColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("tvcnLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("tvcnSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("tvcnSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));			
		
		attrs.addAttr(new MiscResourceAttr("namiEnable",
				"07NAMI", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("namiColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("namiLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("namiSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("namiSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("emxiEnable",
				"19EMXI", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("emxiColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("emxiLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("emxiSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("emxiSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));			
		
		attrs.addAttr(new MiscResourceAttr("emx2Enable",
				"08EMX2", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("emx2Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("emx2LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("emx2SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("emx2SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("fsseEnable",
				"20FSSE", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("fsseColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("fsseLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("fsseSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("fsseSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));		
		
		attrs.addAttr(new MiscResourceAttr("egriEnable",
				"09EGRI", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("egriColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("egriLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("egriSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("egriSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("ukmiEnable",
				"21UKMI", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("ukmiColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("ukmiLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("ukmiSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("ukmiSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));				

		attrs.addAttr(new MiscResourceAttr("ngpiEnable",
				"10NGPI", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ngpiColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ngpiLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ngpiSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ngpiSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("bammEnable",
				"22BAMM", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("bammColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("bammLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("bammSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("bammSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));			
		
		attrs.addAttr(new MiscResourceAttr("ukm2Enable",
				"11UKM2", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("ukm2Color", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("ukm2LineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("ukm2SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("ukm2SymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("bamdEnable",
				"23BAMD", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("bamdColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("bamdLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("bamdSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("bamdSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));			
		
		attrs.addAttr(new MiscResourceAttr("hwfiEnable",
				"12HWFI", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("hwfiColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("hwfiLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("hwfiSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("hwfiSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));		
		attrs.addAttr(new MiscResourceAttr("bamsEnable",
				"24BAMS", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("bamsColor", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("bamsLineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("bamsSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("bamsSymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));			
		
		attrs.addAttr(new MiscResourceAttr(null,
				null, EditElement.VERTICAL_SEPARATOR, 6));			
		attrs.addAttr(new MiscResourceAttr("egr2Enable",
				"25EGR2", EditElement.CHECK_BOX, 7));
		attrs.addAttr(new MiscResourceAttr("egr2Color", "",
				EditElement.COLOR_SELECTOR, 8));
		attrs.addAttr(new MiscResourceAttr("egr2LineWidth",
				"Line Width", EditElement.SPINNER, 9));
		attrs.addAttr(new MiscResourceAttr("egr2SymbolWidth",
				"Symbol Width", EditElement.SPINNER, 10));
		attrs.addAttr(new MiscResourceAttr("egr2SymbolSize",
				"Symbol Size", EditElement.SPINNER, 11));	
	
		
		attrs.addAttr(new MiscResourceAttr(null, null,
				EditElement.SEPARATOR, 1));	
		
		attrs.addAttr(new MiscResourceAttr("timeEnable", "Time",
				EditElement.CHECK_BOX, 1));

		attrs.addAttr(new MiscResourceAttr("nameEnable", "Name",
				EditElement.CHECK_BOX, 1));	
		
		attrs.addAttr(new MiscResourceAttr("speedEnable", "Speed",
				EditElement.CHECK_BOX, 1));

		attrs.addAttr(new MiscResourceAttr("markerEnable", "Marker",
				EditElement.CHECK_BOX, 1));			
		return attrs;
	}

   public String getLegendName() {
		return legendName;
	}

	public void setLegendName(String legendName) {
		this.legendName = legendName;
	}

	public boolean getOhpcEnable() {
		return ohpcEnable;
	}

	public void setOhpcEnable(boolean ohpcEnable) {
		this.ohpcEnable = ohpcEnable;
	}

	public RGB getOhpcColor() {
		return ohpcColor;
	}

	public void setOhpcColor(RGB ohpcColor) {
		this.ohpcColor = ohpcColor;
	}

	public int getOhpcLineWidth() {
		return ohpcLineWidth;
	}

	public void setOhpcLineWidth(int ohpcLineWidth) {
		this.ohpcLineWidth = ohpcLineWidth;
	}

	public int getOhpcSymbolWidth() {
		return ohpcSymbolWidth;
	}

	public void setOhpcSymbolWidth(int ohpcSymbolWidth) {
		this.ohpcSymbolWidth = ohpcSymbolWidth;
	}

	public float getOhpcSymbolSize() {
		return ohpcSymbolSize;
	}

	public void setOhpcSymbolSize(float ohpcSymbolSize) {
		this.ohpcSymbolSize = ohpcSymbolSize;
	}

	public boolean getClipEnable() {
		return clipEnable;
	}

	public void setClipEnable(boolean clipEnable) {
		this.clipEnable = clipEnable;
	}

	public RGB getClipColor() {
		return clipColor;
	}

	public void setClipColor(RGB clipColor) {
		this.clipColor = clipColor;
	}

	public int getClipLineWidth() {
		return clipLineWidth;
	}

	public void setClipLineWidth(int clipLineWidth) {
		this.clipLineWidth = clipLineWidth;
	}

	public int getClipSymbolWidth() {
		return clipSymbolWidth;
	}

	public void setClipSymbolWidth(int clipSymbolWidth) {
		this.clipSymbolWidth = clipSymbolWidth;
	}

	public float getClipSymbolSize() {
		return clipSymbolSize;
	}

	public void setClipSymbolSize(float clipSymbolSize) {
		this.clipSymbolSize = clipSymbolSize;
	}

	public boolean getAvnoEnable() {
		return avnoEnable;
	}

	public void setAvnoEnable(boolean avnoEnable) {
		this.avnoEnable = avnoEnable;
	}

	public RGB getAvnoColor() {
		return avnoColor;
	}

	public void setAvnoColor(RGB avnoColor) {
		this.avnoColor = avnoColor;
	}

	public int getAvnoLineWidth() {
		return avnoLineWidth;
	}

	public void setAvnoLineWidth(int avnoLineWidth) {
		this.avnoLineWidth = avnoLineWidth;
	}

	public int getAvnoSymbolWidth() {
		return avnoSymbolWidth;
	}

	public void setAvnoSymbolWidth(int avnoSymbolWidth) {
		this.avnoSymbolWidth = avnoSymbolWidth;
	}

	public float getAvnoSymbolSize() {
		return avnoSymbolSize;
	}

	public void setAvnoSymbolSize(float avnoSymbolSize) {
		this.avnoSymbolSize = avnoSymbolSize;
	}

	public boolean getMrfoEnable() {
		return mrfoEnable;
	}

	public void setMrfoEnable(boolean mrfoEnable) {
		this.mrfoEnable = mrfoEnable;
	}

	public RGB getMrfoColor() {
		return mrfoColor;
	}

	public void setMrfoColor(RGB mrfoColor) {
		this.mrfoColor = mrfoColor;
	}

	public int getMrfoLineWidth() {
		return mrfoLineWidth;
	}

	public void setMrfoLineWidth(int mrfoLineWidth) {
		this.mrfoLineWidth = mrfoLineWidth;
	}

	public int getMrfoSymbolWidth() {
		return mrfoSymbolWidth;
	}

	public void setMrfoSymbolWidth(int mrfoSymbolWidth) {
		this.mrfoSymbolWidth = mrfoSymbolWidth;
	}

	public float getMrfoSymbolSize() {
		return mrfoSymbolSize;
	}

	public void setMrfoSymbolSize(float mrfoSymbolSize) {
		this.mrfoSymbolSize = mrfoSymbolSize;
	}

	public boolean getUkmEnable() {
		return ukmEnable;
	}

	public void setUkmEnable(boolean ukmEnable) {
		this.ukmEnable = ukmEnable;
	}

	public RGB getUkmColor() {
		return ukmColor;
	}

	public void setUkmColor(RGB ukmColor) {
		this.ukmColor = ukmColor;
	}

	public int getUkmLineWidth() {
		return ukmLineWidth;
	}

	public void setUkmLineWidth(int ukmLineWidth) {
		this.ukmLineWidth = ukmLineWidth;
	}

	public int getUkmSymbolWidth() {
		return ukmSymbolWidth;
	}

	public void setUkmSymbolWidth(int ukmSymbolWidth) {
		this.ukmSymbolWidth = ukmSymbolWidth;
	}

	public float getUkmSymbolSize() {
		return ukmSymbolSize;
	}

	public void setUkmSymbolSize(float ukmSymbolSize) {
		this.ukmSymbolSize = ukmSymbolSize;
	}

	public boolean getAvniEnable() {
		return avniEnable;
	}

	public void setAvniEnable(boolean avniEnable) {
		this.avniEnable = avniEnable;
	}

	public RGB getAvniColor() {
		return avniColor;
	}

	public void setAvniColor(RGB avniColor) {
		this.avniColor = avniColor;
	}

	public int getAvniLineWidth() {
		return avniLineWidth;
	}

	public void setAvniLineWidth(int avniLineWidth) {
		this.avniLineWidth = avniLineWidth;
	}

	public int getAvniSymbolWidth() {
		return avniSymbolWidth;
	}

	public void setAvniSymbolWidth(int avniSymbolWidth) {
		this.avniSymbolWidth = avniSymbolWidth;
	}

	public float getAvniSymbolSize() {
		return avniSymbolSize;
	}

	public void setAvniSymbolSize(float avniSymbolSize) {
		this.avniSymbolSize = avniSymbolSize;
	}

	public boolean getNamiEnable() {
		return namiEnable;
	}

	public void setNamiEnable(boolean namiEnable) {
		this.namiEnable = namiEnable;
	}

	public RGB getNamiColor() {
		return namiColor;
	}

	public void setNamiColor(RGB namiColor) {
		this.namiColor = namiColor;
	}

	public int getNamiLineWidth() {
		return namiLineWidth;
	}

	public void setNamiLineWidth(int namiLineWidth) {
		this.namiLineWidth = namiLineWidth;
	}

	public int getNamiSymbolWidth() {
		return namiSymbolWidth;
	}

	public void setNamiSymbolWidth(int namiSymbolWidth) {
		this.namiSymbolWidth = namiSymbolWidth;
	}

	public float getNamiSymbolSize() {
		return namiSymbolSize;
	}

	public void setNamiSymbolSize(float namiSymbolSize) {
		this.namiSymbolSize = namiSymbolSize;
	}

	public boolean getEmx2Enable() {
		return emx2Enable;
	}

	public void setEmx2Enable(boolean emx2Enable) {
		this.emx2Enable = emx2Enable;
	}

	public RGB getEmx2Color() {
		return emx2Color;
	}

	public void setEmx2Color(RGB emx2Color) {
		this.emx2Color = emx2Color;
	}

	public int getEmx2LineWidth() {
		return emx2LineWidth;
	}

	public void setEmx2LineWidth(int emx2LineWidth) {
		this.emx2LineWidth = emx2LineWidth;
	}

	public int getEmx2SymbolWidth() {
		return emx2SymbolWidth;
	}

	public void setEmx2SymbolWidth(int emx2SymbolWidth) {
		this.emx2SymbolWidth = emx2SymbolWidth;
	}

	public float getEmx2SymbolSize() {
		return emx2SymbolSize;
	}

	public void setEmx2SymbolSize(float emx2SymbolSize) {
		this.emx2SymbolSize = emx2SymbolSize;
	}

	public boolean getEgriEnable() {
		return egriEnable;
	}

	public void setEgriEnable(boolean egriEnable) {
		this.egriEnable = egriEnable;
	}

	public RGB getEgriColor() {
		return egriColor;
	}

	public void setEgriColor(RGB egriColor) {
		this.egriColor = egriColor;
	}

	public int getEgriLineWidth() {
		return egriLineWidth;
	}

	public void setEgriLineWidth(int egriLineWidth) {
		this.egriLineWidth = egriLineWidth;
	}

	public int getEgriSymbolWidth() {
		return egriSymbolWidth;
	}

	public void setEgriSymbolWidth(int egriSymbolWidth) {
		this.egriSymbolWidth = egriSymbolWidth;
	}

	public float getEgriSymbolSize() {
		return egriSymbolSize;
	}

	public void setEgriSymbolSize(float egriSymbolSize) {
		this.egriSymbolSize = egriSymbolSize;
	}

	public boolean getNgpiEnable() {
		return ngpiEnable;
	}

	public void setNgpiEnable(boolean ngpiEnable) {
		this.ngpiEnable = ngpiEnable;
	}

	public RGB getNgpiColor() {
		return ngpiColor;
	}

	public void setNgpiColor(RGB ngpiColor) {
		this.ngpiColor = ngpiColor;
	}

	public int getNgpiLineWidth() {
		return ngpiLineWidth;
	}

	public void setNgpiLineWidth(int ngpiLineWidth) {
		this.ngpiLineWidth = ngpiLineWidth;
	}

	public int getNgpiSymbolWidth() {
		return ngpiSymbolWidth;
	}

	public void setNgpiSymbolWidth(int ngpiSymbolWidth) {
		this.ngpiSymbolWidth = ngpiSymbolWidth;
	}

	public float getNgpiSymbolSize() {
		return ngpiSymbolSize;
	}

	public void setNgpiSymbolSize(float ngpiSymbolSize) {
		this.ngpiSymbolSize = ngpiSymbolSize;
	}

	public boolean getUkm2Enable() {
		return ukm2Enable;
	}

	public void setUkm2Enable(boolean ukm2Enable) {
		this.ukm2Enable = ukm2Enable;
	}

	public RGB getUkm2Color() {
		return ukm2Color;
	}

	public void setUkm2Color(RGB ukm2Color) {
		this.ukm2Color = ukm2Color;
	}

	public int getUkm2LineWidth() {
		return ukm2LineWidth;
	}

	public void setUkm2LineWidth(int ukm2LineWidth) {
		this.ukm2LineWidth = ukm2LineWidth;
	}

	public int getUkm2SymbolWidth() {
		return ukm2SymbolWidth;
	}

	public void setUkm2SymbolWidth(int ukm2SymbolWidth) {
		this.ukm2SymbolWidth = ukm2SymbolWidth;
	}

	public float getUkm2SymbolSize() {
		return ukm2SymbolSize;
	}

	public void setUkm2SymbolSize(float ukm2SymbolSize) {
		this.ukm2SymbolSize = ukm2SymbolSize;
	}

	public boolean getHwfiEnable() {
		return hwfiEnable;
	}

	public void setHwfiEnable(boolean hwfiEnable) {
		this.hwfiEnable = hwfiEnable;
	}

	public RGB getHwfiColor() {
		return hwfiColor;
	}

	public void setHwfiColor(RGB hwfiColor) {
		this.hwfiColor = hwfiColor;
	}

	public int getHwfiLineWidth() {
		return hwfiLineWidth;
	}

	public void setHwfiLineWidth(int hwfiLineWidth) {
		this.hwfiLineWidth = hwfiLineWidth;
	}

	public int getHwfiSymbolWidth() {
		return hwfiSymbolWidth;
	}

	public void setHwfiSymbolWidth(int hwfiSymbolWidth) {
		this.hwfiSymbolWidth = hwfiSymbolWidth;
	}

	public float getHwfiSymbolSize() {
		return hwfiSymbolSize;
	}

	public void setHwfiSymbolSize(float hwfiSymbolSize) {
		this.hwfiSymbolSize = hwfiSymbolSize;
	}

	public boolean getGhmiEnable() {
		return ghmiEnable;
	}

	public void setGhmiEnable(boolean ghmiEnable) {
		this.ghmiEnable = ghmiEnable;
	}

	public RGB getGhmiColor() {
		return ghmiColor;
	}

	public void setGhmiColor(RGB ghmiColor) {
		this.ghmiColor = ghmiColor;
	}

	public int getGhmiLineWidth() {
		return ghmiLineWidth;
	}

	public void setGhmiLineWidth(int ghmiLineWidth) {
		this.ghmiLineWidth = ghmiLineWidth;
	}

	public int getGhmiSymbolWidth() {
		return ghmiSymbolWidth;
	}

	public void setGhmiSymbolWidth(int ghmiSymbolWidth) {
		this.ghmiSymbolWidth = ghmiSymbolWidth;
	}

	public float getGhmiSymbolSize() {
		return ghmiSymbolSize;
	}

	public void setGhmiSymbolSize(float ghmiSymbolSize) {
		this.ghmiSymbolSize = ghmiSymbolSize;
	}

	public boolean getGfniEnable() {
		return gfniEnable;
	}

	public void setGfniEnable(boolean gfniEnable) {
		this.gfniEnable = gfniEnable;
	}

	public RGB getGfniColor() {
		return gfniColor;
	}

	public void setGfniColor(RGB gfniColor) {
		this.gfniColor = gfniColor;
	}

	public int getGfniLineWidth() {
		return gfniLineWidth;
	}

	public void setGfniLineWidth(int gfniLineWidth) {
		this.gfniLineWidth = gfniLineWidth;
	}

	public int getGfniSymbolWidth() {
		return gfniSymbolWidth;
	}

	public void setGfniSymbolWidth(int gfniSymbolWidth) {
		this.gfniSymbolWidth = gfniSymbolWidth;
	}

	public float getGfniSymbolSize() {
		return gfniSymbolSize;
	}

	public void setGfniSymbolSize(float gfniSymbolSize) {
		this.gfniSymbolSize = gfniSymbolSize;
	}

	public boolean getAemiEnable() {
		return aemiEnable;
	}

	public void setAemiEnable(boolean aemiEnable) {
		this.aemiEnable = aemiEnable;
	}

	public RGB getAemiColor() {
		return aemiColor;
	}

	public void setAemiColor(RGB aemiColor) {
		this.aemiColor = aemiColor;
	}

	public int getAemiLineWidth() {
		return aemiLineWidth;
	}

	public void setAemiLineWidth(int aemiLineWidth) {
		this.aemiLineWidth = aemiLineWidth;
	}

	public int getAemiSymbolWidth() {
		return aemiSymbolWidth;
	}

	public void setAemiSymbolWidth(int aemiSymbolWidth) {
		this.aemiSymbolWidth = aemiSymbolWidth;
	}

	public float getAemiSymbolSize() {
		return aemiSymbolSize;
	}

	public void setAemiSymbolSize(float aemiSymbolSize) {
		this.aemiSymbolSize = aemiSymbolSize;
	}

	public boolean getTconEnable() {
		return tconEnable;
	}

	public void setTconEnable(boolean tconEnable) {
		this.tconEnable = tconEnable;
	}

	public RGB getTconColor() {
		return tconColor;
	}

	public void setTconColor(RGB tconColor) {
		this.tconColor = tconColor;
	}

	public int getTconLineWidth() {
		return tconLineWidth;
	}

	public void setTconLineWidth(int tconLineWidth) {
		this.tconLineWidth = tconLineWidth;
	}

	public int getTconSymbolWidth() {
		return tconSymbolWidth;
	}

	public void setTconSymbolWidth(int tconSymbolWidth) {
		this.tconSymbolWidth = tconSymbolWidth;
	}

	public float getTconSymbolSize() {
		return tconSymbolSize;
	}

	public void setTconSymbolSize(float tconSymbolSize) {
		this.tconSymbolSize = tconSymbolSize;
	}

	public boolean getGunaEnable() {
		return gunaEnable;
	}

	public void setGunaEnable(boolean gunaEnable) {
		this.gunaEnable = gunaEnable;
	}

	public RGB getGunaColor() {
		return gunaColor;
	}

	public void setGunaColor(RGB gunaColor) {
		this.gunaColor = gunaColor;
	}

	public int getGunaLineWidth() {
		return gunaLineWidth;
	}

	public void setGunaLineWidth(int gunaLineWidth) {
		this.gunaLineWidth = gunaLineWidth;
	}

	public int getGunaSymbolWidth() {
		return gunaSymbolWidth;
	}

	public void setGunaSymbolWidth(int gunaSymbolWidth) {
		this.gunaSymbolWidth = gunaSymbolWidth;
	}

	public float getGunaSymbolSize() {
		return gunaSymbolSize;
	}

	public void setGunaSymbolSize(float gunaSymbolSize) {
		this.gunaSymbolSize = gunaSymbolSize;
	}

	public boolean getTvcnEnable() {
		return tvcnEnable;
	}

	public void setTvcnEnable(boolean tvcnEnable) {
		this.tvcnEnable = tvcnEnable;
	}

	public RGB getTvcnColor() {
		return tvcnColor;
	}

	public void setTvcnColor(RGB tvcnColor) {
		this.tvcnColor = tvcnColor;
	}

	public int getTvcnLineWidth() {
		return tvcnLineWidth;
	}

	public void setTvcnLineWidth(int tvcnLineWidth) {
		this.tvcnLineWidth = tvcnLineWidth;
	}

	public int getTvcnSymbolWidth() {
		return tvcnSymbolWidth;
	}

	public void setTvcnSymbolWidth(int tvcnSymbolWidth) {
		this.tvcnSymbolWidth = tvcnSymbolWidth;
	}

	public float getTvcnSymbolSize() {
		return tvcnSymbolSize;
	}

	public void setTvcnSymbolSize(float tvcnSymbolSize) {
		this.tvcnSymbolSize = tvcnSymbolSize;
	}

	public boolean getEmxiEnable() {
		return emxiEnable;
	}

	public void setEmxiEnable(boolean emxiEnable) {
		this.emxiEnable = emxiEnable;
	}

	public RGB getEmxiColor() {
		return emxiColor;
	}

	public void setEmxiColor(RGB emxiColor) {
		this.emxiColor = emxiColor;
	}

	public int getEmxiLineWidth() {
		return emxiLineWidth;
	}

	public void setEmxiLineWidth(int emxiLineWidth) {
		this.emxiLineWidth = emxiLineWidth;
	}

	public int getEmxiSymbolWidth() {
		return emxiSymbolWidth;
	}

	public void setEmxiSymbolWidth(int emxiSymbolWidth) {
		this.emxiSymbolWidth = emxiSymbolWidth;
	}

	public float getEmxiSymbolSize() {
		return emxiSymbolSize;
	}

	public void setEmxiSymbolSize(float emxiSymbolSize) {
		this.emxiSymbolSize = emxiSymbolSize;
	}

	public boolean getFsseEnable() {
		return fsseEnable;
	}

	public void setFsseEnable(boolean fsseEnable) {
		this.fsseEnable = fsseEnable;
	}

	public RGB getFsseColor() {
		return fsseColor;
	}

	public void setFsseColor(RGB fsseColor) {
		this.fsseColor = fsseColor;
	}

	public int getFsseLineWidth() {
		return fsseLineWidth;
	}

	public void setFsseLineWidth(int fsseLineWidth) {
		this.fsseLineWidth = fsseLineWidth;
	}

	public int getFsseSymbolWidth() {
		return fsseSymbolWidth;
	}

	public void setFsseSymbolWidth(int fsseSymbolWidth) {
		this.fsseSymbolWidth = fsseSymbolWidth;
	}

	public float getFsseSymbolSize() {
		return fsseSymbolSize;
	}

	public void setFsseSymbolSize(float fsseSymbolSize) {
		this.fsseSymbolSize = fsseSymbolSize;
	}

	public boolean getUkmiEnable() {
		return ukmiEnable;
	}

	public void setUkmiEnable(boolean ukmiEnable) {
		this.ukmiEnable = ukmiEnable;
	}

	public RGB getUkmiColor() {
		return ukmiColor;
	}

	public void setUkmiColor(RGB ukmiColor) {
		this.ukmiColor = ukmiColor;
	}

	public int getUkmiLineWidth() {
		return ukmiLineWidth;
	}

	public void setUkmiLineWidth(int ukmiLineWidth) {
		this.ukmiLineWidth = ukmiLineWidth;
	}

	public int getUkmiSymbolWidth() {
		return ukmiSymbolWidth;
	}

	public void setUkmiSymbolWidth(int ukmiSymbolWidth) {
		this.ukmiSymbolWidth = ukmiSymbolWidth;
	}

	public float getUkmiSymbolSize() {
		return ukmiSymbolSize;
	}

	public void setUkmiSymbolSize(float ukmiSymbolSize) {
		this.ukmiSymbolSize = ukmiSymbolSize;
	}

	public boolean getBammEnable() {
		return bammEnable;
	}

	public void setBammEnable(boolean bammEnable) {
		this.bammEnable = bammEnable;
	}

	public RGB getBammColor() {
		return bammColor;
	}

	public void setBammColor(RGB bammColor) {
		this.bammColor = bammColor;
	}

	public int getBammLineWidth() {
		return bammLineWidth;
	}

	public void setBammLineWidth(int bammLineWidth) {
		this.bammLineWidth = bammLineWidth;
	}

	public int getBammSymbolWidth() {
		return bammSymbolWidth;
	}

	public void setBammSymbolWidth(int bammSymbolWidth) {
		this.bammSymbolWidth = bammSymbolWidth;
	}

	public float getBammSymbolSize() {
		return bammSymbolSize;
	}

	public void setBammSymbolSize(float bammSymbolSize) {
		this.bammSymbolSize = bammSymbolSize;
	}

	public boolean getBamdEnable() {
		return bamdEnable;
	}

	public void setBamdEnable(boolean bamdEnable) {
		this.bamdEnable = bamdEnable;
	}

	public RGB getBamdColor() {
		return bamdColor;
	}

	public void setBamdColor(RGB bamdColor) {
		this.bamdColor = bamdColor;
	}

	public int getBamdLineWidth() {
		return bamdLineWidth;
	}

	public void setBamdLineWidth(int bamdLineWidth) {
		this.bamdLineWidth = bamdLineWidth;
	}

	public int getBamdSymbolWidth() {
		return bamdSymbolWidth;
	}

	public void setBamdSymbolWidth(int bamdSymbolWidth) {
		this.bamdSymbolWidth = bamdSymbolWidth;
	}

	public float getBamdSymbolSize() {
		return bamdSymbolSize;
	}

	public void setBamdSymbolSize(float bamdSymbolSize) {
		this.bamdSymbolSize = bamdSymbolSize;
	}

	public boolean getBamsEnable() {
		return bamsEnable;
	}

	public void setBamsEnable(boolean bamsEnable) {
		this.bamsEnable = bamsEnable;
	}

	public RGB getBamsColor() {
		return bamsColor;
	}

	public void setBamsColor(RGB bamsColor) {
		this.bamsColor = bamsColor;
	}

	public int getBamsLineWidth() {
		return bamsLineWidth;
	}

	public void setBamsLineWidth(int bamsLineWidth) {
		this.bamsLineWidth = bamsLineWidth;
	}

	public int getBamsSymbolWidth() {
		return bamsSymbolWidth;
	}

	public void setBamsSymbolWidth(int bamsSymbolWidth) {
		this.bamsSymbolWidth = bamsSymbolWidth;
	}

	public float getBamsSymbolSize() {
		return bamsSymbolSize;
	}

	public void setBamsSymbolSize(float bamsSymbolSize) {
		this.bamsSymbolSize = bamsSymbolSize;
	}

	public boolean getEgr2Enable() {
		return egr2Enable;
	}

	public void setEgr2Enable(boolean egr2Enable) {
		this.egr2Enable = egr2Enable;
	}

	public RGB getEgr2Color() {
		return egr2Color;
	}

	public void setEgr2Color(RGB egr2Color) {
		this.egr2Color = egr2Color;
	}

	public int getEgr2LineWidth() {
		return egr2LineWidth;
	}

	public void setEgr2LineWidth(int egr2LineWidth) {
		this.egr2LineWidth = egr2LineWidth;
	}

	public int getEgr2SymbolWidth() {
		return egr2SymbolWidth;
	}

	public void setEgr2SymbolWidth(int egr2SymbolWidth) {
		this.egr2SymbolWidth = egr2SymbolWidth;
	}

	public float getEgr2SymbolSize() {
		return egr2SymbolSize;
	}

	public void setEgr2SymbolSize(float egr2SymbolSize) {
		this.egr2SymbolSize = egr2SymbolSize;
	}

	public boolean getTimeEnable() {
		return timeEnable;
	}

	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}

	public boolean getNameEnable() {
		return nameEnable;
	}

	public void setNameEnable(boolean nameEnable) {
		this.nameEnable = nameEnable;
	}

	public boolean getSpeedEnable() {
		return speedEnable;
	}

	public void setSpeedEnable(boolean speedEnable) {
		this.speedEnable = speedEnable;
	}

	public boolean getMarkerEnable() {
		return markerEnable;
	}

	public void setMarkerEnable(boolean markerEnable) {
		this.markerEnable = markerEnable;
	}

}
