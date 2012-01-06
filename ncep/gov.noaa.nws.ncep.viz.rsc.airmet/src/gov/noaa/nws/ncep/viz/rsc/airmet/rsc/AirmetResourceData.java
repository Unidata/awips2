package gov.noaa.nws.ncep.viz.rsc.airmet.rsc;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscRscAttrs;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;


/**
 * AirmetResource - Display AIRMET data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 4/22/10       245        Greg Hull  Initial creation.
 * 6/22/10       273        Greg Hull  NatlCntrsResourceName from Abstract Class
 * 
 * </pre>
 * 
 * @author ghull 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="AirmetResourceData")
public class AirmetResourceData extends AbstractNatlCntrsRequestableResourceData
                        implements IMiscResourceData, INatlCntrsResourceData { 

    @XmlElement
    protected boolean ifrEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     ifrColor;
    @XmlElement
    protected int     ifrLineWidth;
    @XmlElement
	protected int     ifrSymbolSize;

    @XmlElement
    protected boolean mntObscEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     mntObscColor;
    @XmlElement
    protected int     mntObscLineWidth;
    @XmlElement
	protected int     mntObscSymbolSize;

    @XmlElement
    protected boolean turbEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     turbColor1;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     turbColor2;
    @XmlElement
    protected int     turbLineWidth;
    @XmlElement
	protected int     turbSymbolWidth;
    @XmlElement
	protected int     turbSymbolSize;
    @XmlElement
    protected int     turbLevel;

    @XmlElement
    protected boolean icingEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     icingColor;
    @XmlElement
    protected int     icingLineWidth;
    @XmlElement
	protected int     icingSymbolWidth;
    @XmlElement
	protected int     icingSymbolSize;

    @XmlElement
    protected boolean sustWindsEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     sustWindsColor;
    @XmlElement
    protected int     sustWindsLineWidth;
    @XmlElement
	protected int     sustWindsSymbolWidth;
    @XmlElement
	protected int     sustWindsSymbolSize;

    @XmlElement
    protected boolean lowLevelWindShearEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     lowLevelWindShearColor;
    @XmlElement
    protected int     lowLevelWindShearLineWidth;
    @XmlElement
	protected int     lowLevelWindShearSymbolSize;

    @XmlElement
    protected int   lowerFilterLevel;
    @XmlElement
    protected int   upperFilterLevel;

	
    @XmlElement
    protected boolean symbolEnable;
    @XmlElement
    protected boolean timeEnable;
    @XmlElement
    protected boolean flightLevelEnable;
    @XmlElement
    protected boolean seqIdEnable;
        
    
    public boolean getIfrEnable() {
		return ifrEnable;
	}
	public void setIfrEnable(boolean ifrEnable) {
		this.ifrEnable = ifrEnable;
	}
	public RGB getIfrColor() {
		return ifrColor;
	}
	public void setIfrColor(RGB ifrColor) {
		this.ifrColor = ifrColor;
	}
	public int getIfrLineWidth() {
		return ifrLineWidth;
	}
	public void setIfrLineWidth(int ifrLineWidth) {
		this.ifrLineWidth = ifrLineWidth;
	}
	public int getIfrSymbolSize() {
		return ifrSymbolSize;
	}
	public void setIfrSymbolSize(int ifrSymbolSize) {
		this.ifrSymbolSize = ifrSymbolSize;
	}
	public boolean getMntObscEnable() {
		return mntObscEnable;
	}
	public void setMntObscEnable(boolean mntObscEnable) {
		this.mntObscEnable = mntObscEnable;
	}
	public RGB getMntObscColor() {
		return mntObscColor;
	}
	public void setMntObscColor(RGB mntObscColor) {
		this.mntObscColor = mntObscColor;
	}
	public int getMntObscLineWidth() {
		return mntObscLineWidth;
	}
	public void setMntObscLineWidth(int mntObscLineWidth) {
		this.mntObscLineWidth = mntObscLineWidth;
	}
	public int getMntObscSymbolSize() {
		return mntObscSymbolSize;
	}
	public void setMntObscSymbolSize(int mntObscSymbolSize) {
		this.mntObscSymbolSize = mntObscSymbolSize;
	}
	public boolean getTurbEnable() {
		return turbEnable;
	}
	public void setTurbEnable(boolean turbEnable) {
		this.turbEnable = turbEnable;
	}
	public RGB getTurbColor1() {
		return turbColor1;
	}
	public void setTurbColor1(RGB turbColor1) {
		this.turbColor1 = turbColor1;
	}
	public RGB getTurbColor2() {
		return turbColor2;
	}
	public void setTurbColor2(RGB turbColor2) {
		this.turbColor2 = turbColor2;
	}
	public int getTurbLineWidth() {
		return turbLineWidth;
	}
	public void setTurbLineWidth(int turbLineWidth) {
		this.turbLineWidth = turbLineWidth;
	}
	public int getTurbSymbolWidth() {
		return turbSymbolWidth;
	}
	public void setTurbSymbolWidth(int turbSymbolWidth) {
		this.turbSymbolWidth = turbSymbolWidth;
	}
	public int getTurbSymbolSize() {
		return turbSymbolSize;
	}
	public void setTurbSymbolSize(int turbSymbolSize) {
		this.turbSymbolSize = turbSymbolSize;
	}
	public int getTurbLevel() {
		return turbLevel;
	}
	public void setTurbLevel(int turbLevel) {
		this.turbLevel = turbLevel;
	}
	public boolean getIcingEnable() {
		return icingEnable;
	}
	public void setIcingEnable(boolean icingEnable) {
		this.icingEnable = icingEnable;
	}
	public RGB getIcingColor() {
		return icingColor;
	}
	public void setIcingColor(RGB icingColor) {
		this.icingColor = icingColor;
	}
	public int getIcingLineWidth() {
		return icingLineWidth;
	}
	public void setIcingLineWidth(int icingLineWidth) {
		this.icingLineWidth = icingLineWidth;
	}
	public int getIcingSymbolWidth() {
		return icingSymbolWidth;
	}
	public void setIcingSymbolWidth(int icingSymbolWidth) {
		this.icingSymbolWidth = icingSymbolWidth;
	}
	public int getIcingSymbolSize() {
		return icingSymbolSize;
	}
	public void setIcingSymbolSize(int icingSymbolSize) {
		this.icingSymbolSize = icingSymbolSize;
	}
	public boolean getSustWindsEnable() {
		return sustWindsEnable;
	}
	public void setSustWindsEnable(boolean sustWindsEnable) {
		this.sustWindsEnable = sustWindsEnable;
	}
	public RGB getSustWindsColor() {
		return sustWindsColor;
	}
	public void setSustWindsColor(RGB sustWindsColor) {
		this.sustWindsColor = sustWindsColor;
	}
	public int getSustWindsLineWidth() {
		return sustWindsLineWidth;
	}
	public void setSustWindsLineWidth(int sustWindsLineWidth) {
		this.sustWindsLineWidth = sustWindsLineWidth;
	}
	public int getSustWindsSymbolWidth() {
		return sustWindsSymbolWidth;
	}
	public void setSustWindsSymbolWidth(int sustWindsSymbolWidth) {
		this.sustWindsSymbolWidth = sustWindsSymbolWidth;
	}
	public int getSustWindsSymbolSize() {
		return sustWindsSymbolSize;
	}
	public void setSustWindsSymbolSize(int sustWindsSymbolSize) {
		this.sustWindsSymbolSize = sustWindsSymbolSize;
	}
	public boolean getLowLevelWindShearEnable() {
		return lowLevelWindShearEnable;
	}
	public void setLowLevelWindShearEnable(boolean lowLevelWindShearEnable) {
		this.lowLevelWindShearEnable = lowLevelWindShearEnable;
	}
	public RGB getLowLevelWindShearColor() {
		return lowLevelWindShearColor;
	}
	public void setLowLevelWindShearColor(RGB lowLevelWindShearColor) {
		this.lowLevelWindShearColor = lowLevelWindShearColor;
	}
	public int getLowLevelWindShearLineWidth() {
		return lowLevelWindShearLineWidth;
	}
	public void setLowLevelWindShearLineWidth(int lowLevelWindShearLineWidth) {
		this.lowLevelWindShearLineWidth = lowLevelWindShearLineWidth;
	}
	public int getLowLevelWindShearSymbolSize() {
		return lowLevelWindShearSymbolSize;
	}
	public void setLowLevelWindShearSymbolSize(int lowLevelWindShearSymbolSize) {
		this.lowLevelWindShearSymbolSize = lowLevelWindShearSymbolSize;
	}
	public int getLowerFilterLevel() {
		return lowerFilterLevel;
	}
	public void setLowerFilterLevel(int lowerFilterLevel) {
		this.lowerFilterLevel = lowerFilterLevel;
	}
	public int getUpperFilterLevel() {
		return upperFilterLevel;
	}
	public void setUpperFilterLevel(int upperFilterLevel) {
		this.upperFilterLevel = upperFilterLevel;
	}
	public boolean getSymbolEnable() {
		return symbolEnable;
	}
	public void setSymbolEnable(boolean symbolEnable) {
		this.symbolEnable = symbolEnable;
	}
	public boolean getSeqIdEnable() {
		return seqIdEnable;
	}
	public void setSeqIdEnable(boolean seqIdEnable) {
		this.seqIdEnable = seqIdEnable;
	}
	public boolean getTimeEnable() {
		return timeEnable;
	}
	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}
	public boolean getFlightLevelEnable() {
		return flightLevelEnable;
	}
	public void setFlightLevelEnable(boolean flightLevelEnable) {
		this.flightLevelEnable = flightLevelEnable;
	}

	public AirmetResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return "AIRMET";
            }
        };
    }

    @Override
    public boolean isEventResource() {
    	return true;
    }
    
	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 5 );
		MiscResourceAttr spnrAttr;
		
		attrs.addAttr( new MiscResourceAttr( "ifrEnable", "Instr. Flight Rules",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "ifrColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "ifrLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
//		attrs.addAttr( new MiscResourceAttr( "ifrSymbolWidth", "Symbol Width", 
//				EditElement.SPINNER, 5 ));
		attrs.addAttr( new MiscResourceAttr( "ifrSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));
		
		attrs.addAttr( new MiscResourceAttr( "mntObscEnable", "Mount. Obscur.",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "mntObscColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "mntObscLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
//		attrs.addAttr( new MiscResourceAttr( "mntObscSymbolWidth", "Symbol Width", 
//				EditElement.SPINNER, 5 ));
		attrs.addAttr( new MiscResourceAttr( "mntObscSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));

		attrs.addAttr( new MiscResourceAttr( "turbEnable", "Turbulence",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "turbColor1", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "turbColor2", "", 
				EditElement.COLOR_SELECTOR, 3 ));
		spnrAttr = new MiscResourceAttr( "turbLevel", "Level", 
				EditElement.SPINNER, 4 );
		spnrAttr.setSpinnerControls(0, 0, 1001, 10, 10 );
		attrs.addAttr( spnrAttr );

//		attrs.addAttr( new MiscResourceAttr( "", "Turbulence", 
//				EditElement.LABEL, 2 ));
		attrs.addAttr( new MiscResourceAttr( "turbLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "turbSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "turbSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));
			
			
		attrs.addAttr( new MiscResourceAttr( "icingEnable", "Icing",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "icingColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "icingLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "icingSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "icingSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));
		
		attrs.addAttr( new MiscResourceAttr( "sustWindsEnable", "Sustained Winds.",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "sustWindsColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "sustWindsLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "sustWindsSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "sustWindsSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));

		attrs.addAttr( new MiscResourceAttr( "lowLevelWindShearEnable", "Low Lvl Wind Shear.",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "lowLevelWindShearColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "lowLevelWindShearLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
//		attrs.addAttr( new MiscResourceAttr( "lowLevelWindShearSymbolWidth", "Symbol Width", 
//				EditElement.SPINNER, 5 ));
		attrs.addAttr( new MiscResourceAttr( "lowLevelWindShearSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));

		spnrAttr = new MiscResourceAttr( "lowerFilterLevel", "Lower Level Filter", 
				EditElement.SPINNER, 1 );
		spnrAttr.setSpinnerControls( 0, 0, 1001, 10, 10 );
		attrs.addAttr( spnrAttr );

		attrs.addAttr( new MiscResourceAttr( "", "       Upper ", 
						EditElement.LABEL, 2 ));

		spnrAttr = new MiscResourceAttr( "upperFilterLevel", "Level Filter", 
				EditElement.SPINNER, 3 );
		spnrAttr.setSpinnerControls( 0, 0, 1001, 10, 10 );
		attrs.addAttr( spnrAttr );

		attrs.addAttr( new MiscResourceAttr( null, null, 
				   EditElement.SEPARATOR, 1 ));

		attrs.addAttr( new MiscResourceAttr( "symbolEnable", "Show Symbol",
				EditElement.CHECK_BOX, 1 ));

		attrs.addAttr( new MiscResourceAttr( "timeEnable", "Show Time",
				EditElement.CHECK_BOX, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "flightLevelEnable", "Show Flight Levels",
				EditElement.CHECK_BOX, 1 ));

		attrs.addAttr( new MiscResourceAttr( "seqIdEnable", "Show Sequence Id",
				EditElement.CHECK_BOX, 1 ));

		return attrs;
	}	

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			com.raytheon.uf.common.dataplugin.PluginDataObject[] objects)
			throws VizException {
		return new AirmetResource( this, loadProperties );
	}

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof AirmetResourceData == false) {
            return false;
        }

        AirmetResourceData other = (AirmetResourceData) obj;

        // NOTE; The resource attributes are compared in the base class so se don't
        // need to check them here.

        return true;    
    }

}