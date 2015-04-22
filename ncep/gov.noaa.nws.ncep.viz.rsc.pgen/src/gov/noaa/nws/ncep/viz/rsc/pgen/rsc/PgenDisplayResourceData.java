package gov.noaa.nws.ncep.viz.rsc.pgen.rsc;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;


/**
 * PgenResourceData - Resource Data for Display of PGEN Products loaded from XML.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 Dec 2009  202        B. Hebbard  Initial creation.
 * 04 Mar 2010  226        Greg Hull   don't override getQualifiedResourceName()
 * 08 Mar 2010  226        Greg Hull   override generateFrameTimes()
 * 06 Aug 2010  273        Greg Hull   rm color, use legendColor
 * 06 Oct 2010  307        Greg Hull   override getAvailableTimes() 
 * 09 Aug 2011  450        Greg Hull   move productLocation and productName to metadata 
 * 14 Dec 2012  861        Greg Hull   add color Matrix Selector for the 
 * 14 Jul 2013  1011       Greg Hull   request from pgen db.
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-PgenResourceData")
public class PgenDisplayResourceData extends AbstractNatlCntrsRequestableResourceData
                        implements IMiscResourceData, INatlCntrsResourceData { 

    @XmlElement
    protected boolean monoColorEnable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     monoColor;

    @XmlElement
    protected boolean fillModeEnable;
    
    public PgenDisplayResourceData() throws VizException {
        super();
	}

	public boolean getMonoColorEnable() {
		return monoColorEnable;
	}

	public void setMonoColorEnable(boolean monoColorEnable) {
		this.monoColorEnable = monoColorEnable;
	}

	public RGB getMonoColor() {
		return monoColor;
	}

	public void setMonoColor(RGB monoColor) {
		this.monoColor = monoColor;
		setLegendColor( monoColor );
	}

	public boolean getFillModeEnable() {
		return fillModeEnable;
	}

	public void setFillModeEnable(boolean fillModeEnable) {
		this.fillModeEnable = fillModeEnable;
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 1 );
	
		attrs.addAttr( new MiscResourceAttr( "fillModeEnable", "Fill Mode",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "", "",
				EditElement.SEPARATOR, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "monoColorEnable", "Mono Color",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "monoColor", "", 
				EditElement.COLOR_PALLETE, 1 ));

		return attrs;
	}	

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			com.raytheon.uf.common.dataplugin.PluginDataObject[] objects)
			throws VizException {
		return new PgenDisplayResource( this, loadProperties );
	}
	
	public ArrayList<DataTime> generateFrameTimes( ) {
		ArrayList<DataTime> times = new ArrayList<DataTime>();		
		times.add( new DataTime( Calendar.getInstance().getTime() ) );
		return times;
	}

}