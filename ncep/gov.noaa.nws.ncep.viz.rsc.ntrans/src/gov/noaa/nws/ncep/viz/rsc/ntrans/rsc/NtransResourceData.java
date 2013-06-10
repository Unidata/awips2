package gov.noaa.nws.ncep.viz.rsc.ntrans.rsc;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscRscAttrs;

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
 * NtransResourceData - Resource Data for Display of NTRANS Metafiles.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Apr 2013  838        B. Hebbard  Initial creation.
 * 25 Apr 2013  838        G. Hull     don't override getAvailableDataTimes(). rm legend as a parameter
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-NtransResourceData")
public class NtransResourceData extends AbstractNatlCntrsRequestableResourceData
                        implements IMiscResourceData, INatlCntrsResourceData { 

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB color;  //  resource legend color only

    @XmlElement
    protected String modelName;

	@XmlElement
    protected String metafileName;

    @XmlElement
    protected String productName;
    
    //  ------------------------------------------------------------

    /**
     * Create an NTRANS Metafile display resource.
     * 
     * @throws VizException
     */
    public NtransResourceData() throws VizException {
        super();
    }

	public String getModelName() {
		return modelName;
	}

	public void setModelName(String model) {
		this.modelName = model;
	}

	public String getMetafileName() {
		return metafileName;
	}

	public void setMetafileName(String metafile) {
		this.metafileName = metafile;
	}

	public String getProductName() {
		return productName;
	}

	public void setProductName(String group) {
		this.productName = group;
	}

	@Override
	public NcDisplayType[] getSupportedDisplayTypes() {
		return new NcDisplayType[] { NcDisplayType.NTRANS_DISPLAY };
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 3 );
	
		attrs.addAttr( new MiscResourceAttr( "hour0Enable", "Initial (0-hour)",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "hour0Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "hour0LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		attrs.addAttr( new MiscResourceAttr( "hour1Enable", "1-hour",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "hour1Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "hour1LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		attrs.addAttr( new MiscResourceAttr( "hour2Enable", "2-hour",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "hour2Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "hour2LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		attrs.addAttr( new MiscResourceAttr( "outlookEnable", "Outlook",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "outlookColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "outlookLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		
		attrs.addAttr( new MiscResourceAttr( null, null, 
			   EditElement.SEPARATOR, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "hour0sequenceIdEnable", "Sequence ID (0-hour)",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "timeEnable", "Time",
				EditElement.CHECK_BOX, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "motionEnable", "Direction/speed",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "flightLevelEnable", "Flight Level",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "intensityEnable", "Intensity",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "hour1sequenceIdEnable", "Sequence ID (1-hour)",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "hour2sequenceIdEnable", "Sequence ID (2-hour)",
				EditElement.CHECK_BOX, 1 ));

		return attrs;
	}	

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			com.raytheon.uf.common.dataplugin.PluginDataObject[] objects)
			throws VizException {
		return new NtransResource( this, loadProperties );
	}

	/*
	public ArrayList<DataTime> generateFrameTimes( ) {
		ArrayList<DataTime> times = new ArrayList<DataTime>();		
		times.add( new DataTime( Calendar.getInstance().getTime() ) );
		return times;
	}
	*/

}