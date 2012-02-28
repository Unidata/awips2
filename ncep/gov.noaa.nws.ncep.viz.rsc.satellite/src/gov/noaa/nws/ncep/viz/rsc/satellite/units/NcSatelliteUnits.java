package gov.noaa.nws.ncep.viz.rsc.satellite.units;

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;


/**
 * Contains references to units used by satellite data
 * This will override the Unit associated with the label 'IRPixel' with the
 * NatlCntrs NcIRPixel which will use a converter that implements NMAP's 
 * equation for converting satellite pixels to temperature values.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/25/10                 ghull        Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 */
public class NcSatelliteUnits {

	private NcSatelliteUnits() {
	}

	public static final Unit<Temperature> NC_IR_PIXEL = new NcIRPixel();
	
	public static final Unit<Dimensionless> MCIDAS_BRIT = new McidasBritPixel();

	public static void register() {
		
		SatelliteUnits.register();
		
		UnitFormat.getUCUMInstance().label(NcSatelliteUnits.NC_IR_PIXEL, "IRPixel");
		UnitFormat.getUCUMInstance().label(NcSatelliteUnits.MCIDAS_BRIT, "BRIT");
	}

}