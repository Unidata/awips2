package gov.noaa.nws.ncep.metparameters;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.NotDerivableException;

import org.junit.Test;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class AbstractMetParameterTest {
//TODO update and add test-cases...
//	@Test
//	public void testDerive() {
//		MetParameterFactory instance = MetParameterFactory.getInstance();
//        AbstractMetParameter tempParam = instance.createParameter("Temperature",
//                new Amount(243, SI.CELSIUS ) );
////    AbstractMetParameter relhParam = instance.createParameter("RELH", new Amount(0,NonSI.PERCENT) );
////            relhParam.setValue( new Amount(33, SI.CELSIUS ) );
//    AbstractMetParameter dptParam = instance.createParameter("DewPointTemp",
//                    new Amount(23, SI.CELSIUS ) );
//    AbstractMetParameter presParam = instance.createParameter("Pressure",
//                       new Amount(29, NonSI.INCH_OF_MERCURY ) );
//    AbstractMetParameter thteParam = instance.createParameter("PotentialTemperature", SI.KELVIN);
//    AbstractMetParameter heatIndexParam = instance.createParameter("HeatIndex", NonSI.FAHRENHEIT);
//    List<AbstractMetParameter> inputPrms = new ArrayList<AbstractMetParameter>();
////    inputPrms.add( relhParam );
//    inputPrms.add( tempParam );
//    inputPrms.add( dptParam );
//    inputPrms.add( presParam );
//              if( heatIndexParam.derivable(inputPrms) ) {
//        System.out.println("heat index is derivable from temp and dwpt");
//}
//    else {
//        System.out.println("heat index is derivable from temp and dwpt");
//}
//               try {
//   HeatIndex hIndex = (HeatIndex)     heatIndexParam.derive(inputPrms);
////        System.out.println("The computed heat index is: " + hIndex.getValue().doubleValue() + " " + hIndex.getValue().getUnit() );
//} catch (NotDerivableException e) {
//        // TODO Auto-generated catch block
//        e.printStackTrace();
//}
//	}
//
}
