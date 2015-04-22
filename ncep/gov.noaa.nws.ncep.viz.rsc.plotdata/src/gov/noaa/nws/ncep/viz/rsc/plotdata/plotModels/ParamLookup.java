
package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;


import java.text.DecimalFormat;
import java.util.TreeMap;

/**
 * Record implementation for metar plugin
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date        Ticket#     Engineer    Description
 *  ----------  ----------  ----------- --------------------------
 *  20090513          2338  jsanchez    Initial creation.
 * </pre>
 * 
 * @author jsanchez
 * @version 1
 */
public class ParamLookup {
    private static TreeMap<Float, String> PRECIP_MAP = new TreeMap<Float, String>();
    private static TreeMap<Float, String> VIS_MAP = new TreeMap<Float, String>();
    static { 
        VIS_MAP.put(0.0000f,"0");
        VIS_MAP.put(0.03125f,"0");
        VIS_MAP.put(0.09375f,"1/16");
        VIS_MAP.put(0.15625f,"1/8");
        VIS_MAP.put(0.21875f,"3/16");
        VIS_MAP.put(0.28125f,"1/4");
        VIS_MAP.put(0.34375f,"5/16");
        VIS_MAP.put(0.40625f,"3/8");
        VIS_MAP.put(0.46875f,"7/16");
        VIS_MAP.put(0.53125f,"1/2");
        VIS_MAP.put(0.59375f,"9/16");
        VIS_MAP.put(0.65625f,"5/8");
        VIS_MAP.put(0.71875f,"11/16");
        VIS_MAP.put(0.78125f,"3/4");
        VIS_MAP.put(0.84375f,"13/16");
        VIS_MAP.put(0.90625f,"7/8");
        VIS_MAP.put(0.96875f,"15/16");
        VIS_MAP.put(1.03125f,"1");
        VIS_MAP.put(1.09375f,"1~1/16");
        VIS_MAP.put(1.15625f,"1~1/8");
        VIS_MAP.put(1.21875f,"1~3/16");
        VIS_MAP.put(1.28125f,"1~1/4");
        VIS_MAP.put(1.34375f,"1~5/16");
        VIS_MAP.put(1.40625f,"1~3/8");
        VIS_MAP.put(1.46875f,"1~7/16");
        VIS_MAP.put(1.53125f,"1~1/2");
        VIS_MAP.put(1.59375f,"1~9/16");
        VIS_MAP.put(1.65625f,"1~5/8");
        VIS_MAP.put(1.71875f,"1~11/16");
        VIS_MAP.put(1.78125f,"1~3/4");
        VIS_MAP.put(1.84375f,"1~13/16");
        VIS_MAP.put(1.90625f,"1~7/8");
        VIS_MAP.put(1.96875f,"1~15/16");
        VIS_MAP.put(2.0625f,"2");
        VIS_MAP.put(2.1875f,"2~1/8");
        VIS_MAP.put(2.3125f,"2~1/4");
        VIS_MAP.put(2.4375f,"2~3/8");
        VIS_MAP.put(2.5625f,"2~1/2");
        VIS_MAP.put(2.6875f,"2~5/8");
        VIS_MAP.put(2.8125f,"2~3/4");
        VIS_MAP.put(2.9375f,"2~7/8");
        VIS_MAP.put(3.0625f,"3");
        VIS_MAP.put(3.1875f,"3~1/8");
        VIS_MAP.put(3.3125f,"3~1/4");
        VIS_MAP.put(3.4375f,"3~3/8");
        VIS_MAP.put(3.5625f,"3~1/2");
        VIS_MAP.put(3.6875f,"3~5/8");
        VIS_MAP.put(3.8125f,"3~3/4");
        VIS_MAP.put(3.9375f,"3~7/8");
        VIS_MAP.put(4.125f,"4");
        VIS_MAP.put(4.375f,"4~1/4");
        VIS_MAP.put(4.625f,"4~1/2");
        VIS_MAP.put(4.875f,"4~3/4");
        VIS_MAP.put(5.125f,"5");
        VIS_MAP.put(5.375f,"5~1/4");
        VIS_MAP.put(5.625f,"5~1/2");
        VIS_MAP.put(5.875f,"5~3/4");
        VIS_MAP.put( 6.5f,"6");
        VIS_MAP.put(.5f,"7");
        VIS_MAP.put(8.5f,"8");
        VIS_MAP.put(9.5f,"9");
        VIS_MAP.put(0.5f,"10");
        VIS_MAP.put(11.5f,"11");
        VIS_MAP.put(12.5f,"12");
        VIS_MAP.put(13.5f,"13");
        VIS_MAP.put(14.5f,"14");
        VIS_MAP.put(15.5f,"15");
        VIS_MAP.put(16.5f,"16");
        VIS_MAP.put(17.5f,"17");
        VIS_MAP.put(18.5f,"18");
        VIS_MAP.put(19.5f,"19");
        VIS_MAP.put(22.5f,"20");
        VIS_MAP.put(27.5f,"25");
        VIS_MAP.put(32.5f,"30");
        VIS_MAP.put(37.5f,"35");
        VIS_MAP.put(42.5f,"40");
        VIS_MAP.put(47.5f,"45");
        VIS_MAP.put(52.5f,"50");
        VIS_MAP.put(57.5f,"55");
        VIS_MAP.put(62.5f,"60");
        VIS_MAP.put(67.5f,"65");
        VIS_MAP.put(72.5f,"70");
        VIS_MAP.put(77.5f,"75");
        VIS_MAP.put(82.5f,"80");
        VIS_MAP.put(87.5f,"85");
        VIS_MAP.put(92.5f,"90");
        VIS_MAP.put(97.5f,"95");
        VIS_MAP.put(110f,"100");
        
        PRECIP_MAP.put(-0.005f,"T");
        PRECIP_MAP.put(0.005f,"T");
        PRECIP_MAP.put(0.015f,".01");
        PRECIP_MAP.put(0.025f,".02");
        PRECIP_MAP.put(0.035f,".03");
        PRECIP_MAP.put(0.045f,".04");
        PRECIP_MAP.put(0.055f,".05");
        PRECIP_MAP.put(0.065f,".06");
        PRECIP_MAP.put(0.075f,".07");
        PRECIP_MAP.put(0.085f,".08");
        PRECIP_MAP.put(0.095f,".09");
    }

    private static String getVisibiiltyFormat(float vis) {
        String retVal = "";
        if (vis < 97.5) {
            retVal = VIS_MAP.ceilingEntry(vis).getValue();
        } else if (vis <= 110) {
            retVal = VIS_MAP.floorEntry(vis).getValue();
        } 

        return retVal;
    }
    
    private static String getPrecipFormat(double precip) {
        String retVal = "";
        if (precip < 0.085) {
            retVal = PRECIP_MAP.ceilingEntry((float) precip).getValue();
        } else if (precip < 0.095) {
            retVal = PRECIP_MAP.floorEntry((float) precip).getValue();
        } else {
            DecimalFormat p = new DecimalFormat("####.00");
            retVal = p.format(precip);
        }

        return retVal;
    }
    
    public static String get(String param, String value) {
        String retVal = value;
        
        if (param.matches("VIS")) {
            retVal = getVisibiiltyFormat(Float.parseFloat(value));
        } else if (param.startsWith("PR")) {
            retVal = getPrecipFormat(Double.parseDouble(value));
        }
        return retVal;
    }
    
}

