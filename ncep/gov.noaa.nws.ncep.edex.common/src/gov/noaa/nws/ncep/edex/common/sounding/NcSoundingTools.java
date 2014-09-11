package gov.noaa.nws.ncep.edex.common.sounding;

import java.util.List;

/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTools
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------      -------     --------    -----------
 * 07/24/2014               Chin Chen   Initial coding 
 *                                      Support PW computation
 * 
 * 
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
public class NcSoundingTools {

    /*************************************************************
     * PRECIP_WATER Calculates the Precipitation Water in mm from Bottom level
     * of layer (mb) to Top level of layer (mb)
     *************************************************************/
    public static float precip_water(List<NcSoundingLayer> sndlayers) {
        float pw = 0;
        float d1, p1, d2, p2, tot, w1, w2, wbar;

        // ----- Start with interpolated bottom layer -----
        // find surface layer or first layer with valid dewpoint
        int sfcIndex = 0;
        for (int i = 0; i < sndlayers.size(); i++) {
            if (sndlayers.get(i).getDewpoint() != -9999f) {
                sfcIndex = i;
                break;
            }
        }
        d1 = sndlayers.get(sfcIndex).getDewpoint(); // dewp
                                                    // in
                                                    // C
        p1 = sndlayers.get(sfcIndex).getPressure(); // pressure
                                                    // n
                                                    // mb

        tot = 0;
        for (int i = sfcIndex + 1; i < sndlayers.size(); i++) {
            /* ----- Calculate every level that reports a dwpt ----- */
            d2 = sndlayers.get(i).getDewpoint(); // dewp
                                                 // in C
            if (d2 == -9999f)
                continue;
            p2 = sndlayers.get(i).getPressure(); // pressure
                                                 // n mb
            w1 = mixingRatio(d1, p1);
            w2 = mixingRatio(d2, p2);
            wbar = (w1 + w2) / 2;
            tot = tot + wbar * (p1 - p2);
            // System.out.println("p1=" + p1 + " d1=" + d1 + " p2=" + p2 +
            // " d2="
            // + d2);
            d1 = d2;
            p1 = p2;
            // test the case when top level is 400 mb
            // if (p2 == 400)
            // break;
        }

        /* ----- Convert to mm (from g*mb/kg) ----- */
        pw = tot * 0.00040173f * 25.4f;

        return pw;
    }

    public static float precip_water2(List<NcSoundingLayer2> sndlayers) {
        float pw = 0;
        float d1, p1, d2, p2, tot, w1, w2, wbar;
        if (sndlayers == null || sndlayers.size() <= 0)
            return 0;
        // ----- Start with interpolated bottom layer -----
        // find surface layer or first layer with valid dewpoint
        int sfcIndex = 0;
        for (int i = 0; i < sndlayers.size(); i++) {
            if (sndlayers.get(i).getDewpoint().getValue().floatValue() != -9999f) {
                sfcIndex = i;
                break;
            }
        }
        d1 = sndlayers.get(sfcIndex).getDewpoint().getValue().floatValue(); // dewp
                                                                            // in
                                                                            // C
        p1 = sndlayers.get(sfcIndex).getPressure().getValue().floatValue(); // pressure
                                                                            // n
                                                                            // mb

        tot = 0;
        for (int i = sfcIndex + 1; i < sndlayers.size(); i++) {
            /* ----- Calculate every level that reports a dwpt ----- */
            d2 = sndlayers.get(i).getDewpoint().getValue().floatValue(); // dewp
                                                                         // in C
            if (d2 == -9999f)
                continue;
            p2 = sndlayers.get(i).getPressure().getValue().floatValue(); // pressure
                                                                         // n mb
            w1 = mixingRatio(d1, p1);
            w2 = mixingRatio(d2, p2);
            wbar = (w1 + w2) / 2;
            tot = tot + wbar * (p1 - p2);
            d1 = d2;
            p1 = p2;
            // test the case when top level is 400 mb
            // if (p2 == 400)
            // break;
        }

        /* ----- Convert to mm (from g*mb/kg) ----- */
        pw = tot * 0.00040173f * 25.4f;

        return pw;
    }

    /*
     * Compute mixing ratio from DWPC and PRES. Chin: copy from
     * gov.noaa.nws.ncep.edex.uengine.tasks.profile.MergeSounding
     */
    private static float mixingRatio(float td, float pres) {
        float vapr = vaporPressure(td);

        float corr = (1.001f + ((pres - 100.f) / 900.f) * .0034f);

        float e = corr * vapr;
        if (e > (.5f * pres)) {
            return -9999f;
        } else {
            return .62197f * (e / (pres - e)) * 1000.f;
        }
    }

    /*
     * Compute vapor pressure from DWPC. Chin: copy from
     * gov.noaa.nws.ncep.edex.uengine.tasks.profile.MergeSounding
     */
    private static float vaporPressure(float td) {
        return (6.112f * (float) Math.exp((17.67 * td) / (td + 243.5)));
    }

    // The followings are converted from BigSharp, the computation results are
    // about the same as above methods.
    // private static float mixratio(float pres, float temp)
    //
    // {
    // float x, wfw, fwesw;
    //
    // x = 0.02f * (temp - 12.5f + 7500.0f / pres);
    // wfw = 1.0f + 0.0000045f * pres + 0.0014f * x * x;
    // fwesw = wfw * vappres(temp);
    // return 621.97f * (fwesw / (pres - fwesw));
    // }
    //
    // private static float vappres(float temp)
    //
    // {
    // double pol;
    // pol = temp * (1.1112018e-17 + temp * (-3.0994571e-20));
    // pol = temp * (2.1874425e-13 + temp * (-1.789232e-15 + pol));
    // pol = temp * (4.3884180e-09 + temp * (-2.988388e-11 + pol));
    // pol = temp * (7.8736169e-05 + temp * (-6.111796e-07 + pol));
    // pol = .99999683e-00 + temp * (-9.082695e-03 + pol);
    // pol = (pol * pol);
    // pol = (pol * pol);
    // return (6.1078f / (float) (pol * pol));
    // }

}
