/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling
 * 
 * This java class performs the NSHARP NsharpDataHandling functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

package gov.noaa.nws.ncep.ui.nsharp.natives;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

//Chin-T import com.raytheon.uf.common.sounding.SoundingLayer;

public class NsharpDataHandling {

    public static short qc(float value)
    /*************************************************************/
    /* QC */
    /* John Hart NSSFC KCMO */
    /*                                                           */
    /* Quality control of sndg data. Searches for missing */
    /* data (-999) and returns (1 = OK), (0 = missing) */
    /*************************************************************/
    {
        if (value < -998.0F) {
            return 0;
        }
        if (value > 2.0E+05F) {
            return 0;
        }
        return 1;
    }

    public static float vcomp(float wdir, float wspd)
    /*************************************************************/
    /* VCOMP */
    /* John Hart NSSFC KCMO */
    /*                                                           */
    /* Calculates a v-component of the wind (kt), given */
    /* a direction and speed. */
    /*                                                           */
    /* wdir - Wind Direction (deg) */
    /* wspd - Wind Speed (kt) */
    /*************************************************************/
    {
        while (wdir > 360) {
            wdir = wdir - 360;
        }
        wdir = wdir * NsharpNativeConstants.PI / 180.0F;
        return wspd * (float) Math.cos(wdir);
    }

    public static float ucomp(float wdir, float wspd)
    /*************************************************************/
    /* UCOMP */
    /* John Hart NSSFC KCMO */
    /*                                                           */
    /* Calculates a u-component of the wind (kt), given */
    /* a direction and speed. */
    /*                                                           */
    /* wdir - Wind Direction (deg) */
    /* wspd - Wind Speed (kt) */
    /*************************************************************/
    {
        while (wdir > 360) {
            wdir = wdir - 360;
        }
        wdir = wdir * NsharpNativeConstants.PI / 180F;
        return wspd * (float) Math.sin(wdir);
    }

    public static float i_wndu(float pres, List<NcSoundingLayer> sndLys)
    /*************************************************************/
    /* Chin: port it from */
    /* I_WNDU basics.c */
    /* John Hart NSSFC KCMO */
    /*                                                           */
    /* Interpolates the given data to calculate a */
    /* u-component to the wind at pressure level (pres). */
    /*                                                           */
    /* pres - Level(mb) to compute a U-Component */
    /*************************************************************/
    {
        int below, above, i, ok;
        float utop, ubot, nm1;

        below = 0;
        above = 0;

        /* ----- Find Wind Immediately Above level ----- */
        ok = 0;
        for (i = 0; i < sndLys.size(); i++) {
            if ((sndLys.get(i).getPressure() == pres)
                    && (sndLys.get(i).getWindDirection() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (sndLys.get(i).getWindSpeed() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA))
                return ucomp(sndLys.get(i).getWindDirection(), sndLys.get(i)
                        .getWindSpeed());
            if ((sndLys.get(i).getPressure() < pres)
                    && (sndLys.get(i).getWindDirection() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                above = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- Find Wind Immediately Below level ----- */
        ok = 0;
        for (i = sndLys.size() - 1; i > -1; i--) {
            if ((sndLys.get(i).getPressure() >= pres)
                    && (sndLys.get(i).getWindDirection() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                below = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- If both levels are the same, return them ---- */
        if (above == below) {
            return ucomp(sndLys.get(above).getWindDirection(), sndLys
                    .get(above).getWindSpeed());
        }

        /* ----- Now we need to interpolate to get the Wind ----- */
        nm1 = sndLys.get(below).getPressure() - pres;
        ubot = ucomp(sndLys.get(below).getWindDirection(), sndLys.get(below)
                .getWindSpeed());
        utop = ucomp(sndLys.get(above).getWindDirection(), sndLys.get(above)
                .getWindSpeed());
        return ubot
                - (nm1 / (sndLys.get(below).getPressure() - sndLys.get(above)
                        .getPressure())) * (ubot - utop);
    }

    public static float i_wndv(float pres, List<NcSoundingLayer> sndLys)
    /*************************************************************/
    /* Chin: port it from */
    /* I_WNDV basics.c */
    /* John Hart NSSFC KCMO */
    /*                                                           */
    /* Interpolates the given data to calculate a */
    /* v-component to the wind at pressure level (pres). */
    /*                                                           */
    /* pres - Level(mb) to compute a V-Component */
    /*************************************************************/
    {
        int below, above, i, ok;
        float vtop, vbot, nm1;

        below = 0;
        above = 0;

        /* ----- Find Wind Immediately Above level ----- */
        ok = 0;
        for (i = 0; i < sndLys.size(); i++) {
            if ((sndLys.get(i).getPressure() == pres)
                    && (sndLys.get(i).getWindDirection() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (sndLys.get(i).getWindSpeed() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA))
                return vcomp(sndLys.get(i).getWindDirection(), sndLys.get(i)
                        .getWindSpeed());
            if ((sndLys.get(i).getPressure() < pres)
                    && (sndLys.get(i).getWindDirection() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                above = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- Find Wind Immediately Below level ----- */
        ok = 0;
        for (i = sndLys.size() - 1; i > -1; i--) {
            if ((sndLys.get(i).getPressure() >= pres)
                    && (sndLys.get(i).getWindDirection() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                below = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- If both levels are the same, return them ---- */
        if (above == below) {
            return vcomp(sndLys.get(above).getWindDirection(), sndLys
                    .get(above).getWindSpeed());
        }

        /* ----- Now we need to interpolate to get the Wind ----- */
        nm1 = sndLys.get(below).getPressure() - pres;
        vbot = vcomp(sndLys.get(below).getWindDirection(), sndLys.get(below)
                .getWindSpeed());
        vtop = vcomp(sndLys.get(above).getWindDirection(), sndLys.get(above)
                .getWindSpeed());
        return vbot
                - ((nm1 / (sndLys.get(below).getPressure() - sndLys.get(above)
                        .getPressure())) * (vbot - vtop));
    }

    public static float angle(float u, float v)
    /*************************************************************/
    /* Chin: port it from */
    /* ANGLE winds.c */
    /* John Hart NSSFC KCMO */
    /*                                                           */
    /* Calculates an angle (deg) of the wind (u,v). */
    /*                                                           */
    /* u - U-Component (kt) */
    /* v - V-Component (kt) */
    /*************************************************************/
    {
        double sc, t1;
        sc = NsharpNativeConstants.PI / 180;
        if ((u == 0) && (v == 0))
            return 0;
        if ((u == 0) && (v > 0))
            return 360;
        if ((u == 0) && (v < 0))
            return 180;
        t1 = Math.atan(v / u) / sc;
        if (u >= 0) {
            return (float) (90 - t1);
        } else {
            return (float) (270 - t1);
        }
    }

    public static float i_wdir(float pres, List<NcSoundingLayer> sndLys)
    /*************************************************************/
    /* Chin: port it from */
    /* I_WDIR basics.c */
    /* John Hart NSSFC KCMO */
    /*                                                           */
    /* Interpolates the given data to calculate a wind */
    /* direction (deg) at pressure level (pres). */
    /*                                                           */
    /* pres - Level(mb) to compute a Wind */
    /*************************************************************/
    {
        float u, v;

        if (qc(i_wndu(pres, sndLys)) == 0 || qc(i_wndv(pres, sndLys)) == 0) {
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;
        }
        u = i_wndu(pres, sndLys);
        v = i_wndv(pres, sndLys);
        return angle(u, v);
    }

    /*
     * ==========================================================================
     * ===
     */

    public static float i_wspd(float pres, List<NcSoundingLayer> sndLys) {
        /*************************************************************/
        /* Chin: port it from */
        /* I_WSPD basics.c */
        /* John Hart NSSFC KCMO */
        /*                                                           */
        /* Interpolates the given data to calculate a wind */
        /* magnitude (kt) at pressure level (pres). */
        /*                                                           */
        /* pres - Level(mb) to compute a Wind */
        /*************************************************************/

        float u, v;

        if (qc(i_wndu(pres, sndLys)) == 0 || qc(i_wndv(pres, sndLys)) == 0) {
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;
        }

        u = i_wndu(pres, sndLys);
        v = i_wndv(pres, sndLys);
        return (float) (Math.sqrt((u * u) + (v * v)));
    }

    public static float i_temp(float pres, List<NcSoundingLayer> sndLys) {
        /*************************************************************/
        /* Chin: port it from */
        /* I_TEMP basics.c */
        /* John Hart NSSFC KCMO */
        /*                                                           */
        /* Interpolates the given data to calculate a temperature. */
        /* at pressure level (pres). */
        /*                                                           */
        /* pres - Level(mb) to compute a temperature */
        /*************************************************************/

        int below, above, i, ok;
        double nm1, nm2, nm4;

        below = 0;
        above = 0;

        /* ----- Find Temperature Immediately Above level ----- */
        ok = 0;
        for (i = 0; i < sndLys.size(); i++) {
            if ((sndLys.get(i).getPressure() == pres)
                    && (sndLys.get(i).getTemperature() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                /*
                 * if pressure level exists, and temp exists, no need to
                 * interpolate
                 */
                return sndLys.get(i).getTemperature();
            }
            if ((sndLys.get(i).getPressure() < pres)
                    && (sndLys.get(i).getTemperature() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                above = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- Find Temperature Immediately Below level ----- */
        ok = 0;
        for (i = sndLys.size() - 1; i > -1; i--) {
            if ((sndLys.get(i).getPressure() >= pres)
                    && (sndLys.get(i).getTemperature() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                below = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- If both levels are the same, return them ---- */
        if (above == below) {
            return sndLys.get(above).getTemperature();
        }

        /* ----- Now we need to interpolate to get the temperature ----- */

        nm1 = sndLys.get(above).getTemperature()
                - sndLys.get(below).getTemperature();
        nm2 = Math.log(sndLys.get(below).getPressure()
                / sndLys.get(above).getPressure());
        nm4 = Math.log(sndLys.get(below).getPressure() / pres);
        return (float) (sndLys.get(below).getTemperature() + ((nm4 / nm2) * nm1));
    }

    /*
     * ==========================================================================
     * ===
     */

    public static float i_dwpt(float pres, List<NcSoundingLayer> sndLys) {
        /*************************************************************/
        /* Chin: port it from */
        /* I_DWPT basics.c */
        /* John Hart NSSFC KCMO */
        /*                                                           */
        /* Interpolates the given data to calculate a dew point */
        /* at pressure level (pres). */
        /*                                                           */
        /* pres - Level(mb) to compute a Dew Point */
        /*************************************************************/

        int below, above, i, ok;
        double nm1, nm2, nm4;
        below = 0;
        above = 0;

        /* ----- Find Dew Point Immediately Above level ----- */
        ok = 0;
        for (i = 0; i < sndLys.size(); i++) {
            if ((sndLys.get(i).getPressure() == pres)
                    && (sndLys.get(i).getDewpoint() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (sndLys.get(i).getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                /*
                 * no need to interpolate if we have the level and dewpoint
                 * exists
                 */
                return sndLys.get(i).getDewpoint();
            }
            if ((sndLys.get(i).getPressure() < pres)
                    && (sndLys.get(i).getDewpoint() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                above = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- Find Dew Point Immediately Below level ----- */
        ok = 0;
        for (i = sndLys.size() - 1; i > -1; i--) {
            if ((sndLys.get(i).getPressure() >= pres)
                    && (sndLys.get(i).getDewpoint() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                below = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- If both levels are the same, return them ---- */
        if (above == below) {
            return sndLys.get(above).getDewpoint();
        }

        /* ----- Now we need to interpolate to get the dew point ----- */
        nm1 = sndLys.get(above).getDewpoint() - sndLys.get(below).getDewpoint();
        nm2 = Math.log(sndLys.get(below).getPressure()
                / sndLys.get(above).getPressure());
        nm4 = Math.log(sndLys.get(below).getPressure() / pres);
        return (float) (sndLys.get(below).getDewpoint() + ((nm4 / nm2) * nm1));
    }

    public static float i_pres(float hght, List<NcSoundingLayer> sndLys,
            int index) {
        /*************************************************************/
        /* Chin: port it from */
        /* I_PRES basics.c */
        /* John Hart NSSFC KCMO */
        /*                                                           */
        /* Interpolates the given data to calculate a pressure(mb) */
        /* at height (hght). */
        /*                                                           */
        /* hght - Height(m) of level */
        /*************************************************************/

        int below, above, i, ok;
        double nm1, nm2, nm3;

        below = 0;
        above = 0;

        /* ----- Find Pressure Immediately Above level ----- */
        ok = 0;
        for (i = 0; i < sndLys.size(); i++) {
            /* if we have the level, no need to interpolate pressure */
            if ((sndLys.get(i).getGeoHeight() == hght)
                    && (sndLys.get(i).getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                index = i;
                return (sndLys.get(i).getPressure());
            }
            if ((sndLys.get(i).getGeoHeight() > hght)
                    && (sndLys.get(i).getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                above = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- Find Pressure Below level ----- */
        ok = 0;
        for (i = sndLys.size() - 1; i > -1; i--) {
            if ((sndLys.get(i).getGeoHeight() <= hght)
                    && (sndLys.get(i).getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                below = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- If both levels are the same, return them ---- */
        if (above == below) {
            index = below;
            return sndLys.get(above).getPressure();
        }

        /* ----- Now we need to interpolate to get the Pressure ----- */
        nm1 = hght - sndLys.get(below).getGeoHeight();
        nm2 = sndLys.get(above).getGeoHeight()
                - sndLys.get(below).getGeoHeight();
        nm3 = Math.log(sndLys.get(above).getPressure()
                / sndLys.get(below).getPressure());
        index = below;
        return (float) (sndLys.get(below).getPressure() * Math.exp((nm1 / nm2)
                * nm3));
    }

    public static float i_hght(float pres, List<NcSoundingLayer> sndLys,
            int index) {
        /*************************************************************/
        /* Chin: port it from */
        /* I_HGHT basics.c */
        /* John Hart NSSFC KCMO */
        /*                                                           */
        /* Interpolates the given data to calculate a height */
        /* at pressure level (pres). */
        /*                                                           */
        /* pres - Level(mb) to compute a Height */
        /*************************************************************/

        int below, above, i, ok;
        double nm1, nm2, nm4;

        below = 0;
        above = 0;

        /* ----- Find Height Immediately Above level ----- */
        ok = 0;
        for (i = 0; i < sndLys.size(); i++) {
            if ((sndLys.get(i).getPressure() == pres)
                    && (sndLys.get(i).getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                /* no need to interpolate if we have the level!!! */
                index = i;
                return sndLys.get(i).getGeoHeight();
            }
            if ((sndLys.get(i).getPressure() < pres)
                    && (sndLys.get(i).getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (sndLys.get(i).getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                above = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- Find Height Immediately Below level ----- */
        ok = 0;
        for (i = sndLys.size() - 1; i > -1; i--) {
            if ((sndLys.get(i).getPressure() >= pres)
                    && (sndLys.get(i).getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                below = i;
                ok = 1;
                break;
            }
        }
        if (ok == 0)
            return NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

        /* ----- If both levels are the same, return them ---- */
        if (above == below) {
            index = below;
            return sndLys.get(above).getGeoHeight();
        }

        /* ----- Now we need to interpolate to get the height ----- */
        nm1 = sndLys.get(above).getGeoHeight()
                - sndLys.get(below).getGeoHeight();
        nm2 = Math.log(sndLys.get(below).getPressure()
                / sndLys.get(above).getPressure());
        nm4 = Math.log(sndLys.get(below).getPressure() / pres);
        index = below;
        return (float) (sndLys.get(below).getGeoHeight() + ((nm4 / nm2) * nm1));
    }

    // compare two layers based on reverse height, pressure, then wind
    public static Comparator<NcSoundingLayer> reverseHeightPressureWindComparator() {

        return new Comparator<NcSoundingLayer>() {

            @Override
            public int compare(NcSoundingLayer layerA, NcSoundingLayer layerB) {
                int retValue = 0;
                if (layerA != layerB) {
                    // reverse sort relative to height!
                    retValue = Double.compare(layerB.getGeoHeight(),
                            layerA.getGeoHeight());

                    if (retValue == 0) {
                        // the two layers have same height. sort them by
                        // pressure
                        retValue = Double.compare(layerA.getPressure(),
                                layerB.getPressure());

                        if (retValue == 0) {
                            // the two layers have same height. sort them by
                            // wind
                            retValue = Double.compare(layerB.getWindSpeed(),
                                    layerA.getWindSpeed());
                        }
                    }
                }
                return retValue;
            }
        };
    }

    // compare two layers based on reverse pressure, height, then wind
    public static Comparator<NcSoundingLayer> reversePressureHeightWindComparator() {

        return new Comparator<NcSoundingLayer>() {

            @Override
            public int compare(NcSoundingLayer layerA, NcSoundingLayer layerB) {
                int retValue = 0;
                if (layerA != layerB) {
                    // reverse sort relative to pressure!
                    retValue = Double.compare(layerB.getPressure(),
                            layerA.getPressure());

                    if (retValue == 0) {
                        // the two layers have same height. sort them by height
                        retValue = Double.compare(layerA.getGeoHeight(),
                                layerB.getGeoHeight());

                        if (retValue == 0) {
                            // the two layers have same height. sort them by
                            // wind
                            retValue = Double.compare(layerB.getWindSpeed(),
                                    layerA.getWindSpeed());
                        }
                    }
                }
                return retValue;
            }
        };
    }

    // compare two layers based on reverse pressure,
    public static Comparator<NcSoundingLayer> reversePressureComparator() {

        return new Comparator<NcSoundingLayer>() {

            @Override
            public int compare(NcSoundingLayer layerA, NcSoundingLayer layerB) {
                int retValue = 0;
                if (layerA != layerB) {
                    // reverse sort relative to pressure!
                    retValue = Double.compare(layerB.getPressure(),
                            layerA.getPressure());
                }
                return retValue;
            }
        };
    }

    // replace all NaN value with -9999
    private static List<NcSoundingLayer> replaceNanSoundingData(
            List<NcSoundingLayer> soundingLys) {
        for (NcSoundingLayer layer : soundingLys) {
            if (Float.isNaN(layer.getGeoHeight()) == true) {
                layer.setGeoHeight(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            }
            if (Float.isNaN(layer.getPressure()) == true) {
                layer.setPressure(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            }
            if (Float.isNaN(layer.getTemperature()) == true) {
                layer.setTemperature(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            }
            if (Float.isNaN(layer.getDewpoint()) == true) {
                layer.setDewpoint(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            }
            if (Float.isNaN(layer.getWindDirection()) == true) {
                layer.setWindDirection(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            }
            if (Float.isNaN(layer.getWindSpeed()) == true) {
                layer.setWindSpeed(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            }
            if (Float.isNaN(layer.getOmega()) == true) {
                layer.setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            }
        }
        return soundingLys;
    }

    public static List<NcSoundingLayer> removeInvalidSoundingData(
            List<NcSoundingLayer> soundingLys) {
        List<NcSoundingLayer> sndLysLst = soundingLys;
        List<Integer> removingItemList = new ArrayList<Integer>();
        int i = 0;
        sndLysLst = replaceNanSoundingData(sndLysLst);
        for (NcSoundingLayer layer : sndLysLst) {
            /*
             * if (layer.getWindDirection() < 0 ){ removingItemList.add(i); }
             * else if (layer.getWindSpeed() < 0 ){ removingItemList.add(i); }
             * else
             */if (layer.getGeoHeight() < -50) {
                removingItemList.add(i);
            } else if (layer.getPressure() < 0) {
                removingItemList.add(i);
            } else if (layer.getTemperature() < layer.getDewpoint()) {
                removingItemList.add(i);
            }
            /*
             * else if (layer.getTemperature() < -300 ){
             * removingItemList.add(i); } else if (layer.getDewpoint() < -300 ){
             * removingItemList.add(i); }
             */
            i++;

        }
        // remove data with missing data
        Collections.reverse(removingItemList);
        for (i = 0; i < removingItemList.size(); i++) {
            sndLysLst.remove(removingItemList.get(i).intValue());
        }
        return sndLysLst;
    }

    public static List<NcSoundingLayer> updateObsSoundingDataForShow(
            List<NcSoundingLayer> soundingLys, float stnElv) {
        List<NcSoundingLayer> sndLysLst = soundingLys;
        List<Integer> removingItemList = new ArrayList<Integer>();
        int i = 0;
        // reset missing data to NSHARP_NATIVE_UNVALID_DATA
        for (NcSoundingLayer layer : sndLysLst) {
            // if( (layer.getPressure() < 100 )&&(layer.getPressure() >=0 ))
            // System.out.println("pressure below 100 "+ layer.getPressure() +
            // " height " + layer.getGeoHeight() );
            if (layer.getWindDirection() < 0)
                layer.setWindDirection(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getWindSpeed() < 0)
                layer.setWindSpeed(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getGeoHeight() < -50)
                layer.setGeoHeight(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getPressure() < 0)
                layer.setPressure(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getTemperature() < -300)
                layer.setTemperature(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getDewpoint() < -300)
                layer.setDewpoint(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getOmega() < -100)
                layer.setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);

            // if both pressure and height are missing, remove this layer
            if ((layer.getPressure() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (layer.getGeoHeight() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA))
                removingItemList.add(i);

            i++;
        }
        // remove data with missing pressure and height
        Collections.reverse(removingItemList);
        for (i = 0; i < removingItemList.size(); i++) {
            sndLysLst.remove(removingItemList.get(i).intValue());
        }

        // sort layers based on pressure, height, wind and list them from
        // highest pressure
        Collections.sort(sndLysLst, reverseHeightPressureWindComparator());

        /*
         * remove duplicate data with same pressure, or same hieght or pressure
         * less than 100
         */
        // create duplicate list
        removingItemList.clear();
        for (i = 0; i < sndLysLst.size() - 1; i++) {
            if (((sndLysLst.get(i).getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) && (sndLysLst
                    .get(i).getPressure() == sndLysLst.get(i + 1).getPressure()))
                    || ((sndLysLst.get(i).getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) && (sndLysLst
                            .get(i).getGeoHeight() == sndLysLst.get(i + 1)
                            .getGeoHeight()))) {
                removingItemList.add(i + 1);
                continue;
            }
        }

        Collections.reverse(removingItemList);

        for (i = 0; i < removingItemList.size(); i++) {

            sndLysLst.remove(removingItemList.get(i).intValue());
        }

        // create 3 layer list for interpolation.
        List<NcSoundingLayer> finalLst = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> presLst = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> heightLst = new ArrayList<NcSoundingLayer>();
        for (NcSoundingLayer layer : sndLysLst) {
            // List one, layer with both pressure and height
            if ((layer.getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (layer.getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                finalLst.add(0, layer);
            }
            // list two, layer with pressure only
            if ((layer.getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (layer.getGeoHeight() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                presLst.add(layer);
            }
            // list three, layer with height only
            if ((layer.getPressure() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    && (layer.getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)) {
                heightLst.add(layer);
            }
        }
        /*
         * System.out.println("pressure list"); for (SoundingLayer soundingLy :
         * presLst){ System.out.println(" pressure "+ soundingLy.getPressure() +
         * " height "+ soundingLy.getGeoHeight() ); }
         * System.out.println("height list"); for (SoundingLayer soundingLy :
         * heightLst){ System.out.println(" pressure "+ soundingLy.getPressure()
         * + " height "+ soundingLy.getGeoHeight() ); }
         * System.out.println("final list"); for (NcSoundingLayer soundingLy :
         * finalLst){ System.out.println(" pressure "+ soundingLy.getPressure()
         * + " height "+ soundingLy.getGeoHeight() +" temp "
         * +soundingLy.getTemperature() + " windS " +
         * soundingLy.getWindSpeed()); }
         */

        // Interpolates height layer with missing pressure.
        for (NcSoundingLayer soundingLy : heightLst) {
            // round up height
            soundingLy
                    .setGeoHeight((float) Math.rint(soundingLy.getGeoHeight()));
            int index = 0;
            float pressure = i_pres(soundingLy.getGeoHeight(), finalLst, index);
            if (pressure != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
                soundingLy.setPressure(pressure);
                // System.out.println("interpolated pressure "+
                // soundingLy.getPressure() + " height "+
                // soundingLy.getGeoHeight() );
                finalLst.add(index, soundingLy);
            }
        }

        Collections.sort(finalLst, reversePressureHeightWindComparator());

        // System.out.println("Height layer with pressure interpolation");
        // /for (NcSoundingLayer soundingLy : heightLst){
        // System.out.println(" pressure "+ soundingLy.getPressure() +
        // " height "+ soundingLy.getGeoHeight() );
        // }

        // Interpolates pressure layer with missing height.
        for (NcSoundingLayer soundingLy : presLst) {
            int index = 0;
            float ht = i_hght(soundingLy.getPressure(), finalLst, index);
            if (ht != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
                soundingLy.setGeoHeight(ht);
                finalLst.add(index, soundingLy);
            }
        }
        Collections.sort(finalLst, reversePressureHeightWindComparator());

        // System.out.println(" new final list");
        // for (NcSoundingLayer soundingLy : finalLst){
        // System.out.println(" pressure "+ soundingLy.getPressure() +
        // " height "+ soundingLy.getGeoHeight() +
        // " temp " +soundingLy.getTemperature()+ " windS " +
        // soundingLy.getWindSpeed());
        // }
        /*
         * remove duplicate data with same pressure AGAIN
         */
        // create duplicate list
        removingItemList.clear();
        for (i = 0; i < finalLst.size() - 1; i++) {
            if (finalLst.get(i).getPressure() == finalLst.get(i + 1)
                    .getPressure()) {
                removingItemList.add(i + 1);
                continue;
            }
        }
        Collections.reverse(removingItemList);
        for (i = 0; i < removingItemList.size(); i++) {
            finalLst.remove(removingItemList.get(i).intValue());
        }

        // Interpolates missing temperature, dewpoint, wind speed and direction
        // using pressure
        // remove not interpolateable layers
        // Also remove layer with pressure below 100 except 50 and 75 mb layers.
        // removingItemList = new ArrayList<Integer>();
        removingItemList.clear();
        i = 0;
        float interpolatedValue;
        Boolean found75 = false, found50 = false;
        for (NcSoundingLayer soundingLy : finalLst) {
            /* remove layer with pressure below 100 */
            if (soundingLy.getPressure() < 100) {
                if (soundingLy.getPressure() == 75) {
                    found75 = true;
                } else if (soundingLy.getPressure() == 50) {
                    found50 = true;
                } else {
                    removingItemList.add(i);
                    i++;
                    continue;
                }

            }

            if (soundingLy.getTemperature() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {

                interpolatedValue = i_temp(soundingLy.getPressure(), finalLst);
                if (interpolatedValue != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    soundingLy.setTemperature(interpolatedValue);
                else {
                    // can not interpolate its temp, marked as invalid data
                    removingItemList.add(i);
                    i++;
                    continue;
                }
            }
            if (soundingLy.getDewpoint() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
                interpolatedValue = i_dwpt(soundingLy.getPressure(), finalLst);
                if (interpolatedValue != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    soundingLy.setDewpoint(interpolatedValue);
                else {
                    // can not interpolate its dew point, marked as invalid data
                    removingItemList.add(i);
                    i++;
                    continue;
                }
            }
            // because of i_wndu() and i_wndv() algorithm..we should call
            // i_wspd() first, then i_wdir()
            if (soundingLy.getWindSpeed() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
                interpolatedValue = i_wspd(soundingLy.getPressure(), finalLst);
                if (interpolatedValue != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    soundingLy.setWindSpeed(interpolatedValue);
                else {
                    // can not interpolate its wind speed, marked as invalid
                    // data
                    removingItemList.add(i);
                    i++;
                    continue;
                }
            }
            if (soundingLy.getWindDirection() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
                interpolatedValue = i_wdir(soundingLy.getPressure(), finalLst);
                if (interpolatedValue != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
                    soundingLy.setWindDirection(interpolatedValue);

                else {
                    // can not interpolate its wind direction, marked as invalid
                    // data
                    removingItemList.add(i);
                    i++;
                    continue;
                }
            }
            i++;
        }
        // remove any invalid data still having missing data or pressure below
        // 100
        Collections.reverse(removingItemList);
        for (i = 0; i < removingItemList.size(); i++) {
            // System.out.println("removing "+
            // sndLys.get(dupItemList.get(i).intValue()).getPressure());
            finalLst.remove(removingItemList.get(i).intValue());
        }

        /*
         * Chin's NOTE: native nsharp extend sounding data to add 50 and 75 mb
         * layers. It uses 150mb layer's temp, dew, wind dir and wind speed. and
         * interpolate height only. see xtnd_sndg() at readdata.c for original c
         * code.
         */
        if (finalLst.size() > 2) {
            if (found75 == false) {
                double nm1, nm2, nm4;
                NcSoundingLayer soundingLy;
                int above1, above2;

                soundingLy = new NcSoundingLayer();
                soundingLy.setPressure(75);
                soundingLy.setTemperature(i_temp(150, finalLst));
                soundingLy.setDewpoint(i_dwpt(150, finalLst));
                soundingLy.setWindDirection(i_wdir(150, finalLst));
                soundingLy.setWindSpeed(i_wspd(150, finalLst));
                soundingLy
                        .setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
                // interpolate height for 75mb layer
                above1 = finalLst.size() - 1;
                above2 = finalLst.size() - 2;
                nm1 = finalLst.get(above1).getGeoHeight()
                        - finalLst.get(above2).getGeoHeight();
                nm2 = Math.log(finalLst.get(above2).getPressure()
                        / finalLst.get(above1).getPressure());
                nm4 = Math.log(finalLst.get(above2).getPressure() / 75);
                soundingLy.setGeoHeight((float) (finalLst.get(above2)
                        .getGeoHeight() + ((nm4 / nm2) * nm1)));
                if (found50 == true) {
                    finalLst.add(finalLst.size() - 1, soundingLy);
                } else
                    finalLst.add(soundingLy);
            }
            if (found50 == false) {
                double nm1, nm2, nm4;
                NcSoundingLayer soundingLy;
                int above1, above2;

                soundingLy = new NcSoundingLayer();
                soundingLy.setPressure(50);
                soundingLy.setTemperature(i_temp(150, finalLst));
                soundingLy.setDewpoint(i_dwpt(150, finalLst));
                soundingLy.setWindDirection(i_wdir(150, finalLst));
                soundingLy.setWindSpeed(i_wspd(150, finalLst));
                soundingLy
                        .setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
                // interpolate height for 50mb layer
                above1 = finalLst.size() - 1;
                above2 = finalLst.size() - 2;
                nm1 = finalLst.get(above1).getGeoHeight()
                        - finalLst.get(above2).getGeoHeight();
                nm2 = Math.log(finalLst.get(above2).getPressure()
                        / finalLst.get(above1).getPressure());
                nm4 = Math.log(finalLst.get(above2).getPressure() / 50);
                soundingLy.setGeoHeight((float) (finalLst.get(above2)
                        .getGeoHeight() + ((nm4 / nm2) * nm1)));
                finalLst.add(soundingLy);
            }
        }

        // System.out.println(" final final list");
        // for (NcSoundingLayer soundingLy : finalLst){
        // System.out.println(" pressure "+ soundingLy.getPressure() +
        // " height "+ soundingLy.getGeoHeight() +
        // " temp " +soundingLy.getTemperature() + " windS " +
        // soundingLy.getWindSpeed());
        // }
        // System.out.println("final size " + finalLst.size() + " stnElv " +
        // stnElv);
        return finalLst;

    }

    // This api is called when user wnat to show raw data
    public static List<NcSoundingLayer> sortObsSoundingDataForShow(
            List<NcSoundingLayer> soundingLys, float stnElv) {
        List<NcSoundingLayer> sndLysLst = soundingLys;
        List<Integer> removingItemList = new ArrayList<Integer>();
        int i = 0;
        // reset missing data to NSHARP_NATIVE_UNVALID_DATA
        for (NcSoundingLayer layer : sndLysLst) {
            // if( (layer.getPressure() < 100 )&&(layer.getPressure() >=0 ))
            // System.out.println("pressure below 100 "+ layer.getPressure() +
            // " height " + layer.getGeoHeight() );
            if (layer.getWindDirection() < 0)
                layer.setWindDirection(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getWindSpeed() < 0)
                layer.setWindSpeed(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getGeoHeight() < -50)
                layer.setGeoHeight(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getPressure() < 0)
                layer.setPressure(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getTemperature() < -300)
                layer.setTemperature(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getDewpoint() < -300)
                layer.setDewpoint(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
            if (layer.getOmega() < -100)
                layer.setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);

            i++;
        }

        // sort layers based on pressure, height, wind and list them from
        // highest pressure
        Collections.sort(sndLysLst, reversePressureComparator());

        /*
         * remove duplicate data with same pressure, or same hieght or pressure
         * less than 100
         */
        // create duplicate list
        removingItemList.clear();
        for (i = 0; i < sndLysLst.size() - 1; i++) {
            if (((sndLysLst.get(i).getPressure() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) && (sndLysLst
                    .get(i).getPressure() == sndLysLst.get(i + 1).getPressure()))
                    || ((sndLysLst.get(i).getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) && (sndLysLst
                            .get(i).getGeoHeight() == sndLysLst.get(i + 1)
                            .getGeoHeight()))) {
                removingItemList.add(i + 1);
                continue;
            }
        }

        Collections.reverse(removingItemList);

        for (i = 0; i < removingItemList.size(); i++) {

            sndLysLst.remove(removingItemList.get(i).intValue());
        }

        // System.out.println("  final list");
        // for (NcSoundingLayer soundingLy : sndLysLst){
        // System.out.println(" pressure "+ soundingLy.getPressure() +
        // " height "+ soundingLy.getGeoHeight() +
        // " temp " +soundingLy.getTemperature() + " windS " +
        // soundingLy.getWindSpeed());
        // }
        // System.out.println("final size " + sndLysLst.size() + " stnElv " +
        // stnElv);
        return sndLysLst;

    }

    public static List<NcSoundingLayer> organizeSoundingDataForShow(
            List<NcSoundingLayer> soundingLys, float gndElv) {
        // System.out.println("grond level="+gndElv);
        if (soundingLys.size() <= 0)
            return soundingLys;
        // get rid of layers with invalid data
        soundingLys = removeInvalidSoundingData(soundingLys);

        List<Integer> removingItemList = new ArrayList<Integer>();

        // remove under ground layer(s). Note: There may be more than one
        // under ground layer.
        boolean found75 = false, found50 = false;
        for (int i = 0; i < soundingLys.size(); i++) {
            NcSoundingLayer layer = soundingLys.get(i);
            if (layer.getGeoHeight() == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA
                    || layer.getGeoHeight() < gndElv) {
                removingItemList.add(i);
            } else if (layer.getPressure() < 100 && layer.getPressure() != 50f
                    && layer.getPressure() != 75f) {
                removingItemList.add(i);
            }
            if (layer.getPressure() == 75) {
                found75 = true;// for use later
            } else if (layer.getPressure() == 50) {
                found50 = true;
            }
        }
        // remove invalid data from tail
        Collections.reverse(removingItemList);
        for (int i = 0; i < removingItemList.size(); i++) {
            soundingLys.remove(removingItemList.get(i).intValue());
        }

        /*
         * Chin's NOTE: native nsharp extend sounding data to add 50 and 75 mb
         * layers. It uses 150mb layer's temp, dew, wind dir and wind speed. and
         * interpolate height only. see xtnd_sndg() at readdata.c for original c
         * code.
         */
        if (soundingLys.size() > 2) {
            if (found75 == false) {
                double nm1, nm2, nm4;
                NcSoundingLayer soundingLy;
                int above1, above2;

                soundingLy = new NcSoundingLayer();
                soundingLy.setPressure(75);
                soundingLy.setTemperature(i_temp(150, soundingLys));
                soundingLy.setDewpoint(i_dwpt(150, soundingLys));
                soundingLy.setWindDirection(i_wdir(150, soundingLys));
                soundingLy.setWindSpeed(i_wspd(150, soundingLys));
                soundingLy
                        .setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
                // interpolate height for 75mb layer
                above1 = soundingLys.size() - 1;
                above2 = soundingLys.size() - 2;
                nm1 = soundingLys.get(above1).getGeoHeight()
                        - soundingLys.get(above2).getGeoHeight();
                nm2 = Math.log(soundingLys.get(above2).getPressure()
                        / soundingLys.get(above1).getPressure());
                nm4 = Math.log(soundingLys.get(above2).getPressure() / 75);
                soundingLy.setGeoHeight((float) (soundingLys.get(above2)
                        .getGeoHeight() + ((nm4 / nm2) * nm1)));
                if (found50 == true) {
                    soundingLys.add(soundingLys.size() - 1, soundingLy);
                } else
                    soundingLys.add(soundingLy);
            }
            if (found50 == false) {
                double nm1, nm2, nm4;
                NcSoundingLayer soundingLy;
                int above1, above2;

                soundingLy = new NcSoundingLayer();
                soundingLy.setPressure(50);
                soundingLy.setTemperature(i_temp(150, soundingLys));
                soundingLy.setDewpoint(i_dwpt(150, soundingLys));
                soundingLy.setWindDirection(i_wdir(150, soundingLys));
                soundingLy.setWindSpeed(i_wspd(150, soundingLys));
                soundingLy
                        .setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
                // interpolate height for 50mb layer
                above1 = soundingLys.size() - 1;
                above2 = soundingLys.size() - 2;
                nm1 = soundingLys.get(above1).getGeoHeight()
                        - soundingLys.get(above2).getGeoHeight();
                nm2 = Math.log(soundingLys.get(above2).getPressure()
                        / soundingLys.get(above1).getPressure());
                nm4 = Math.log(soundingLys.get(above2).getPressure() / 50);
                soundingLy.setGeoHeight((float) (soundingLys.get(above2)
                        .getGeoHeight() + ((nm4 / nm2) * nm1)));
                soundingLys.add(soundingLy);
            }
        }

        return soundingLys;
    }
}
