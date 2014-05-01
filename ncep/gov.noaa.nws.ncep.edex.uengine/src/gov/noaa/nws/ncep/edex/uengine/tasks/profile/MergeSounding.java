package gov.noaa.nws.ncep.edex.uengine.tasks.profile;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

//import edu.emory.mathcs.backport.java.util.Collections;

/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.rsc.NsharpMapResource This java class performs
 * the NSHARP Resource functions. This code has been developed by the SIB for
 * use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 08/21/2010	301			T. Lee		Initial coding
 * 09/15/2010	301			C. Chen		Added DB retrieval
 * 09/22/2010	301			T. Lee		Added UAIR merging algorithm
 * 11/05/2010   301         C. Chen     Minor changes to fix index out of bound issue
 * 11/15/2010   301         C. Chen     fix a index out of bound bug
 * 12/2010		301			T. Lee/NCEP	Re-factored for BUFRUA
 * 5/10/2011    301         C. Chen     added rhToDewpoint(), tempToVapr()
 * 02/28/2012               Chin Chen   modify several sounding query algorithms for better performance
 * 8/2012					T. Lee/NCEP	Removed missing wind interpolation
 * 8/2012					T. Lee/NCEP	Fixed max wind merging; May fix NSHARP EL calculation
 * 12/2013					T. Lee/NCEP	Fixed missing height at top level before sorting
 * </pre>
 * 
 * @author T. Lee
 * @version 1.0
 */

public class MergeSounding {
    final float RMISSD = IDecoderConstantsN.UAIR_FLOAT_MISSING;

    final int IMISSD = IDecoderConstantsN.INTEGER_MISSING;

    /**
     * Default constructor
     */
    public MergeSounding() {
    }

    /*
     * Process native sounding data. Convert specific humidity to dew point
     * temperature then compute the moist height.
     */
    public List<NcSoundingLayer> nativeModelSounding(List<NcSoundingLayer> sls,
            float elevation) {
        spfhToDewpoint(sls);
        constructHeight(sls, elevation);
        return sls;
    }

    /*
     * Process upper air sounding data. Note that TTAA is the original/sorted
     * data, while MAN is the TTAA without underground data and MAN_D is the
     * TTAA for display, i.e., the first level is the surface level, any under
     * -ground levels will be above the surface level.
     */
    public List<NcSoundingLayer> mergeUairSounding(String level,
            List<NcSoundingLayer> ttaa, List<NcSoundingLayer> ttbb,
            List<NcSoundingLayer> ttcc, List<NcSoundingLayer> ttdd,
            List<NcSoundingLayer> ppaa, List<NcSoundingLayer> ppbb,
            List<NcSoundingLayer> ppcc, List<NcSoundingLayer> ppdd,
            List<NcSoundingLayer> trop_a, List<NcSoundingLayer> trop_c,
            List<NcSoundingLayer> wmax_a, List<NcSoundingLayer> wmax_c,
            float elevation) {
        List<NcSoundingLayer> sndata = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> man = null;
        System.out.println("Enter mergeUairSounding.....");

        // Return the specific levels requested by users
        if (ttaa.size() > 0) {
            Collections.sort(ttaa, new reverseSortByPressure());
            // System.out.println(" TTAA sounding: ");
            // printOut(ttaa);

            if (level.toUpperCase().equalsIgnoreCase("MAN")) {
                return ttaa;
            }
            man = removeUnderGround(ttaa);
            // System.out.println(" TTAA sounding above ground: ");
            // printOut(man);

        } else {
            if (ppaa.size() < 1 && ttbb.size() < 1) {
                System.out
                        .println(" Missing TTAA/TTBB and PPAA data.");
                return missingSounding();
            } else {
                man = missingSounding();
                System.out.println(" Missing Mandatory (TTAA) Data!");
            }
        }

        // Sorting the data

        if (ttbb.size() > 0) {
            Collections.sort(ttbb, new reverseSortByPressure());
            // System.out.println(" TTBB sounding: ");
            // printOut(ttbb);
        }

        if (ttcc.size() > 0) {
            Collections.sort(ttcc, new reverseSortByPressure());
            // System.out.println(" TTCC sounding: ");
            // printOut(ttcc);

        }

        if (ttdd.size() > 0) {
            Collections.sort(ttdd, new reverseSortByPressure());
            // System.out.println(" TTDD sounding: ");
            // printOut(ttdd);

        }

        if (ppaa.size() > 0) {
            if (checkWindData(ppaa)) {
                Collections.sort(ppaa, new MergeSounding.sortByHeight());
                // System.out.println(" PPAA sounding by Height: ");
                // printOut(ppaa);

            } else {
                Collections.sort(ppaa,
                        new MergeSounding.reverseSortByPressure());
                // System.out.println(" PPAA sounding by Pressure: ");
                // printOut(ppaa);

            }
        }

        if (ppcc.size() > 0) {
            if (checkWindData(ppcc)) {
                Collections.sort(ppcc, new MergeSounding.sortByHeight());
                // System.out.println(" PPCC sounding by Height:");
                // printOut(ppcc);

            } else {
                Collections.sort(ppcc,
                        new MergeSounding.reverseSortByPressure());
                // System.out.println(" PPCC sounding by Pressure: ");
                // printOut(ppcc);
            }
        }

        if (ppbb.size() > 0) {
            if (checkWindData(ppbb)) {
                Collections.sort(ppbb, new MergeSounding.sortByHeight());
                // System.out.println(" PPBB sounding by Height:");
                // printOut(ppbb);

            } else {
                Collections.sort(ppbb,
                        new MergeSounding.reverseSortByPressure());
                // System.out.println(" PPBB sounding by Pressure: ");
                // printOut(ppbb);
            }
        }

        if (ppdd.size() > 0) {
            if (checkWindData(ppdd)) {
                Collections.sort(ppdd, new MergeSounding.sortByHeight());
                // System.out.println(" PPDD sounding by Height");
                // printOut(ppdd);
            } else {
                Collections.sort(ppdd,
                        new MergeSounding.reverseSortByPressure());
                // System.out.println(" PPDD sounding by Pressure: ");
                // printOut(ppdd);
            }

        }

        // Find surface data, return if users request surface data only.
        NcSoundingLayer sl = new NcSoundingLayer();
        sl = getSurfaceData(man, ttbb, ppbb, elevation);
        sndata.add(0, sl);
        // System.out.println(" surface data ");
        // printOut(sndata);

        if (isNumber(level) >= 0) {
            if (equal(0.f, Float.valueOf(level.trim()).floatValue())
                    || equal(sl.getPressure(), Float.valueOf(level.trim())
                            .floatValue())) {
                return sndata;
            }
        }

        // Merge mandatory data
        mergeMandatory(man, ttcc, sndata);
        // System.out.println("after merge Mandatory data: ");
        // printOut(sndata);

        // Check if the single level is mandatory or not
        if (isNumber(level) >= 0) {
            for (int kk = 0; kk < sndata.size(); kk++) {
                if (equal(Float.valueOf(level.trim()).floatValue(),
                        sndata.get(kk).getPressure())) {
                    sl.setPressure(sndata.get(kk).getPressure());
                    sl.setTemperature(sndata.get(kk).getTemperature());
                    sl.setDewpoint(sndata.get(kk).getDewpoint());
                    sl.setWindDirection(sndata.get(kk).getWindDirection());
                    sl.setWindSpeed(sndata.get(kk).getWindSpeed());
                    sl.setGeoHeight(sndata.get(kk).getGeoHeight());
                    sl.setOmega(sndata.get(kk).getOmega());
                    sndata.clear();
                    sndata.add(sl);
                    return sndata;
                }
            }
        }

        // Merge mandatory winds
        mergeMandatoryWinds(ppaa, ppcc, sndata);
        // System.out.println("after merge PPAA/PPCC: ");
        // printOut(sndata);

        // Merge tropopause
        mergeTropSigTemp(trop_a, trop_c, sndata);
        // System.out.println("after merge Tropause data: ");
        // printOut(sndata);

        // Merge TTBB
        mergeTropSigTemp(ttbb, ttdd, sndata);
        // System.out.println("after merge TTBB/TTDD: ");
        // printOut(sndata);

        constructTtbbHeight(sndata);
        // System.out.println("after construct TTBB Height: ");
        // printOut(sndata);

        if (!checkWindData(ppbb)) {
            mergeSigMaxWindOnPressure(ppbb, ppdd, sndata);
            // System.out.println("after merge TTBB/TTDD: ");
            // printOut(sndata);

        }

        mergeSigMaxWindOnPressure(wmax_a, wmax_c, sndata);
        // System.out.println("after merge max wind: ");
        // printOut(sndata);

        constructPpbbHeight(sndata);
        // System.out.println(" After contrstruct PPBB height: ");
        // printOut(sndata);

        if (checkWindData(ppbb)) {
            mergeSigWindOnHeight(ppbb, ppdd, sndata);
            constructPpbbPressure(sndata);
            // System.out.println("after merge PPBB/PPDD: ");
            // printOut(sndata);
        }

        // Reorder sounding profile so the first level is the surface data.
        // No need to reorder now becuz surface data is always the 1st level.
        // reOrderSounding(sndata);

        // Interpolate missing temperature, dew point and winds.
        constructMissing(1, sndata);
        constructMissing(2, sndata);

        // constructMissing(3,sndata);
        // System.out.println("after contrstruct Missing data: ");
        // printOut(sndata);

        // Return single level or add underground mandatory data to the sounding
        // profile

        List<NcSoundingLayer> sndout = new ArrayList<NcSoundingLayer>();
        sndout = removeMissingPressure(sndata);
        // System.out.println(" Missing pressure removed ");
        // printOut(sndout);
        if (isNumber(level) == 0) {
            float rlev = new Integer(Integer.parseInt(level.trim()))
                    .floatValue();
            return getSingLevel(rlev, sndout);
        } else if (isNumber(level) == 1) {
            float rlev = new Float(Float.parseFloat(level.trim()));
            return getSingLevel(rlev, sndout);
        } else {
            if (sndout.size() < 2)
                System.out.println(" Not enough levels to plot the sounding! ");
            return addUnderGround(ttaa, sndout);
        }
    }

    /*
     * Check an alpha-numerical string is a number or characters.
     */
    public int isNumber(String level) {
        try {
            if (Integer.parseInt(level) >= 0) {
                return 0;
            } else {
                return -1;
            }
        } catch (NumberFormatException nfe1) {
            try {
                if (Float.parseFloat(level) >= 0.f) {
                    return 1;
                } else {
                    return -1;
                }
            } catch (NumberFormatException nfe2) {
                try {
                    if (Double.parseDouble(level) >= 0.) {
                        return 2;
                    } else {
                        return -1;
                    }
                } catch (NumberFormatException nfe3) {
                    return -1;
                }
            }
        }
    }

    /*
     * convert specific humidity to dew point temperature.
     */
    float elevation;

    public List<NcSoundingLayer> spfhToDewpoint(List<NcSoundingLayer> sndata) {
        float spfh, pres;
        float dwpc = RMISSD;

        for (NcSoundingLayer layer : sndata) {
            if (layer.getDewpoint() == RMISSD) {
                spfh = layer.getSpecHumidity();
                pres = layer.getPressure();

                if (spfh == RMISSD || pres == RMISSD || spfh <= 0.f
                        || pres <= 0.f) {
                    continue;
                } else {
                    float rmix = spfh / (1.f - spfh);
                    float e = (pres * rmix) / (.62197f + rmix);
                    e = e / (1.001f + ((pres - 100.f) / 900.f) * .0034f);
                    dwpc = (float) (Math.log(e / 6.112) * 243.5 / (17.67 - Math
                            .log((e / 6.112))));
                    layer.setDewpoint(dwpc);
                    // //System.out.println("spfhToDewpoint dwpc: " + dwpc);
                }

            }
        }
        return sndata;
    }

    /*
     * computes DWPC from TMPC and RELH Note: If DWPC is less than -190 degrees
     * C, it is treated as missing data Code is based on GEMPAK's prrhdp.f
     */
    public List<NcSoundingLayer> rhToDewpoint(List<NcSoundingLayer> sndata) {
        float rh, vapr, vaps, temp;
        float dwpc = RMISSD;

        for (NcSoundingLayer layer : sndata) {
            if (layer.getDewpoint() == RMISSD) {
                rh = layer.getRelativeHumidity();
                temp = layer.getTemperature();

                if (rh == RMISSD || temp == RMISSD) {
                    continue;
                } else {
                    vaps = tempToVapr(temp);
                    vapr = rh * vaps / 100;
                    if (vapr < Math.exp(-30))
                        continue;
                    else {
                        dwpc = (float) (243.5 * (Math.log(6.112) - Math
                                .log(vapr)) / (Math.log(vapr) - Math.log(6.112) - 17.67));
                        layer.setDewpoint(dwpc);
                        // //System.out.println("rhToDewpoint dwpc: " + dwpc);
                    }
                }
            }
        }
        return sndata;
    }

    /*
     * computes VAPR from TMPC Code is based on GEMPAK's prvapr.f
     */
    private float tempToVapr(float temp) {
        return (float) (6.112 * Math.exp((17.67 * temp) / (temp + 243.5)));
    }

    private void constructHeight(List<NcSoundingLayer> sndata, float elev) {

        /*
         * For native model sounding, using hypsometric equation to build height
         */
        elevation = elev;
        int lev = sndata.size();
        float tb = RMISSD, tdb = RMISSD, pb = RMISSD;
        float tt = RMISSD, tdt = RMISSD, pt = RMISSD;
        float dwptsf, psfc, tmpcsf, scaleh, mhgt = RMISSD;

        for (int k = 0; k < lev; k++) {

            if (k == 0) {
                tmpcsf = sndata.get(k).getTemperature();
                dwptsf = sndata.get(k).getDewpoint();
                psfc = sndata.get(k).getPressure();
                tb = tmpcsf;
                tt = tmpcsf;
                tdb = dwptsf;
                tdt = dwptsf;
                pb = psfc;
                pt = psfc;

                scaleh = scaleHeight(tb, tt, tdb, tdt, pb, pt);
                mhgt = moistHeight(elevation, pb, pt, scaleh);
            } else {
                tt = sndata.get(k).getTemperature();
                tdt = sndata.get(k).getDewpoint();
                pt = sndata.get(k).getPressure();
                scaleh = scaleHeight(tb, tt, tdb, tdt, pb, pt);

                mhgt = moistHeight(mhgt, pb, pt, scaleh);
                tb = tt;
                tdb = tdt;
                pb = pt;

            }
            sndata.get(k).setGeoHeight(mhgt);
        }
    }

    /*
     * Compute moist height.
     */
    private float moistHeight(float zb, float pb, float pt, float scale) {
        // System.out.println("From moistHeight: ");
        if (zb == RMISSD || pb == RMISSD || pt == RMISSD || scale == RMISSD) {
            return RMISSD;
        } else {
            // System.out.println("the computed moistHeight is  " + (float) (zb
            // + scale * Math.log(pb / pt)));
            return (float) (zb + scale * Math.log(pb / pt));
        }
    }

    /*
     * Compute scale height.
     */
    private float scaleHeight(float tb, float tt, float tdb, float tdt,
            float pb, float pt) {
        // System.out.println("From scaleHeight: " );
        final float RDGAS = 287.04f, GRAVTY = 9.80616f, RKAP = RDGAS / GRAVTY;
        if (tb == RMISSD || tt == RMISSD || pb == RMISSD || pt == RMISSD) {
            return RMISSD;
        } else {
            float tvb = virtualTemperature(tb, tdb, pb);
            float tvt = virtualTemperature(tt, tdt, pt);
            // System.out.println("tvb = " + tvb);
            // System.out.println("tvt = " + tvt);
            if (tvb == RMISSD || tvt == RMISSD) {
                return RMISSD;
            } else {
                float tav = (tvb + tvt) / 2.0f;

                // System.out.println("tav = " + tav);
                // System.out.println("RKAP * tav = " + RKAP * tav);
                return (RKAP * tav);
            }
        }
    }

    /*
     * Compute virtual temperature
     */
    private float virtualTemperature(float tt, float td, float pres) {
        if (tt == RMISSD || pres == RMISSD) {
            return RMISSD;
        } else if (td == RMISSD) {
            return celciusToKevin(tt);
        } else {
            float tmpk = celciusToKevin(tt);
            float rmix = mixingRatio(td, pres);
            if (rmix == RMISSD) {
                return celciusToKevin(tt);
            } else {
                return tmpk * (1.f + .001f * rmix / .62197f)
                        / (1.f + .001f * rmix);
            }
        }
    }

    /*
     * Convert Celcius to Kelvin.
     */
    float TMCK = 273.15f;

    private float celciusToKevin(float tc) {
        if (tc == RMISSD) {
            return RMISSD;
        } else {
            return (tc + TMCK);
        }
    }

    /*
     * Compute mixing ratio from DWPC and PRES.
     */
    private float mixingRatio(float td, float pres) {
        if (td == RMISSD || pres == RMISSD) {
            return RMISSD;
        } else {
            float vapr = vaporPressure(td);
            if (vapr == RMISSD) {
                return RMISSD;
            }

            float corr = (1.001f + ((pres - 100.f) / 900.f) * .0034f);

            float e = corr * vapr;
            if (e > (.5f * pres)) {
                return RMISSD;
            } else {
                return .62197f * (e / (pres - e)) * 1000.f;
            }
        }
    }

    /*
     * Compute vapor pressure from DWPC.
     */
    private float vaporPressure(float td) {
        if (td == RMISSD) {
            return RMISSD;
        } else {
            return (6.112f * (float) Math.exp((17.67 * td) / (td + 243.5)));
        }
    }

    /*
     * Merge observed sounding data
     */

    /*
     * Check wind data if the data is reported on pressure or height surfaces.
     * Return TRUE if reported on height. (MR_CHKW)
     * 
     * Note that this is coded different from MR_CHKW, in that it will set zwind
     * to false only if pressure is less than 0. An odd logic.
     */
    public boolean checkWindData(List<NcSoundingLayer> sndata) {
        boolean zwind = true;
        for (int kk = 0; kk < sndata.size(); kk++) {
            if (sndata.get(kk).getPressure() != RMISSD) {
                zwind = false;
            }
        }
        return zwind;
    }

    /*
     * Find surface data. (MR_SRFC)
     */
    final int NPARMS = 7;

    public NcSoundingLayer getSurfaceData(List<NcSoundingLayer> man,
            List<NcSoundingLayer> ttbb, List<NcSoundingLayer> ppbb,
            float elevation) {
        float psfc = RMISSD;
        NcSoundingLayer sl_sfc = new NcSoundingLayer();

        /*
         * Check for surface information in mandatory data.
         */
        if (man == null || man.size() < 1) {
            sl_sfc = missingSounding().get(0);
        } else {
            // If surface pressure is greater than 1080mb, set to missing.
            // Otherwise
            // surface pressure will be the first report level on TTAA.
            // Note that GEMPAK sets it to 1060mb.
            psfc = man.get(0).getPressure();
            if (psfc > 1080.) {
                sl_sfc.setPressure(RMISSD);
            } else {
                sl_sfc.setPressure(psfc);
            }
            sl_sfc.setTemperature(man.get(0).getTemperature());
            sl_sfc.setDewpoint(man.get(0).getDewpoint());
            sl_sfc.setWindDirection(man.get(0).getWindDirection());
            sl_sfc.setWindSpeed(man.get(0).getWindSpeed());
            sl_sfc.setGeoHeight(elevation);
            sl_sfc.setOmega(man.get(0).getOmega());
        }

        /*
         * Find the first reporting mandatory level above the surface.
         */
        float pman = RMISSD;
        int iman = 1;
        try {
            while (pman == RMISSD && iman < man.size()) {
                if (man.get(iman).getPressure() != RMISSD
                        && man.get(iman).getTemperature() != RMISSD
                        && man.get(iman).getGeoHeight() != RMISSD) {
                    pman = man.get(iman).getPressure();
                }
                iman++;
            }
        } catch (Exception e) {
            // do nothing
        }

        /*
         * If surface pressure is missing or is less than first reporting
         * mandatory level, set surface data to missing.
         */
        if (psfc == RMISSD || (psfc < pman && pman != RMISSD)) {
            sl_sfc = missingSounding().get(0);
        }

        /*
         * Use TTBB/PPBB to get surface data if TTAA is missing. The check for
         * significant level pressure to be less than or equal to psfc
         * eliminates erroneous data.
         */
        if (ttbb.size() > 0) {
            psfc = sl_sfc.getPressure();
            float psql = ttbb.get(0).getPressure();

            if (equal(psfc, psql) || psfc == RMISSD) {
                if (psql != RMISSD && psfc == RMISSD)
                    sl_sfc.setPressure(psql);

                if (sl_sfc.getTemperature() == RMISSD)
                    sl_sfc.setTemperature(ttbb.get(0).getTemperature());

                if (sl_sfc.getDewpoint() == RMISSD)
                    sl_sfc.setDewpoint(ttbb.get(0).getDewpoint());
            }
        }

        /*
         * If the first significant level wind data is surface information, use
         * it to replace the surface data if the pressure is at surface.
         */

        // PPBB reported in Height.
        if (checkWindData(ppbb)) {
            if (ppbb.size() > 0) {
                if (ppbb.get(0).getGeoHeight() == 0.
                        && sl_sfc.getWindDirection() == RMISSD) {
                    sl_sfc.setWindDirection(ppbb.get(0).getWindDirection());
                    sl_sfc.setWindSpeed(ppbb.get(0).getWindSpeed());
                }
            }
        } else {
            // PPBB reported in Pressure.
            if (ppbb.size() > 0) {
                if (ppbb.get(0).getPressure() != RMISSD
                        && sl_sfc.getPressure() == RMISSD) {
                    float psgl = Math.abs(ppbb.get(0).getPressure());
                    sl_sfc.setPressure(psgl);

                    if (ppbb.get(0).getWindDirection() != RMISSD
                            && sl_sfc.getWindDirection() == RMISSD) {
                        if (equal(psfc, psgl)) {
                            sl_sfc.setWindDirection(ppbb.get(0)
                                    .getWindDirection());
                            sl_sfc.setWindSpeed(ppbb.get(0).getWindSpeed());
                        }
                    }
                }
            }
        }

        /*
         * Return surface data.
         */
        return sl_sfc;
    }

    private boolean equal(float x, float y) {
        final float RDIFFD = .0001f;
        if ((x + y) == 0.) {
            return Math.abs(x - y) < RDIFFD;
        } else {
            return Math.abs(x - y) / Math.abs((x + y) / 2.) < RDIFFD;
        }
    }

    /*
     * Merge the mandatory below 100 mb data and the mandatory above data.
     * sndata has surface observation ONLY. (MR_MAND)
     */

    public void mergeMandatory(List<NcSoundingLayer> man_a,
            List<NcSoundingLayer> man_c, List<NcSoundingLayer> sndata) {

        float plast;
        // if (man_a == null) {
        // System.out.println(" NO data in Man \n");
        // } else {
        // System.out.println(" Man not null \n");

        //
        if (man_a.size() < 1 && man_c.size() < 1) {
            return;
        }

        if (sndata.get(0).getPressure() == RMISSD) {
            plast = 2000.f;
        } else {
            plast = sndata.get(0).getPressure();
        }

        /*
         * Move the mandatory data below 100mb to the output array, sndata.
         * Check that pressure is not missing and is decreasing.
         */
        float pres;
        if (man_a.size() > 0) {
            for (int kk = 1; kk < man_a.size(); kk++) {
                pres = man_a.get(kk).getPressure();
                if (pres < plast
                        && pres != RMISSD
                        && (man_a.get(kk).getTemperature() != RMISSD || man_a
                                .get(kk).getWindDirection() != RMISSD)) {
                    addDataToList(kk, man_a, sndata);
                    plast = pres;
                }
            }
        }

        /*
         * Move the mandatory data above 100 mb to the output array.
         */
        if (man_c.size() > 0) {
            for (int kk = 0; kk < man_c.size(); kk++) {
                pres = man_c.get(kk).getPressure();
                if (pres < plast && pres != RMISSD
                        && man_c.get(kk).getTemperature() != RMISSD) {
                    addDataToList(kk, man_c, sndata);
                    plast = man_c.get(kk).getPressure();
                }
            }
        }
    }

    /*
     * Merge the mandatory below 100 mb wind data and the mandatory above wind
     * data. (MR_MANW)
     */
    public void mergeMandatoryWinds(List<NcSoundingLayer> man_wa,
            List<NcSoundingLayer> man_wc, List<NcSoundingLayer> sndata) {

        if (man_wa.size() < 1 && man_wc.size() < 1) {
            return;
        }

        /*
         * Append data.
         */
        if (man_wc.size() > 0) {
            for (int kk = 0; kk < man_wc.size(); kk++) {
                man_wa.add(man_wc.get(kk));
            }
        }
        /*
         * Loop through mandatory wind data.
         */
        for (int lev = 0; lev < man_wa.size(); lev++) {

            /*
             * If this is the correct level, add wind data.
             */
            boolean found = false;
            float ppp = man_wa.get(lev).getPressure();
            for (int kk = 0; kk < sndata.size() && !found; kk++) {
                float pres = sndata.get(kk).getPressure();
                if (equal(ppp, pres)) {
                    if (sndata.get(kk).getWindDirection() == RMISSD) {
                        sndata.get(kk).setWindDirection(
                                man_wa.get(lev).getWindDirection());
                        sndata.get(kk).setWindSpeed(
                                man_wa.get(lev).getWindSpeed());
                    }
                    found = true;
                }
            }

            /*
             * If not found, add to the list
             */
            if (!found) {
                float ddd = man_wa.get(lev).getWindDirection();
                if (ppp != RMISSD && ddd != RMISSD) {
                    addDataToList(lev, man_wa, sndata);
                }
            }
        }
    }

    /*
     * Merge tropopause, max wind and significant temperature data (TTBB) to the
     * station data array. The input parameter could be tropopause data or
     * significant temperature data. MR_TROP & MR_SIGT
     */
    public List<NcSoundingLayer> mergeTropSigTemp(List<NcSoundingLayer> trop_a,
            List<NcSoundingLayer> trop_c, List<NcSoundingLayer> sndata) {
        if (trop_a.size() < 1 && trop_c.size() < 1) {
            return sndata;
        }

        /*
         * Append two lists of wind data.
         */
        if (trop_c.size() > 0) {
            for (int kk = 0; kk < trop_c.size(); kk++) {
                trop_a.add(trop_c.get(kk));
            }
        }

        for (int lev = 0; lev < trop_a.size(); lev++) {
            boolean found = false;
            float ppp = trop_a.get(lev).getPressure();
            for (int kk = 0; kk < sndata.size() && !found; kk++) {
                float pres = sndata.get(kk).getPressure();
                if (equal(ppp, pres)) {

                    // add data to missing
                    if (sndata.get(kk).getTemperature() == RMISSD) {
                        sndata.get(kk).setTemperature(
                                trop_a.get(lev).getTemperature());
                        sndata.get(kk).setDewpoint(
                                trop_a.get(lev).getDewpoint());
                    }

                    if (sndata.get(kk).getWindDirection() == RMISSD) {
                        sndata.get(kk).setWindDirection(
                                trop_a.get(lev).getWindDirection());
                        sndata.get(kk).setWindSpeed(
                                trop_a.get(lev).getWindSpeed());

                    }
                    found = true;
                }
            }

            /*
             * if not found, add to the list
             */
            if (!found) {
                float ttt = trop_a.get(lev).getTemperature();
                if (ppp != RMISSD && ttt != RMISSD) {
                    addDataToList(lev, trop_a, sndata);
                }
            }

        }

        /*
         * Sort the sounding data in descending order.
         */
        Collections.sort(sndata, new reverseSortByPressure());
        return sndata;
    }

    /*
     * Compute height at significant temperature levels (TTBB) using a moist
     * hydrostatic computation. (MR_SCMZ)
     */
    public void constructTtbbHeight(List<NcSoundingLayer> sndata) {
        boolean mand = false;
        boolean newblock = true;
        int blev = 0, tlev = 0;
        float[] scale = new float[200];
        float pb, zb, tb, tdb, zlev, plev;
        float pt, zt = 0.f, tt, tdt, znew = 0.f;
        // System.out.println("In construct TTBB Height 1: ");
        // printOut(sndata);

        if (sndata.size() <= 2)
            return;

        for (int nlev = 0; nlev < sndata.size(); nlev++) {
            if (newblock) {
                if (sndata.get(nlev).getGeoHeight() != RMISSD
                        && sndata.get(nlev).getPressure() != RMISSD
                        && sndata.get(nlev).getTemperature() != RMISSD) {

                    blev = nlev;
                    newblock = false;
                }
            } else {
                if (sndata.get(nlev).getGeoHeight() != RMISSD
                        && sndata.get(nlev).getTemperature() != RMISSD) {
                    tlev = nlev;
                    mand = true;
                }
            }

            /*
             * Compute scale height to this level
             */
            if (mand) {
                pb = sndata.get(blev).getPressure();
                zb = sndata.get(blev).getGeoHeight();
                tb = sndata.get(blev).getTemperature();
                tdb = sndata.get(blev).getDewpoint();
                zlev = sndata.get(blev).getGeoHeight();
                plev = sndata.get(blev).getPressure();

                for (int kk = blev + 1; kk <= tlev; kk++) {
                    pt = sndata.get(kk).getPressure();
                    zt = sndata.get(kk).getGeoHeight();
                    tt = sndata.get(kk).getTemperature();
                    tdt = sndata.get(kk).getDewpoint();
                    scale[kk] = scaleHeight(tb, tt, tdb, tdt, pb, pt);
                    znew = moistHeight(zb, pb, pt, scale[kk]);
                    if (znew != RMISSD) {
                        pb = pt;
                        tb = tt;
                        tdb = tdt;
                        zb = znew;
                    }
                }

                /*
                 * Compute the scaling factor so the computed moist height is
                 * consistent at the mandatory level. Then recompute the height.
                 */
                float s = (zt - zlev) / (znew - zlev);
                float zbb = zlev;
                float pbb = plev;
                for (int kk = blev + 1; kk < tlev; kk++) {
                    pt = sndata.get(kk).getPressure();
                    zt = sndata.get(kk).getGeoHeight();
                    scale[kk] = scale[kk] * s;
                    znew = moistHeight(zbb, pbb, pt, scale[kk]);
                    if (znew != RMISSD) {
                        pbb = pt;
                        zbb = znew;
                        sndata.get(kk).setGeoHeight(znew);
                    }
                }
                mand = false;
                newblock = true;

                if ((tlev + 1) != sndata.size()) {
                    if (sndata.get(tlev + 1).getGeoHeight() == RMISSD
                            && sndata.get(tlev + 1).getPressure() != RMISSD
                            && sndata.get(tlev + 1).getTemperature() != RMISSD) {
                        nlev--;
                    }
                }
            }
        }
        // System.out
        // .println("In construct TTBB Height before check TOP missing level: ");
        // printOut(sndata);

        // System.out.println(" TLEV: " + tlev + " sndata.size()" +
        // sndata.size() + "\n");
        // Fill missing height at the top levels
        fillMissingHeightAtTop(sndata);

        return;
    }

    /*
     * Merge the significant and maximum wind data on pressure surfaces. MR_PWND
     */
    public void mergeSigMaxWindOnPressure(List<NcSoundingLayer> sig_wa,
            List<NcSoundingLayer> sig_wc, List<NcSoundingLayer> sndata) {

        if (sig_wa.size() < 1 && sig_wc.size() < 1) {
            return;
        }

        // System.out.println(" Windmax below trop \n");
        // printOut(sig_wa);
        // System.out.println(" Windmax above trop \n");
        // printOut(sig_wc);

        /*
         * Append two lists of wind data.
         */
        if (sig_wc.size() > 0 && sig_wc.get(0).getPressure() != RMISSD) {
            for (int kk = 0; kk < sig_wc.size(); kk++) {
                sig_wa.add(sig_wc.get(kk));
            }
        }

        /*
         * Merging
         */
        int nlevel = sndata.size();
        for (int kk = 0; kk < sig_wa.size(); kk++) {
            boolean found = false;
            for (int lev = 0; lev < nlevel; lev++) {
                if (equal(sndata.get(lev).getPressure(), sig_wa.get(kk)
                        .getPressure())) {

                    // add data to missing
                    if (sndata.get(lev).getWindDirection() == RMISSD) {
                        sndata.get(lev).setWindDirection(
                                sig_wa.get(kk).getWindDirection());
                        sndata.get(lev).setWindSpeed(
                                sig_wa.get(kk).getWindSpeed());
                    }
                    found = true;
                }
            }
            // System.out.println("In mergeSigMaxWindOnPressure:);
            // printOut(sndata);

            /*
             * if not found, add to the list.
             */
            if (!found) {
                if ((sig_wa.get(kk).getWindDirection() != RMISSD && sig_wa.get(
                        kk).getPressure() != RMISSD)) {

                    NcSoundingLayer sl = new NcSoundingLayer();
                    sl.setPressure(sig_wa.get(kk).getPressure());
                    sl.setTemperature(sig_wa.get(kk).getTemperature());
                    sl.setDewpoint(sig_wa.get(kk).getDewpoint());
                    sl.setWindDirection(sig_wa.get(kk).getWindDirection());
                    sl.setWindSpeed(sig_wa.get(kk).getWindSpeed());
                    sl.setGeoHeight(sig_wa.get(kk).getGeoHeight());
                    sl.setOmega(sig_wa.get(kk).getOmega());
                    sndata.add(sl);
                    nlevel++;
                }
            }

            Collections.sort(sndata, new reverseSortByPressure());
        }
        fillMissingHeightAtTop(sndata);
        // printOut(sndata);
        return;
    }

    /*
     * Construct height at significant wind levels (PPBB) if reported on
     * pressure levels. Using moist hydrostatic computation for missing height
     * at top levels. MR_INTZ
     */
    public void constructPpbbHeight(List<NcSoundingLayer> sndata) {
        int tlev = 0;
        // System.out.println("From constructPpbbHeight(): " );
        for (int kk = sndata.size() - 1; kk >= 0; kk--) {
            if (sndata.get(kk).getGeoHeight() != RMISSD) {
                tlev = kk;
                break;
            }
        }

        float pb = RMISSD, pt = RMISSD, zt = RMISSD, zb = RMISSD;
        int next;

        if (sndata.size() <= 2)
            return;

        for (int kk = 0; kk < tlev; kk++) {
            float pres = sndata.get(kk).getPressure();
            float hght = sndata.get(kk).getGeoHeight();
            if (pres == RMISSD) {
                // DO NOTHING
            } else if (hght != RMISSD) {
                pb = pres;
                zb = hght;
                pt = 2000.f;
            } else if (pb == RMISSD) {
                // DO NOTHING
            } else {

                /*
                 * Find next level with height and then interpolate the data.
                 */
                next = kk + 1;
                while (pres <= pt) {
                    if (sndata.get(next).getGeoHeight() != RMISSD) {
                        pt = sndata.get(next).getPressure();
                        zt = sndata.get(next).getGeoHeight();
                    } else {
                        next++;
                    }
                }
                float hhh = (float) (zb + (zt - zb)
                        * (Math.log(pres / pb) / Math.log(pt / pb)));
                sndata.get(kk).setGeoHeight(hhh);
            }
        }

        fillMissingHeightAtTop(sndata);
    }

    /*
     * Merge significant wind on height surfaces. The argument is sndata.
     */
    public void mergeSigWindOnHeight(List<NcSoundingLayer> sig_wa,
            List<NcSoundingLayer> sig_wc, List<NcSoundingLayer> sndata) {

        /*
         * The following code needs to be replaced by significant wind data from
         * database.
         */

        /*
         * Do nothing if wind report is not on height surfaces.
         */
        if (sig_wa.size() < 1 && sig_wc.size() < 1) {
            return;
        }

        /*
         * Add two lists of wind data.
         */
        if (sig_wc.size() > 0) {
            for (int kk = 0; kk < sig_wc.size(); kk++) {
                sig_wa.add(sig_wc.get(kk));
            }
        }
        // System.out.println(" in mergeSigWindOnHeight 0: ");
        // printOut(sndata);

        int nlevel = sndata.size();
        for (int kk = 0; kk < sig_wa.size(); kk++) {
            boolean found = false;
            float zzz = sig_wa.get(kk).getGeoHeight();

            // Check surface level independently because sometimes station
            // report wind data twice at surface. We don't want the surface
            // pressure to be missing.
            if (zzz == 0) {
                if (sndata.get(0).getWindDirection() == RMISSD) {
                    sndata.get(0).setWindDirection(
                            sig_wa.get(0).getWindDirection());
                    sndata.get(0).setWindSpeed(sig_wa.get(kk).getWindSpeed());
                }
                found = true;
            } else {
                for (int lev = 0; lev < nlevel; lev++) {
                    float hght = sndata.get(lev).getGeoHeight();
                    if (equal(zzz, hght) || (zzz == 0 && lev == 0 && kk == 0)) {
                        // add data to missing
                        if (sndata.get(lev).getWindDirection() == RMISSD) {
                            sndata.get(lev).setWindDirection(
                                    sig_wa.get(kk).getWindDirection());
                            sndata.get(lev).setWindSpeed(
                                    sig_wa.get(kk).getWindSpeed());
                        }
                        found = true;
                    }
                }
            }

            // System.out.println(" in mergeSigWindOnHeight 1: ");
            // printOut(sndata);

            /*
             * if not found, add to the list.
             */
            if (!found) {
                if (sig_wa.get(kk).getWindDirection() != RMISSD
                        && sig_wa.get(kk).getGeoHeight() != RMISSD) {
                    NcSoundingLayer sl = new NcSoundingLayer();
                    sl.setPressure(sig_wa.get(kk).getPressure());
                    sl.setTemperature(sig_wa.get(kk).getTemperature());
                    sl.setDewpoint(sig_wa.get(kk).getDewpoint());
                    sl.setWindDirection(sig_wa.get(kk).getWindDirection());
                    sl.setWindSpeed(sig_wa.get(kk).getWindSpeed());
                    sl.setGeoHeight(sig_wa.get(kk).getGeoHeight());
                    sl.setOmega(sig_wa.get(kk).getOmega());
                    sndata.add(sl);
                }
            }
        }

        /*
         * Sorting the combined temperature and wind data based on Geopotential
         * height.
         */
        // System.out.println(" in mergeSigWindOnHeight 2: ");
        // printOut(sndata);

        fillMissingHeightAtTop(sndata);

        Collections.sort(sndata, new sortByHeight());
        return;
    }

    // Sort by height
    public static class sortByHeight implements Comparator<NcSoundingLayer> {
        public int compare(NcSoundingLayer l1, NcSoundingLayer l2) {
            return Float.compare(l1.getGeoHeight(), l2.getGeoHeight());
        }

    }

    // Reverse sort by pressure
    public static class reverseSortByPressure implements
            Comparator<NcSoundingLayer> {
        public int compare(NcSoundingLayer l1, NcSoundingLayer l2) {
            return Float.compare(l2.getPressure(), l1.getPressure());
        }

    }

    /*
     * Construct pressure at significant wind levels (PPBB) that are reported on
     * height levels. MR_INTP
     */
    public List<NcSoundingLayer> constructPpbbPressure(
            List<NcSoundingLayer> sndata) {

        if (sndata.size() <= 2)
            return sndata;

        float pb = RMISSD, pt = RMISSD, zt = RMISSD, zb = RMISSD;
        int blev = IMISSD, tlev = IMISSD;
        for (int lev = 0; lev < sndata.size(); lev++) {
            float pres = sndata.get(lev).getPressure();
            float hght = sndata.get(lev).getGeoHeight();
            if (pres != RMISSD && hght != RMISSD) {
                tlev = lev;
                pt = pres;
                zt = hght;
            }

            if (blev != IMISSD && tlev != IMISSD) {
                for (int kk = blev + 1; kk < tlev; kk++) {
                    float z = sndata.get(kk).getGeoHeight();
                    if (sndata.get(kk).getGeoHeight() != RMISSD) {
                        float ppp = (float) (pb * Math.exp((double) ((z - zb)
                                * Math.log(pt / pb) / (zt - zb))));
                        sndata.get(kk).setPressure(ppp);
                    }
                }
            }
            blev = tlev;
            pb = pt;
            zb = zt;
        }

        if (tlev == (sndata.size() - 1) || tlev == IMISSD) {
            return sndata;
        } else {

            /*
             * Compute missing pressure at top levels.
             */
            pb = sndata.get(tlev - 1).getPressure();
            zb = sndata.get(tlev - 1).getGeoHeight();

            for (int kk = tlev + 1; kk < sndata.size(); kk++) {
                if (sndata.get(kk).getPressure() == RMISSD) {
                    pt = sndata.get(kk - 1).getPressure();
                    zt = sndata.get(kk - 1).getGeoHeight();
                    float zz = sndata.get(kk).getGeoHeight();
                    float rmult = (float) ((zz - zb) / (zt - zb));
                    sndata.get(kk).setPressure(
                            (float) (pb * (Math.pow(pt / pb, rmult))));
                    pb = pt;
                    zb = zt;
                }
            }
        }
        return sndata;
    }

    /*
     * Reorder the sounding data so the first level is always the surface data.
     */
    public List<NcSoundingLayer> reOrderSounding(List<NcSoundingLayer> sndata) {
        List<NcSoundingLayer> outdat = new ArrayList<NcSoundingLayer>();
        float tt, td, dd, ff;
        int klev = 0;
        if (sndata.size() <= 1)
            return sndata;

        /*
         * Find the surface level
         */
        for (int kk = 0; kk < sndata.size(); kk++) {
            tt = sndata.get(kk).getTemperature();
            td = sndata.get(kk).getDewpoint();
            dd = sndata.get(kk).getWindDirection();
            ff = sndata.get(kk).getWindSpeed();
            if (tt == RMISSD && td == RMISSD && dd == RMISSD && ff == RMISSD) {
                // DO NOTHING
            } else {
                klev = kk;
                addDataToList(0, sndata, outdat);
            }
        }

        /*
         * Reorder the data below the surface levels.
         */
        for (int kk = 0; kk < klev; kk++) {
            addDataToList(kk, sndata, outdat);
        }

        for (int kk = klev + 1; kk < sndata.size(); kk++) {
            addDataToList(kk, sndata, outdat);
        }
        return outdat;
    }

    /*
     * Construct missing temperature (iflag = 1), dewpoint temperature (iflag=2)
     * and wind (iflag = 3). This method is called after reOrderSounding().
     * MR_MISS
     */
    public List<NcSoundingLayer> constructMissing(int iflag,
            List<NcSoundingLayer> sndata) {
        float pb = RMISSD, pt = RMISSD, data = RMISSD, pres, tb, tt, tdb, tdt;
        int jlev = IMISSD, tlev = IMISSD;
        boolean contin = true;
        if (sndata.size() <= 2)
            return sndata;
        for (int blev = 1; blev < sndata.size() - 1 && contin; blev++) {
            jlev = blev;

            switch (iflag) {
            case 1: {
                data = sndata.get(blev).getTemperature();
                break;
            }
            case 2: {
                data = sndata.get(blev).getDewpoint();
                break;
            }
            case 3: {
                data = sndata.get(blev).getWindDirection();
                break;
            }
            default: {
                return sndata;
            }
            }

            if (data == RMISSD) {

                /*
                 * find data at level above. Data should already be at level
                 * below after reOrderSounding() call.
                 */
                boolean found = false;
                while (!found) {
                    jlev++;
                    switch (iflag) {
                    case 1: {
                        data = sndata.get(jlev).getTemperature();
                        break;
                    }
                    case 2: {
                        data = sndata.get(jlev).getDewpoint();
                        break;
                    }
                    case 3: {
                        data = sndata.get(jlev).getWindDirection();
                        break;
                    }
                    default: {
                        // System.out.println("Invalid data flag");
                    }
                    }
                    int top = sndata.size();
                    if (data != RMISSD || jlev + 1 >= top) {
                        found = true;
                        tlev = jlev;
                        if (jlev >= top) {
                            tlev = IMISSD;
                            contin = false;
                        }
                    }
                }

                /*
                 * Add check to eliminate dew point layer more than 100mb.
                 */
                if (iflag == 2 && tlev != IMISSD) {
                    if ((sndata.get(blev).getPressure() - sndata.get(tlev)
                            .getPressure()) > 100.) {
                        for (int kk = tlev; kk < sndata.size(); kk++) {
                            sndata.get(kk).setDewpoint(RMISSD);
                        }
                        tlev = IMISSD;
                        contin = false;
                    }
                }

                /*
                 * Add check to eliminate interpolation of winds from below 100
                 * mb to above 100 mb. This eliminates interpolation to very
                 * high level winds.
                 */
                /*
                 * if (iflag == 3 && tlev != IMISSD && (sndata.get(blev -
                 * 1).getPressure() > 100.) && (sndata.get(tlev).getPressure() <
                 * 100.)) { tlev = IMISSD; }
                 */
                /*
                 * Interpolate with respect to logP.
                 */
                float rmult = RMISSD;
                if (tlev != IMISSD) {
                    pb = sndata.get(blev - 1).getPressure();
                    pres = sndata.get(blev).getPressure();
                    pt = sndata.get(tlev).getPressure();
                    if (pt != RMISSD && pb != RMISSD && pres != RMISSD) {
                        rmult = (float) (Math.log(pres / pb) / Math
                                .log(pt / pb));
                    }

                    switch (iflag) {
                    case 1: {
                        tb = sndata.get(blev - 1).getTemperature();
                        tt = sndata.get(tlev).getTemperature();

                        if (tb != RMISSD && tt != RMISSD && rmult != RMISSD) {
                            data = tb + (tt - tb) * rmult;

                            sndata.get(blev).setTemperature(data);
                        }

                        tdb = sndata.get(blev - 1).getDewpoint();
                        tdt = sndata.get(tlev).getDewpoint();
                        if (tdb != RMISSD && tdt != RMISSD && rmult != RMISSD) {
                            data = tdb + (tdt - tdb) * rmult;
                            sndata.get(blev).setDewpoint(data);
                        }
                        break;
                    }
                    case 2: {
                        tdb = sndata.get(blev - 1).getDewpoint();
                        tdt = sndata.get(tlev).getDewpoint();
                        if (tdb != RMISSD && tdt != RMISSD && rmult != RMISSD) {
                            data = tdb + (tdt - tdb) * rmult;
                            sndata.get(blev).setDewpoint(data);
                        }
                        break;
                    }
                    case 3: {
                        float drctb = sndata.get(blev - 1).getWindDirection();
                        float drctt = sndata.get(tlev).getWindDirection();

                        if (drctt != RMISSD && drctb != RMISSD
                                && rmult != RMISSD) {
                            drctb = drctb % 360.f;
                            drctt = drctt % 360.f;
                            if (Math.abs(drctb - drctt) > 180.f) {
                                if (drctb < drctt) {
                                    drctb = drctb + 360.f;
                                } else {
                                    drctt = drctt + 360.f;
                                }
                            }
                            float drct = (drctb + (drctt - drctb) * rmult) % 360.f;
                            sndata.get(blev).setWindDirection(drct);

                            // Interpolate wind speed
                            float spedb = sndata.get(blev - 1).getWindSpeed();
                            float spedt = sndata.get(tlev).getWindSpeed();
                            float sped = spedb + (spedt - spedb) * rmult;
                            sndata.get(blev).setWindSpeed(sped);
                        }
                        break;

                    }
                    }
                }
            }
        }
        return sndata;
    }

    /*
     * Re-order the data so the first level is always the ground level. MR_COND
     */
    public List<NcSoundingLayer> addUnderGround(List<NcSoundingLayer> man,
            List<NcSoundingLayer> sndata) {
        if (sndata == null || sndata.size() < 2)
            return sndata;
        if (sndata.get(0).getPressure() == RMISSD || man.size() < 1)
            return sndata;

        int blev = 0;
        boolean contin = true;
        while (blev < sndata.size() && contin) {
            if (man.get(blev).getPressure() > sndata.get(0).getPressure()) {
                blev++;
                if (blev >= man.size())
                    contin = false;
            } else {
                contin = false;
            }
        }

        if (blev >= sndata.size()) {
            return sndata;
        }

        /*
         * Added below-ground mandatory levels to sounding layers.
         */
        List<NcSoundingLayer> outdat = new ArrayList<NcSoundingLayer>();

        int nlev = sndata.size();

        // write to surface data first
        addDataToList(0, sndata, outdat);

        // add below-ground mandatory data
        if (blev > 0 && blev < sndata.size()) {
            for (int kk = 0; kk < blev; kk++) {
                addDataToList(kk, man, outdat);
            }
        }

        // add the rest of the data
        for (int kk = 1; kk < nlev; kk++) {
            addDataToList(kk, sndata, outdat);
        }
        return outdat;

    }

    /*
     * Re-order the data so the first level is always the ground level. MR_COND
     */
    public List<NcSoundingLayer> removeUnderGround(List<NcSoundingLayer> sndata) {
        List<NcSoundingLayer> outdat = new ArrayList<NcSoundingLayer>();
        /*
         * Remove below-ground mandatory levels from sounding layers. Only the
         * first 8 missing levels can be mandatory levels.
         */
        if (sndata.size() <= 1)
            return sndata;
        for (int kk = 0; kk < sndata.size(); kk++) {
            if (sndata.get(kk).getTemperature() <= RMISSD
                    && sndata.get(kk).getDewpoint() <= RMISSD
                    && sndata.get(kk).getWindDirection() <= RMISSD
                    && sndata.get(kk).getWindSpeed() <= RMISSD) {
            } else if (sndata.get(kk).getPressure() <= RMISSD) {
            } else {
                addDataToList(kk, sndata, outdat);
            }
        }
        return outdat;
    }

    /*
     * Interpolate data to a single level, including surface.
     */
    public List<NcSoundingLayer> getSingLevel(float pres,
            List<NcSoundingLayer> sndata) {
        NcSoundingLayer sl = new NcSoundingLayer();
        List<NcSoundingLayer> sls = new ArrayList<NcSoundingLayer>();
        sndata = removeUnderGround(sndata);
        if (sndata.size() <= 1)
            return missingSounding(); // Chin: check size again, after remove
                                      // underground level, size changed

        for (int kk = 1; /* chin 0; */kk < sndata.size() - 1; kk++) {
            if (pres > sndata.get(0).getPressure() || pres < 0.f) {
                return missingSounding();
            } else {

                if (pres >= sndata.get(kk).getPressure()) {
                    float pt, pb, zt, zb, tt, tb, tdt, tdb, dt, db, st, sb;
                    pb = sndata.get(kk - 1).getPressure();
                    pt = sndata.get(kk).getPressure();
                    tb = sndata.get(kk - 1).getTemperature();
                    tt = sndata.get(kk).getTemperature();
                    tdb = sndata.get(kk - 1).getDewpoint();
                    tdt = sndata.get(kk).getDewpoint();
                    db = sndata.get(kk - 1).getWindDirection() % 360.f;
                    dt = sndata.get(kk).getWindDirection() % 360.f;
                    sb = sndata.get(kk - 1).getWindSpeed();
                    st = sndata.get(kk).getWindSpeed();
                    zb = sndata.get(kk - 1).getGeoHeight();
                    zt = sndata.get(kk).getGeoHeight();
                    sl.setPressure(pres);

                    float rmult = (float) (Math.log(pres / pb) / Math.log(pt
                            / pb));
                    sl.setTemperature(tb + (tt - tb) * rmult);
                    sl.setDewpoint(tdb + (tdt - tdb) * rmult);
                    if (Math.abs(db - dt) > 180.) {
                        if (db < dt) {
                            db = db + 360.f;
                        } else {
                            dt = dt + 360.f;
                        }
                    }
                    sl.setWindDirection(db + (dt - db) * rmult);
                    sl.setWindSpeed(sb + (st - sb) * rmult);
                    sl.setGeoHeight(zb + (zt - zb) * rmult);
                    sls.add(sl);
                    return sls;
                }
            }
        }
        return missingSounding();
    }

    /*
     * Add data to output sounding profile.
     */
    public void addDataToList(int index, List<NcSoundingLayer> indat,
            List<NcSoundingLayer> outdat) {
        NcSoundingLayer sl = new NcSoundingLayer();
        sl.setPressure(indat.get(index).getPressure());
        sl.setTemperature(indat.get(index).getTemperature());
        sl.setDewpoint(indat.get(index).getDewpoint());
        sl.setWindDirection(indat.get(index).getWindDirection());
        sl.setWindSpeed(indat.get(index).getWindSpeed());
        sl.setGeoHeight(indat.get(index).getGeoHeight());
        sl.setOmega(indat.get(index).getOmega());
        outdat.add(sl);
    }

    /*
     * Set missing to output sounding profile.
     */
    public List<NcSoundingLayer> missingSounding() {
        List<NcSoundingLayer> outdat = new ArrayList<NcSoundingLayer>();
        NcSoundingLayer sl = new NcSoundingLayer();
        sl.setPressure(RMISSD);
        sl.setTemperature(RMISSD);
        sl.setDewpoint(RMISSD);
        sl.setWindDirection(RMISSD);
        sl.setWindSpeed(RMISSD);
        sl.setGeoHeight(RMISSD);
        sl.setOmega(RMISSD);
        outdat.add(sl);
        return outdat;
    }

    /*
     * Print the sounding data out.
     */
    public void printOut(List<NcSoundingLayer> sndata) {
        for (NcSoundingLayer soundLy : sndata) {
            System.out.print(" PRES: " + soundLy.getPressure() + " HGHT: "
                    + soundLy.getGeoHeight() + " TMPC: "
                    + soundLy.getTemperature() + " DWPC: "
                    + soundLy.getDewpoint() + " DRCT: "
                    + soundLy.getWindDirection() + " SPED: "
                    + soundLy.getWindSpeed() + " OMEG " + soundLy.getOmega()
                    + "\n");
        }
    }

    /*
     * Extrapolate missing height at the top level before sorting the data by
     * height.
     */
    private void fillMissingHeightAtTop(List<NcSoundingLayer> sndata) {
        int blev, kk;
        float pb, zb, tb, tdb;
        float pt, tt, tdt;
        float znew;
        boolean contin;

        int miss = 0;
        contin = true;
        if (sndata.get(sndata.size() - 1).getGeoHeight() != RMISSD)
            return;
        for (kk = sndata.size() - 1; kk > 0 && contin; kk--) {
            if (sndata.get(kk).getGeoHeight() == RMISSD) {
                miss++;
                if (kk == 0)
                    return;

                if (sndata.get(kk - 1).getGeoHeight() != RMISSD)
                    contin = false;
            }
        }

        miss++;
        if (miss > 0) {
            blev = sndata.size() - miss;
            pb = sndata.get(blev).getPressure();
            zb = sndata.get(blev).getGeoHeight();
            tb = sndata.get(blev).getTemperature();
            tdb = sndata.get(blev).getDewpoint();

            for (kk = blev + 1; kk < sndata.size(); kk++) {
                pt = sndata.get(kk).getPressure();
                tt = sndata.get(kk).getTemperature();
                tdt = sndata.get(kk).getDewpoint();
                float xxx = scaleHeight(tb, tt, tdb, tdt, pb, pt);
                znew = moistHeight(zb, pb, pt, xxx);
                if (znew == RMISSD) {
                    xxx = scaleHeight(tb, tb, tdb, tdb, pb, pt);
                    znew = moistHeight(zb, pb, pt, xxx);
                }

                if (znew != RMISSD) {
                    sndata.get(kk).setGeoHeight(znew);
                    pb = pt;
                    tb = tt;
                    tdb = tdt;
                    zb = znew;
                }
            }
        }
    }

    /*
     * Remove missing pressure from the sounding profile.
     */
    public List<NcSoundingLayer> removeMissingPressure(
            List<NcSoundingLayer> sndin) {
        List<NcSoundingLayer> sndout = new ArrayList<NcSoundingLayer>();

        for (int kk = 0; kk < sndin.size(); kk++) {
            if (sndin.get(kk).getPressure() > 0.) {
                addDataToList(kk, sndin, sndout);
            }
        }
        return sndout;
    }
}
