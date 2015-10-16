/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.SourceCompData;
import com.raytheon.uf.common.monitor.xml.SourceXML;

/**
 * Force FFG values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2011            mpduff     Initial creation
 * 01/14/13     1569       dhladky    changed arraylist to list
 * 04/15/13     1890       dhladky    Changed COUNTY to use constant
 * 05/10/13     1919       mpduff     If there are forced pfafs then the aggregate is forced.
 * 05/22/13     1902       mpduff     Added methods to get forced values.
 * 06/17/13     2085       njensen    Made forceIt() more thread safe
 * 12 Oct, 2015 4948       dhladky    Simplified Forcings, forcings now work for interpolated "fake" guidance.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FFFGForceUtil {

    private final FFMPResource resource;

    private SourceXML sourceXML1 = null;

    private SourceXML sourceXML2 = null;

    private final FFMPGuidanceInterpolation interp;

    private double src1Hr = -999;

    private double src2Hr = 999;

    private double sliderTime;

    public FFFGForceUtil(FFMPResource resource, String guidType) {
        this.resource = resource;
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();
        ArrayList<SourceCompData> sourceCompList = fdm.getUserXMLData();
        if ((sourceCompList == null) || (sourceCompList.size() == 0)) {
            sourceCompList = fdm.getMasterXMLData();
        }
        ArrayList<String> forcedSourceList = new ArrayList<String>();
        for (SourceCompData scd : sourceCompList) {
            forcedSourceList.add(scd.getSourceName());
        }

        interp = resource.getGuidanceInterpolators().get(guidType);
        if (interp != null) {
            FFMPSourceConfigurationManager srcManager = FFMPSourceConfigurationManager
                    .getInstance();
            if (interp.getSource1() != null) {
                sourceXML1 = srcManager.getSource(interp.getSource1());
            }

            if (interp.getSource2() != null) {
                sourceXML2 = srcManager.getSource(interp.getSource2());
            }

            if (sourceXML1 != null) {
                src1Hr = sourceXML1.getDurationHour();
            }
            if (sourceXML2 != null) {
                src2Hr = sourceXML2.getDurationHour();
            }

            // special case for slider less than 1
            if (src1Hr == src2Hr) {
                src1Hr = 0;
            }

            sliderTime = resource.getTime();
        }
    }

    public ForceUtilResult calculateForcings(String domain, FFMPTemplates ft,
            FFMPBasin cBasin) {
        return forceIt(ft, cBasin, null, domain);
    }

    public ForceUtilResult calculateForcings(List<Long> pfafList,
            FFMPTemplates ft, FFMPBasin cBasin) {
        return forceIt(ft, cBasin, pfafList, "NA");
    }

    /**
     * Force values for this given basin
     * @param ft
     * @param cBasin
     * @param pfafList
     * @param domain
     * @return
     */
    private ForceUtilResult forceIt(FFMPTemplates ft, FFMPBasin cBasin,
            List<Long> pfafList, String domain) {
        boolean forced = false;
        List<Long> forcedPfafList = new ArrayList<Long>();
        ForceUtilResult retVal = new ForceUtilResult(forced, pfafList,
                forcedPfafList);
        if (interp == null) {
            return retVal;
        }

        if (domain == null) {
            pfafList = ft.getAggregatePfafs(cBasin.getPfaf(),
                    resource.getSiteKey(), resource.getHuc());
        } else if (!domain.equals("NA")) {
            pfafList = ft.getAggregatePfafsByDomain(cBasin.getPfaf(),
                    resource.getSiteKey(), domain, resource.getHuc());
        } // else use the existing pfaf list

        // Add current pfaf to the list
        if ((pfafList != null) && (pfafList.size() == 0)) {
            if (cBasin.getAggregated()) {
                pfafList.add(ft.getAggregatedPfaf(cBasin.getPfaf(),
                        resource.getSiteKey(), resource.getHuc()));
            } else {
                pfafList.add(cBasin.getPfaf());
            }
        }

        FFFGDataMgr fdm = FFFGDataMgr.getInstance();

        if ((sliderTime >= src1Hr) && (sliderTime <= src2Hr)) {
            // Slider falls between the source times
            if (sliderTime == src1Hr) {
                forced = fdm.isForced(sourceXML1.getSourceName(),
                        cBasin.getPfaf());
                forcedPfafList = this.getForcedBasins(
                        sourceXML1.getSourceName(), pfafList, ft);
            } else if (sliderTime == src2Hr) {
                forced = fdm.isForced(sourceXML2.getSourceName(),
                        cBasin.getPfaf());
                forcedPfafList = this.getForcedBasins(
                        sourceXML2.getSourceName(), pfafList, ft);
            } else {
                if (cBasin.getAggregated()) {
                    forced = fdm.isForced(sourceXML1.getSourceName(),
                            cBasin.getPfaf());
                } else {
                    forced = fdm.isForced(sourceXML1.getSourceName(),
                            pfafList.get(0));
                }
                forcedPfafList = this.getForcedBasins(
                        sourceXML1.getSourceName(), pfafList, ft);
                if ((sourceXML2 != null)
                        && (forced == false)
                        && ((forcedPfafList == null) || (forcedPfafList.size() == 0))) {
                    if (cBasin.getAggregated()) {
                        forced = fdm.isForced(sourceXML2.getSourceName(),
                                cBasin.getPfaf());
                    } else {
                        forced = fdm.isForced(sourceXML2.getSourceName(),
                                pfafList.get(0));
                    }
                    forcedPfafList.addAll(this.getForcedBasins(
                            sourceXML2.getSourceName(), pfafList, ft));
                }
            }

            // Check if the aggregate is forced if the individual basin isn't
            if ((forced == false) && (forcedPfafList.size() == 0)) {
                if (!cBasin.getAggregated()) {
                    Long aggregate = ft.getAggregatedPfaf(cBasin.getPfaf(),
                            resource.getSiteKey(), resource.getHuc());
                    if (sourceXML1 != null) {
                        forced = fdm.isForced(sourceXML1.getSourceName(),
                                aggregate);
                        if (!forced && (sourceXML2 != null)) {
                            forced = fdm.isForced(sourceXML2.getSourceName(),
                                    aggregate);
                        }
                    }
                } else {
                    if (sourceXML1 != null) {
                        forced = fdm.isForced(sourceXML1.getSourceName(),
                                cBasin.getPfaf());
                        if (!forced && (sourceXML2 != null)) {
                            forced = fdm.isForced(sourceXML2.getSourceName(),
                                    cBasin.getPfaf());
                        }
                    }
                }
            } else if (!forcedPfafList.isEmpty()) {
                forced = true;
            }
        }

        retVal.forced = forced;
        retVal.pfafList = pfafList;
        retVal.forcedPfafList = forcedPfafList;
        return retVal;
    }

    /**
     * Get list of forced FFG basins
     * @param source
     * @param pfafList2
     * @param ft
     * @return
     */
    private ArrayList<Long> getForcedBasins(String source,
            List<Long> pfafList2, FFMPTemplates ft) {
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();
        ArrayList<Long> forcedList = new ArrayList<Long>();
        long prevCtyFips = 0l;
        for (int i = 0; i < pfafList2.size(); i++) {
            if (pfafList2.get(i) == null) {
                continue;
            }
            long pfaf = pfafList2.get(i);
            long countyFips = ft.getCountyFipsByPfaf(pfaf);

            if (countyFips != prevCtyFips) {
                if (fdm.isBasinForced(source, countyFips)) {
                    forcedList.add(pfaf);
                }
                prevCtyFips = countyFips;
            }
            if (fdm.isBasinForced(source, pfaf)) {
                if (!forcedList.contains(pfaf)) {
                    forcedList.add(pfaf);
                }
            }
        }

        return forcedList;
    }

    /**
     * Average forced FFG value for list of aggregate pfafs.
     * @param pfafList
     * @param forcedPfafs
     * @param interpolation
     * @param expiration
     * @param templates
     * @return
     */
    public float getAvgForcedValue(List<Long> pfafList, List<Long> forcedPfafs,
            FFMPGuidanceInterpolation interpolation, long expiration,
            FFMPTemplates templates) {
        float tvalue = 0.0f;
        float value;
        int i = 0;
        if (!interpolation.isInterpolate()) {
            FFFGDataMgr dman = FFFGDataMgr.getInstance();
            for (long pfaf : forcedPfafs) {
                long countyFips = templates.getCountyFipsByPfaf(pfaf);
                templates.getCountyFipsByPfaf(pfaf);
                value = dman.adjustValue(Float.NaN,
                        interpolation.getStandardSource(), pfaf, countyFips);

                tvalue += value;
                i++;
            }

            return tvalue / i;
        } else {
            return getInterpolatedForcedValue(interpolation, templates, forcedPfafs, INTERPOLATION_TYPE.AVG)/forcedPfafs.size(); 
        }
    }

    /**
     * Get the max forced value (max is smallest number for FFG)
     * 
     * @param pfafList
     *            list of pfaf ids
     * @param forcedPfafs
     *            list of forced pfaf ids
     * @param interpolation
     *            FFMPGuidanceInterpolation object
     * @param expiration
     *            force expiration
     * @param templates
     *            ffmp templates
     * @return max forced value
     */
    public float getMaxForcedValue(List<Long> pfafList, List<Long> forcedPfafs,
            FFMPGuidanceInterpolation interpolation, long expiration,
            FFMPTemplates templates) {
        float tvalue = 0.0f;
        float value;
        if (!interpolation.isInterpolate()) {
            FFFGDataMgr dman = FFFGDataMgr.getInstance();
            for (long pfaf : forcedPfafs) {
                long countyFips = templates.getCountyFipsByPfaf(pfaf);
                templates.getCountyFipsByPfaf(pfaf);
                value = dman.adjustValue(Float.NaN,
                        interpolation.getStandardSource(), pfaf, countyFips);

                if (value > tvalue) {
                    tvalue = value;
                }
            }

            return tvalue;

        } else {
            return getInterpolatedForcedValue(interpolation, templates, forcedPfafs, INTERPOLATION_TYPE.MAX); 
        }
    }

    /**
     * @param sliderTime
     *            the sliderTime to set
     */
    public void setSliderTime(double sliderTime) {
        this.sliderTime = sliderTime;
    }

    /**
     * Get the forced values for the pfaf list.
     * 
     * @param pfafList
     *            list of pfaf ids
     * @param forcedPfafs
     *            list of forced pfafs
     * @param ffmpGuidanceInterpolation
     *            FFMPGuidanceInterpolation object
     * @param guidSourceExpiration
     *            expiration time
     * @param ft
     *            ffmp templates
     * @return list of forced guidance values
     */
    public List<Float> getForcedGuidValues(List<Long> pfafList,
            List<Long> forcedPfafs,
            FFMPGuidanceInterpolation ffmpGuidanceInterpolation,
            long guidSourceExpiration, FFMPTemplates ft, String huc) {
        List<Float> guidList = new ArrayList<Float>();
        if (pfafList != null) {
            if (huc.equals(FFMPRecord.ALL) || huc.equals(FFMPRecord.COUNTY)) {
                // Add all pfafs in list
                for (Long pfaf : pfafList) {
                    if (pfaf == null) {
                        continue;
                    }
                    List<Long> pl = new ArrayList<Long>();
                    pl.add(pfaf);
                    float val = getAvgForcedValue(pl, forcedPfafs,
                            ffmpGuidanceInterpolation, guidSourceExpiration, ft);
                    guidList.add(val);
                }
            } else {
                // Only add aggregate value to guidList for numbered hucs (second pfaf in list for forcing).
                if (pfafList.get(1) != null) {
                    List<Long> pl = new ArrayList<Long>();
                    pl.add(pfafList.get(1));
                    float val = getAvgForcedValue(pl, forcedPfafs,
                            ffmpGuidanceInterpolation, guidSourceExpiration, ft);
                    guidList.add(val);
                }
            } 
        }

        return guidList;
    }
    
    /**
     * Calculate Interpolation of forced values for group of basins
     * 
     * @param interpolation
     * @param templates
     * @param forcedPfafs
     * @param type
     * @return
     */
    private Float getInterpolatedForcedValue(
            FFMPGuidanceInterpolation interpolation, FFMPTemplates templates,
            List<Long> forcedPfafs, INTERPOLATION_TYPE type) {

        float value1 = 0.0f;
        float value2 = 0.0f;
        String source1 = interpolation.getSource1();
        String source2 = interpolation.getSource2();
        double ratioOffset = interpolation.getInterpolationOffset();
        Float adjValue1 = 0.0f;
        Float adjValue2 = 0.0f;
        Float guidance = Float.NaN;
        Float tvalue = 0.0f;

        // interpolate from zero to first guidance
        if (source1.equals(source2)) {
            if ((ratioOffset == Double.NaN) || (ratioOffset == 0.0)) {
                adjValue2 = Float.NaN;
            }

            FFFGDataMgr dman = FFFGDataMgr.getInstance();
            for (long pfaf : forcedPfafs) {
                long countyFips = templates.getCountyFipsByPfaf(pfaf);
                templates.getCountyFipsByPfaf(pfaf);

                if (dman.isExpired() == false) {
                    adjValue2 = dman.adjustValue(adjValue2, source2, pfaf,
                            countyFips);
                }

                if (!adjValue2.isNaN()) {
                    value2 = adjValue2.floatValue();
                }

                // straight from awips1 code ( FFMPdataUtils.C )
                // We have an extrapolation to zero (the low side).
                // The formula below yields:
                // coeff = 0.62 for 0.25 time frame (endpoints.second)
                // coeff = 0.75 for 0.50 time frame (endpoints.second)
                // coeff = 0.88 for 0.75 time frame (endpoints.second)
                // coeff = 0.95 for 0.90 time frame (endpoints.second)
                // float mid, frac;
                // mid = endpoints.second / 2.0;
                // frac = 1.0 - ( ( duration - mid ) / mid );
                // coeff = ( duration / endpoints.second ) + (0.25 * frac);

                if ((interpolation.getHour(source1) == 0)
                        || (source1.equals(source2) && (interpolation
                                .getHour(source2) == 1))) {
                    Double ratio = new Double(ratioOffset);
                    if (ratio.equals(.25)) {
                        guidance = (float) (.62 * value2);
                    } else if (ratio.equals(.5)) {
                        guidance = (float) (.75 * value2);
                    } else if (ratio.equals(.75)) {
                        guidance = (float) (.88 * value2);
                    } else if (ratio.equals(.9)) {
                        guidance = (float) (.95 * value2);
                    }
                }

                if (type == INTERPOLATION_TYPE.MAX) {
                    if (guidance > tvalue) {
                        tvalue = guidance;
                    }
                } else if (type == INTERPOLATION_TYPE.AVG) {
                    tvalue += guidance;
                }
            }// otherwise interpolate linearly I guess
        } else {
            // check if values at the source do not exist
            FFFGDataMgr dman = FFFGDataMgr.getInstance();
            for (long pfaf : forcedPfafs) {
                long countyFips = templates.getCountyFipsByPfaf(pfaf);
                templates.getCountyFipsByPfaf(pfaf);

                if (dman.isExpired() == false) {
                    adjValue1 = dman.adjustValue(adjValue1, source1, pfaf,
                            countyFips);
                }

                if (!adjValue1.isNaN()) {
                    value1 = adjValue1.floatValue();
                }
                if (dman.isExpired() == false) {
                    adjValue2 = dman.adjustValue(adjValue2, source2, pfaf,
                            countyFips);
                }

                if (!adjValue2.isNaN()) {
                    value2 = adjValue2.floatValue();
                }

                if ((value1 == Float.NaN) || (value2 == Float.NaN)) {
                    guidance = Float.NaN;
                }

                guidance = (float) (value1 + ((value2 - value1) * ratioOffset));

                if (type == INTERPOLATION_TYPE.MAX) {
                    if (guidance > tvalue) {
                        tvalue = guidance;
                    }
                } else if (type == INTERPOLATION_TYPE.AVG) {
                    tvalue += guidance;
                }
            }
        }

        return tvalue;
    }
    
    /**
     * Find forced value for individual basin
     * 
     * @param templates
     * @param cBasinPfaf
     * @param guidType
     * @return
     */
    public Float getInterpolatedForcedValue(FFMPTemplates templates,
            long cBasinPfaf, String guidType) {

        float value1 = 0.0f;
        float value2 = 0.0f;
        FFMPGuidanceInterpolation interpolate = resource
                .getGuidanceInterpolators().get(guidType);
        String source1 = interpolate.getSource1();
        String source2 = interpolate.getSource2();
        double ratioOffset = interpolate.getInterpolationOffset();
        Float adjValue1 = 0.0f;
        Float adjValue2 = 0.0f;
        float guidance = 0.0f;

        // interpolate from zero to first guidance
        if (source1.equals(source2)) {
            if ((ratioOffset == Double.NaN) || (ratioOffset == 0.0)) {
                adjValue2 = Float.NaN;
            }

            FFFGDataMgr dman = FFFGDataMgr.getInstance();
            if (dman.isExpired() == false) {
                adjValue2 = dman
                        .adjustValue(adjValue2, source2, cBasinPfaf, 0l);
            }

            if (!adjValue2.isNaN()) {
                value2 = adjValue2.floatValue();
            }

            // straight from awips1 code ( FFMPdataUtils.C )
            // We have an extrapolation to zero (the low side).
            // The formula below yields:
            // coeff = 0.62 for 0.25 time frame (endpoints.second)
            // coeff = 0.75 for 0.50 time frame (endpoints.second)
            // coeff = 0.88 for 0.75 time frame (endpoints.second)
            // coeff = 0.95 for 0.90 time frame (endpoints.second)
            // float mid, frac;
            // mid = endpoints.second / 2.0;
            // frac = 1.0 - ( ( duration - mid ) / mid );
            // coeff = ( duration / endpoints.second ) + (0.25 * frac);

            if ((interpolate.getHour(source1) == 0)
                    || (source1.equals(source2) && (interpolate
                            .getHour(source2) == 1))) {
                Double ratio = new Double(ratioOffset);
                if (ratio.equals(.25)) {
                    guidance = (float) (.62 * value2);
                } else if (ratio.equals(.5)) {
                    guidance = (float) (.75 * value2);
                } else if (ratio.equals(.75)) {
                    guidance = (float) (.88 * value2);
                } else if (ratio.equals(.9)) {
                    guidance = (float) (.95 * value2);
                }
            }

            // otherwise interpolate linearly I guess

        } else {
            // check if values at the source do not exist
            FFFGDataMgr dman = FFFGDataMgr.getInstance();
            if (dman.isExpired() == false) {
                adjValue1 = dman
                        .adjustValue(adjValue1, source1, cBasinPfaf, 0l);
            }

            if (!adjValue1.isNaN()) {
                value1 = adjValue1.floatValue();
            }
            if (dman.isExpired() == false) {
                adjValue2 = dman
                        .adjustValue(adjValue2, source2, cBasinPfaf, 0l);
            }

            if (!adjValue2.isNaN()) {
                value2 = adjValue2.floatValue();
            }

            if ((value1 == Float.NaN) || (value2 == Float.NaN)) {
                guidance = Float.NaN;
            }

            guidance = (float) (value1 + ((value2 - value1) * ratioOffset));
        }
        
        return guidance;
    }
    
    /**
     * 
     * INTERPOLATION type MAX, AVG
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum INTERPOLATION_TYPE {

        MAX("MAX"), AVG("AVG");

        private final String itype;

        private INTERPOLATION_TYPE(String name) {
            itype = name;
        }

        public String getInterpolationType() {
            return itype;
        }
    };
}
