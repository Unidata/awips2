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
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FFFGForceUtil {
    private boolean forced = false;

    private List<Long> forcedPfafList = new ArrayList<Long>();

    private List<Long> pfafList = new ArrayList<Long>();

    private final FFMPResource resource;

    private String domain = "NA";

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

    public void calculateForcings(String domain, FFMPTemplates ft,
            FFMPBasin cBasin) {
        this.domain = domain;
        forceIt(ft, cBasin);
    }

    public void calculateForcings(List<Long> pfafList, FFMPTemplates ft,
            FFMPBasin cBasin) {
        this.pfafList = pfafList;
        forceIt(ft, cBasin);
    }

    private void forceIt(FFMPTemplates ft, FFMPBasin cBasin) {
        if (interp == null) {
            return;
        }

        if (domain == null) {
            pfafList = ft.getAggregatePfafs(cBasin.getPfaf(),
                    resource.getSiteKey(), resource.getHuc());
        } else if (!domain.equals("NA")) {
            if (!resource.getHuc().equals(FFMPRecord.ALL)) {
                pfafList = ft.getAggregatePfafsByDomain(cBasin.getPfaf(),
                        resource.getSiteKey(), domain, resource.getHuc());
            }
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
    }

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

    public float getAvgForcedValue(List<Long> pfafList, List<Long> forcedPfafs,
            FFMPGuidanceInterpolation interpolation, long expiration,
            FFMPTemplates templates) {
        float tvalue = 0.0f;
        float value;
        int i = 0;
        if (interpolation.isInterpolate() == false) {
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
        }

        return Float.NaN;
    }

    /**
     * @return the forced
     */
    public boolean isForced() {
        return forced;
    }

    /**
     * @param forced
     *            the forced to set
     */
    public void setForced(boolean forced) {
        this.forced = forced;
    }

    /**
     * @return the forcedPfafList
     */
    public List<Long> getForcedPfafList() {
        return forcedPfafList;
    }

    /**
     * @param forcedPfafList
     *            the forcedPfafList to set
     */
    public void setForcedPfafList(ArrayList<Long> forcedPfafList) {
        this.forcedPfafList = forcedPfafList;
    }

    /**
     * @return the pfafList
     */
    public List<Long> getPfafList() {
        return pfafList;
    }

    /**
     * @param sliderTime
     *            the sliderTime to set
     */
    public void setSliderTime(double sliderTime) {
        this.sliderTime = sliderTime;
    }
}
