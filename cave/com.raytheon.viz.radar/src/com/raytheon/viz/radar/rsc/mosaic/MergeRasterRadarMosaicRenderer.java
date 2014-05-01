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
package com.raytheon.viz.radar.rsc.mosaic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.core.rsc.BestResResource;
import com.raytheon.viz.radar.rsc.MosaicPaintProperties;
import com.raytheon.viz.radar.rsc.RadarImageResource;
import com.raytheon.viz.radar.rsc.mosaic.RadarMosaicRendererFactory.IRadarMosaicRenderer;

/**
 * mosaic renderer that merges large low res raster products with smaller
 * highres raster products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class MergeRasterRadarMosaicRenderer implements IRadarMosaicRenderer {

    private ResourcePair largest = null;

    private Set<List<RadarRecord>> recordLists = new HashSet<List<RadarRecord>>();

    @Override
    public void dispose() {

    }

    @Override
    public void mosaic(IGraphicsTarget target, PaintProperties paintProps,
            RadarMosaicResource mosaicToRender) throws VizException {
        if (((MosaicPaintProperties) paintProps).isForceRepaint()) {
            double largestSize = Double.MIN_VALUE;
            List<RadarRecord> records = new ArrayList<RadarRecord>();
            // collect the records.
            for (ResourcePair rp : mosaicToRender.getResourceList()) {
                if (rp.getResource() != null) {
                    AbstractVizResource<?, ?> rsc = rp.getResource();
                    DataTime time = mosaicToRender.getTimeForResource(rsc);
                    if (rsc instanceof BestResResource) {
                        rsc = ((BestResResource) rsc).getBestResResource(time);
                    }
                    if (rsc instanceof RadarImageResource) {

                        RadarRecord rr = ((RadarImageResource<?>) rsc)
                                .getRadarRecord(time);
                        if (rr != null) {
                            records.add(rr);
                            double size = RadarUtil.calculateExtent(rr);
                            if (size > largestSize) {
                                largestSize = size;
                                largest = rp;
                            }
                        }
                    }
                }
            }
            // Sort them from lowest res to highest res
            Collections.sort(records, new Comparator<RadarRecord>() {

                @Override
                public int compare(RadarRecord rec1, RadarRecord rec2) {
                    Integer res1 = rec1.getGateResolution();
                    Integer res2 = rec2.getGateResolution();
                    return res1.compareTo(res2);
                }

            });
            // merge each lower res one with a high res one
            if (!recordLists.contains(records)) {
                for (RadarRecord record : records) {
                    Iterator<List<RadarRecord>> itr = recordLists.iterator();
                    while (itr.hasNext()) {
                        List<RadarRecord> recordList = itr.next();
                        if (recordList.contains(record)) {
                            itr.remove();
                        }
                    }
                }
                recordLists.add(records);
            }
            String format = null;
            if (!records.isEmpty()) {
                format = records.get(0).getFormat();
            }
            Set<DataTime> timesToClear = null;
            if ("Raster".equals(format)) {
                timesToClear = mergeRaster(records);
            } else if ("Radial".equals(format)) {
                timesToClear = mergeRadial(records);
            }
            for (ResourcePair rp : mosaicToRender.getResourceList()) {
                if (rp.getResource() != null) {
                    AbstractVizResource<?, ?> rsc = rp.getResource();
                    for (DataTime time : timesToClear) {
                        if (rsc instanceof BestResResource) {
                            rsc = ((BestResResource) rsc)
                                    .getBestResResource(time);
                        }
                        if (rsc == null) {
                            continue;
                        }
                        if (rsc instanceof RadarImageResource) {
                            ((RadarImageResource<?>) rsc).redoImage(time);
                        }
                    }
                }
            }
        }
        for (ResourcePair rp : mosaicToRender.getResourceList()) {
            if (rp.getResource() != null) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                DataTime time = mosaicToRender.getTimeForResource(rsc);
                if (rsc instanceof BestResResource) {
                    rsc = ((BestResResource) rsc).getBestResResource(time);
                }
                if (rsc == null) {
                    continue;
                }
                paintProps.setDataTime(time);
                if (rp == largest || !(rsc instanceof RadarImageResource)) {
                    rsc.paint(target, paintProps);
                } else {
                    ((RadarImageResource<?>) rsc)
                            .paintRadar(target, paintProps);
                }
            }

        }
    }

    private Set<DataTime> mergeRaster(List<RadarRecord> records)
            throws VizException {
        Set<DataTime> result = new HashSet<DataTime>();
        for (int c = 1; c < records.size(); c++) {
            RadarRecord rr1 = records.get(c - 1);
            RadarRecord rr2 = records.get(c);

            Unit<?> unit1 = rr1.getDataUnit();
            Unit<?> unit2 = rr2.getDataUnit();
            if (!unit2.isCompatible(unit1)) {
                // No joining can occur
                continue;
            }
            UnitConverter converter = unit2.getConverterTo(unit1);

            int ratio = rr2.getGateResolution() / rr1.getGateResolution();
            int nx = rr1.getNumBins() / ratio;
            int ny = rr1.getNumRadials() / ratio;
            byte[] rawData1 = rr1.getRawData();
            byte[] rawData2 = rr2.getRawData();

            boolean rawData1Changed = false;
            boolean rawData2Changed = false;
            int cx = rr1.getNumBins() / 2;
            int cy = rr1.getNumRadials() / 2;
            // 99/200 is magic from A1. It makes the acceptable circle
            // sligtly smaller(like 3 pixels) than the size of the data
            int rSqr = (Math.max(rr1.getNumBins(), rr1.getNumRadials())) * 99 / 200;
            rSqr = rSqr * rSqr;
            // i,j is the coordinate in the highres
            // i2,j2 is the coordinate in the low res
            // copy the low res pixels into highres outside the radius
            for (int i = 0; i < rr1.getNumBins(); i++) {
                int i2 = ((rr2.getNumBins() - nx) / 2) + i / ratio;
                for (int j = 0; j < rr1.getNumRadials(); j++) {
                    int j2 = ((rr2.getNumRadials() - ny) / 2) + j / ratio;
                    int dSqr = (cx - i) * (cx - i) + (cy - j) * (cy - j);
                    int index1 = j * rr1.getNumBins() + i;
                    int index2 = j2 * rr2.getNumBins() + i2;
                    if (dSqr >= rSqr && rawData1[index1] == 0
                            && rawData2[index2] != 0) {
                        rawData1[index1] = (byte) converter
                                .convert(rawData2[index2]);
                        rawData1Changed = true;
                    }
                }
            }
            // blank out the middle of low res
            for (int i = 0; i < rr1.getNumBins(); i++) {
                int i2 = ((rr2.getNumBins() - nx) / 2) + i / ratio;
                for (int j = 0; j < rr1.getNumRadials(); j++) {
                    int j2 = ((rr2.getNumRadials() - ny) / 2) + j / ratio;
                    int index2 = j2 * rr2.getNumBins() + i2;
                    if (rawData2[index2] != 0) {
                        rawData2[index2] = 0;
                        rawData2Changed = true;
                    }
                }
            }
            if (rawData1Changed || rawData2Changed) {
                result.add(rr1.getDataTime());
            }
            if (rawData2Changed) {
                result.add(rr2.getDataTime());
            }
        }
        return result;
    }

    private Set<DataTime> mergeRadial(List<RadarRecord> records)
            throws VizException {
        Set<DataTime> result = new HashSet<DataTime>();
        for (int c = 1; c < records.size(); c++) {
            RadarRecord rr1 = records.get(c - 1);
            RadarRecord rr2 = records.get(c);

            int numBinsToClear = rr1.getNumBins() * rr1.getGateResolution()
                    / rr2.getGateResolution();

            byte[] rawData2 = rr2.getRawData();
            boolean rawData2Changed = false;

            // blank out the middle of low res
            for (int i = 0; i < numBinsToClear; i++) {
                for (int j = 0; j < rr2.getNumRadials(); j++) {
                    int index = j * rr2.getNumBins() + i;
                    if (rawData2[index] != 0) {
                        rawData2[index] = 0;
                        rawData2Changed = true;
                    }
                }
            }

            if (rawData2Changed) {
                result.add(rr2.getDataTime());
            }
        }
        return result;
    }

}
