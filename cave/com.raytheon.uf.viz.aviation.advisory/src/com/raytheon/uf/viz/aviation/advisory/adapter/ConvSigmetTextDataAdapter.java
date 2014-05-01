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
package com.raytheon.uf.viz.aviation.advisory.adapter;

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;

import java.util.ArrayList;
import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.vividsolutions.jts.geom.Coordinate;

@XmlAccessorType(XmlAccessType.NONE)
public class ConvSigmetTextDataAdapter extends AbstractAdvisoryDataAdapter {

    private static final String FORMAT = "%s - %s %s %s %dKFT\n%s %s %s";

    private static final float LINE_WIDTH = 1.0f;

    private static final LineStyle LINE_STYLE = LineStyle.SOLID;

    @Override
    public Collection<AdvisoryRecord> convertRecords(
            Collection<PluginDataObject> records) {
        Collection<AdvisoryRecord> result = new ArrayList<AdvisoryRecord>();
        for (PluginDataObject record : records) {
            result.addAll(convertRecord(record));
        }
        return result;
    }

    @Override
    public Collection<AdvisoryRecord> convertRecord(PluginDataObject record) {
        Collection<AdvisoryRecord> result = new ArrayList<AdvisoryRecord>();
        if (record instanceof ConvSigmetRecord) {
            ConvSigmetRecord sigmetRecord = (ConvSigmetRecord) record;
            if (sigmetRecord.getConvSigmetSection() != null) {
                for (ConvSigmetSection section : sigmetRecord
                        .getConvSigmetSection()) {
                    AdvisoryRecord oRecord = convertSection(section);
                    if (oRecord != null) {
                        result.add(oRecord);
                    }
                }
            }
        }
        return result;
    }

    private AdvisoryRecord convertSection(ConvSigmetSection section) {
        if (section.getCloudTop() != null
                && section.getConvSigmetLocation() != null) {
            for (ConvSigmetLocation loc : section.getConvSigmetLocation()) {
                if (loc.getIndex() == 1) {
                    Coordinate coord = new Coordinate(loc.getLongitude(),
                            loc.getLatitude());
                    String label = getText(section);
                    AdvisoryRecord aRecord = new AdvisoryRecord(coord, label);
                    return aRecord;
                }
            }
        }
        return null;
    }

    protected String getText(ConvSigmetSection section) {
        // decode the storm type from the section
        String stormType = "???? TS";
        if (section.getSegment().contains("SVR TS")
                || section.getSegment().contains("SEV TS")) {
            stormType = "SVR TS";
        } else if (section.getSegment().contains("EMBD TS")
                || section.getSegment().contains("EMBDD TS")) {
            stormType = "EMBDD TS";
        } else if (section.getSegment().contains("TS")) {
            stormType = "TS";
        }
        // sequenceId was decoded in the decoder
        String sequenceId = section.getSequenceID();
        if (sequenceId == null) {
            sequenceId = "";
        }
        // intensity was decoded in the decoder
        String intensity = section.getIntensity();
        if (intensity == null) {
            intensity = "";
        }
        // cloud top was decoded in the deocder.
        String cloudTop = section.getCloudTop();
        if (cloudTop == null) {
            cloudTop = "";
        }
        // flight level is in hft, convert to kft
        int flightLevel = section.getFlightLevel() / 10;
        // decode last line to determine these values
        String hailTo = "";
        String tornadoes = "";
        String windTo = "";
        String[] segmentLines = section.getSegment().split("\n");
        if (segmentLines.length > 5) {
            String line2data = segmentLines[5].trim();
            if (!line2data.isEmpty()) {
                for (String part : line2data.split("\\.\\.\\.")) {
                    if (part.startsWith("HAIL TO")) {
                        hailTo = part.replace(" IN", "\"");
                    } else if (part.startsWith("WIND GUSTS TO")) {
                        windTo = part;
                    } else if (part.startsWith("TORNADOES")) {
                        tornadoes = part;
                    }
                }

            }
        }
        return String.format(FORMAT, sequenceId, intensity, stormType,
                cloudTop, flightLevel, hailTo, tornadoes, windTo).trim();
    }

    @Override
    public float getLineWidth() {
        return LINE_WIDTH;
    }

    @Override
    public LineStyle getLineStyle() {
        return LINE_STYLE;
    }

}
