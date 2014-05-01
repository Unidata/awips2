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
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.vividsolutions.jts.geom.Coordinate;

@XmlAccessorType(XmlAccessType.NONE)
public class ConvSigmetOutlookDataAdapter extends AbstractAdvisoryDataAdapter {

    private static final String LABEL_FORMAT = "%s Outlook";

    private static final String CLASS_TYPE = "OUTLOOK";

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
                    if (CLASS_TYPE.equals(section.getClassType())) {
                        AdvisoryRecord oRecord = convertSection(section);
                        if (oRecord != null) {
                            result.add(oRecord);
                        }
                    }

                }
            }
        }
        return result;
    }

    private AdvisoryRecord convertSection(ConvSigmetSection section) {
        Set<ConvSigmetLocation> locations = section.getConvSigmetLocation();
        if (locations == null || locations.size() == 0) {
            return null;
        }
        Coordinate[] coords = new Coordinate[locations.size()];
        for (ConvSigmetLocation loc : locations) {
            coords[loc.getIndex() - 1] = new Coordinate(loc.getLongitude(),
                    loc.getLatitude());
        }
        String label = String.format(LABEL_FORMAT, section.getSequenceID());
        AdvisoryRecord aRecord = new AdvisoryRecord(coords, label,
                section.getSegment());
        return aRecord;

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
