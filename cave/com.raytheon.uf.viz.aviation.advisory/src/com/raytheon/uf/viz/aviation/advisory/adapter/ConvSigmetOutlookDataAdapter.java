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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Set;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import org.locationtech.jts.geom.Coordinate;

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;

/**
 *
 * Aviation data adapter for Conv Sigmet for Now cast and Forecast. This class
 * adds wx statement text to sigmet polygons in d2d.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * ????    2001  ????     ????      Initial creation
 * Jul 24, 2020  79536    pbutler   D2D and wx statements to hover over Sigmet polygons.
 * Jun  10, 2021  85216    omoncayo  C-SIGMET OUTLOOK Text Label enhancement.
 *
 * </pre>
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ConvSigmetOutlookDataAdapter extends AbstractAdvisoryDataAdapter {

    private static final String FORMAT = "Valid UNTIL %02d%02d%02d\n%s";

    private static final String LABEL_FORMAT = "%s OUTLOOK";

    private static final String SEGMENT_SEPERATOR = "\n.  \n";

    private static final String CLASS_TYPE = "OUTLOOK";

    private static final float LINE_WIDTH = 1.0f;

    private static final LineStyle LINE_STYLE = LineStyle.SOLID;

    @Override
    public Collection<AdvisoryRecord> convertRecords(
            Collection<PluginDataObject> records) {
        Collection<AdvisoryRecord> result = new ArrayList<>();
        for (PluginDataObject record : records) {
            result.addAll(convertRecord(record));
        }
        return result;
    }

    @Override
    public Collection<AdvisoryRecord> convertRecord(PluginDataObject record) {
        Collection<AdvisoryRecord> result = new ArrayList<>();
        if (record instanceof ConvSigmetRecord) {
            ConvSigmetRecord sigmetRecord = (ConvSigmetRecord) record;
            if (sigmetRecord.getConvSigmetSection() != null) {
                for (ConvSigmetSection section : sigmetRecord
                        .getConvSigmetSection()) {
                    if (CLASS_TYPE.equals(section.getClassType())) {
                        // ----- convert section -------
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
        if (locations == null || locations.isEmpty()) {
            return null;
        }
        Coordinate[] coords = new Coordinate[locations.size()];
        for (ConvSigmetLocation loc : locations) {

            coords[loc.getIndex() - 1] = new Coordinate(loc.getLongitude(),
                    loc.getLatitude());

        }
        String sequenceId = section.getSequenceID();
        if (sequenceId == null) {
            sequenceId = "";
        } else if (sequenceId.length() >= 3) {
            sequenceId = sequenceId.substring(0, 3);
        }
        Calendar endTime = section.getEndTime();
        int day = 0;
        int hour = 0;
        int min = 0;
        if (endTime != null) {
            day = endTime.get(Calendar.DAY_OF_MONTH);
            hour = endTime.get(Calendar.HOUR_OF_DAY);
            min = endTime.get(Calendar.MINUTE);
        }
        String segment = section.getSegment();
        if (segment != null) {
            segment = segment.split(SEGMENT_SEPERATOR)[0];
        } else {
            segment = "";
        }

        String inspectString = String.format(FORMAT, day, hour, min, segment);

        String label = String.format(LABEL_FORMAT, sequenceId);

        AdvisoryRecord aRecord = null;

        if (coords.length >= 4) {
            aRecord = new AdvisoryRecord(coords, label, inspectString);
        }

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
