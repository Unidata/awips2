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
package com.raytheon.viz.radar.interrogators;

import java.util.Set;

import javax.measure.Measure;
import javax.measure.quantity.Angle;
import javax.measure.quantity.Length;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.raytheon.uf.viz.core.rsc.interrogation.StringInterrogationKey;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface for Radar Interrogators.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2010            mnash       Initial creation
 * Sep 13, 2016 3239       nabowle     Use the Interrogatable API.
 *
 * </pre>
 *
 * @author mnash
 */

public interface IRadarInterrogator {
    /*
     * StringInterrogationKey used to support
     * AbstractRadarResource#interrogate(ReferencedCoordinate)
     */

    public static final InterrogationKey<String> ICAO = new StringInterrogationKey<>(
            "ICAO", String.class);

    public static final StringInterrogationKey<Measure<? extends Number, Length>> RANGE = new StringInterrogationKey<>(
            "Range", Interrogator.getTypedMeasureClass());

    public static final InterrogationKey<Measure<? extends Number, Angle>> AZIMUTH = new StringInterrogationKey<>(
            "Azimuth", Interrogator.getTypedMeasureClass());

    public static final InterrogationKey<Measure<? extends Number, Length>> AGL = new StringInterrogationKey<>(
            "AGL", Interrogator.getTypedMeasureClass());

    public static final InterrogationKey<Measure<? extends Number, Length>> MSL = new StringInterrogationKey<>(
            "MSL", Interrogator.getTypedMeasureClass());

    public static final InterrogationKey<Number> SHEAR = new StringInterrogationKey<>(
            "Shear", Number.class);

    public static final StringInterrogationKey<double[]> CRS_LOCATION = new StringInterrogationKey<>(
            "crsLocation", double[].class);

    public static final InterrogationKey<Number> PRIMAY_ELEVATION_ANGLE = new StringInterrogationKey<>(
            "Angle", Number.class);

    public static final InterrogationKey<String> MNEMONIC = new StringInterrogationKey<>(
            "Mnemonic", String.class);

    /**
     * A String value useful for inspection. It may provide an already formatted
     * version of Value, or more information than Value can cover.
     */
    public static final InterrogationKey<String> VALUE_STRING = new StringInterrogationKey<>(
            "Value", String.class);

    public InterrogateMap sample(RadarRecord record, Coordinate latLon,
            ColorMapParameters params, Set<InterrogationKey<?>> keys);

    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys);

    public Set<InterrogationKey<?>> getInterrogationKeys();

    /**
     * Get the set of keys who's values are already included within the Value
     * String, if any.
     *
     * @return The set of keys who's values are already included within the
     *         Value String, if any.
     */
    public Set<InterrogationKey<?>> getValueStringKeys();
}
