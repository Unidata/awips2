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
package com.raytheon.uf.edex.ogc.common.db;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.lang3.time.DateUtils;
import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.ReferenceIdentifier;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcDimension;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Converts layer objects from storage to intermediate OGC metadata layer
 * objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  Jun 13, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 * @param <DIMENSION>
 * @param <L>
 */
public class LayerTransformer<DIMENSION extends SimpleDimension, L extends SimpleLayer<DIMENSION>> {

    protected static IUFStatusHandler log = UFStatus
            .getHandler(LayerTransformer.class);

    public enum TimeFormat {
        LIST, HOUR_RANGES
    };

    protected String key;

    protected ILayerCache<DIMENSION, L> lcache;

    protected static String timeUnit = "ISO8601";

    public static final Pattern frontDot = Pattern
            .compile("^(-?[0-9]*\\.?[0-9]+)(.*)$");

    public static final Pattern backDot = Pattern
            .compile("^(-?[0-9]+\\.?[0-9]*)(.*)$");

    /**
     * Construct a LayerTransformer that uses a different layerClass and
     * layerWrapperClass than the default SimpleLayer and LayerHolder
     * 
     * @param key
     * @param layerTable
     * @param das
     * @param layerClass
     * @param layerHolderClass
     */
    public LayerTransformer(String key, ILayerCache<DIMENSION, L> lcache) {
        this.key = key;
        this.lcache = lcache;
    }

    /**
     * @param name
     * @return null if layer not found
     * @throws DataAccessLayerException
     */
    public L find(String name) throws OgcException {
        return lcache.getLayer(name);
    }

    /**
     * @param layer
     * @param dimension
     * @return null if layer/dimension not found
     * @throws DataAccessLayerException
     */
    public DIMENSION getDimension(String layer, String dimension)
            throws OgcException {
        L l = find(layer);
        if (l == null) {
            return null;
        }
        return l.getDimension(dimension);
    }

    /**
     * Get all dimensions that start with prefix. Case insensitive.
     * 
     * @param layer
     * @param prefix
     * @return empty list if layer/dimension not found
     */
    public static <DIMENSION extends SimpleDimension, L extends SimpleLayer<DIMENSION>> List<DIMENSION> getDimsByPrefix(
            L layer, String prefix) {
        List<DIMENSION> rval = new ArrayList<DIMENSION>(2);
        if (layer == null) {
            return rval;
        }
        for (DIMENSION d : layer.getDimensions()) {
            String lower = d.getName().toLowerCase();
            if (lower.startsWith(prefix.toLowerCase())) {
                rval.add(d);
            }
        }
        return rval;
    }

    /**
     * @param dim
     * @return empty set if dim is null or doesn't have any parsable values
     */
    public static TreeSet<Double> getDimValuesAsDouble(SimpleDimension dim) {
        TreeSet<Double> rval = new TreeSet<Double>();
        if (dim == null) {
            return rval;
        }
        for (String val : dim.getValues()) {
            try {
                Matcher m = frontDot.matcher(val);
                if (m.matches()) {
                    val = m.group(1);
                } else {
                    m = backDot.matcher(val);
                    if (m.matches()) {
                        val = m.group(1);
                    }
                }
                rval.add(Double.parseDouble(val));
            } catch (Throwable e) {
                // continue
            }
        }
        return rval;
    }

    /**
     * @param layer
     * @return
     */
    public List<OgcBoundingBox> getBoundingBoxes(L layer) {
        String crs = layer.getTargetCrsCode();
        double minx = layer.getTargetMinx();
        double maxx = layer.getTargetMaxx();
        double miny = layer.getTargetMiny();
        double maxy = layer.getTargetMaxy();
        OgcBoundingBox rval = new OgcBoundingBox(crs, minx, maxx, miny, maxy);
        return Arrays.asList(rval);
    }

    public static String getCrsName(CoordinateReferenceSystem crs) {
        if (crs == null) {
            return null;
        }
        Set<ReferenceIdentifier> ids = crs.getIdentifiers();
        if (ids == null || ids.isEmpty()) {
            return crs.getName().toString();
        } else {
            return ids.iterator().next().toString();
        }
    }

    /**
     * @param layer
     * @return
     */
    public static <L extends SimpleLayer<?>> OgcGeoBoundingBox getGeoBoundingBox(
            L layer) {
        Polygon crs84Bounds = layer.getCrs84Bounds();
        if (crs84Bounds == null) {
            return null;
        }
        ReferencedEnvelope env = JTS.toEnvelope(crs84Bounds);
        return new OgcGeoBoundingBox(env);
    }

    /**
     * @param layer
     * @param tformat
     * @return
     */
    public static <L extends SimpleLayer<?>> List<String> getTimes(L layer) {
        return getTimes(layer, TimeFormat.LIST);
    }

    /**
     * @param layer
     * @param tformat
     * @return
     */
    public static <L extends SimpleLayer<?>> List<String> getTimes(L layer,
            TimeFormat tformat) {
        List<String> rval;
        // TODO this could be adapted to a pattern that allows for formatters to
        // be externally added
        switch (tformat) {
        case LIST:
            rval = getTimesAsList(layer);
            break;
        case HOUR_RANGES:
            rval = getTimesAsHourRanges(layer);
            break;
        default:
            throw new IllegalArgumentException("No handler for time format: "
                    + tformat);
        }
        return rval;
    }

    /**
     * @param layer
     * @return
     */
    protected static <L extends SimpleLayer<?>> List<String> getTimesAsHourRanges(
            L layer) {
        Set<Date> times = layer.getTimes();
        if (times == null || times.isEmpty()) {
            return new ArrayList<String>(0);
        }
        if (times.size() % 2 != 0) {
            String msg = "Odd number of times for layer " + layer.getName()
                    + ". Should be even to construct time ranges.";
            log.warn(msg + " Dropping last time");
        }

        Iterator<Date> iter = times.iterator();
        int ranges = times.size() / 2;
        List<String> rval = new ArrayList<String>(ranges);
        for (int i = 0; i < ranges; ++i) {
            Date start = iter.next();
            Date end = iter.next();
            rval.add(formatRange(start, end));
        }

        return rval;
    }

    protected static String formatRange(Date start, Date end) {
        return format(start) + '/' + format(end) + "/0";
    }

    /**
     * End a range started by startRange()
     * 
     * @param d
     *            start of range
     * @param i
     *            iterator to rest of times that possibly include the rest of
     *            the range
     * @param sb
     *            where the formatted output goes
     * @return the start of the next range, null if there are no more ranges
     */
    protected static Date endRange(Date d, Iterator<Date> i, StringBuilder sb) {
        long milliStart = d.getTime();
        long milliPrev = milliStart;
        Date rval = null;
        Date prev = null;
        Date curr = null;
        while (i.hasNext()) {
            curr = i.next();
            if (curr.getTime() - milliPrev > TimeUtil.MILLIS_PER_HOUR) {
                // we've reached the end of the range return rval
                rval = curr;
                break;
            }
            prev = curr;
            milliPrev = prev.getTime();
        }
        if (prev == null) {
            // iterator didn't have anything
            prev = new Date(milliStart + TimeUtil.MILLIS_PER_HOUR);
        } else {
            // we want the range to end at the next hour
            prev = new Date(prev.getTime() + TimeUtil.MILLIS_PER_HOUR);
        }
        sb.append(format(prev));
        // FIXME 0 indicates a continuum range, we should support discrete
        // periods in the range
        sb.append("/0");
        return rval;
    }

    public static String format(Date d) {
        Calendar c = GregorianCalendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTime(d);
        return DatatypeConverter.printDateTime(c);
    }

    protected static void startRange(StringBuilder sb, Date d) {
        sb.append(format(d));
        sb.append('/');
    }

    protected static <L extends SimpleLayer<?>> List<String> getTimesAsList(
            L layer) {
        Set<Date> times = layer.getTimes();
        if (times == null || times.isEmpty()) {
            return new ArrayList<String>(0);
        }
        TreeSet<Date> sorted = new TreeSet<Date>(times);
        Iterator<Date> i = sorted.iterator();
        List<String> rval = new ArrayList<String>(sorted.size());
        while (i.hasNext()) {
            rval.add(format(i.next()));
        }
        return rval;
    }

    public TreeSet<Date> getAllTimes() throws OgcException {
        TreeSet<Date> rval = new TreeSet<Date>();
        List<L> layers = getLayers();
        for (L l : layers) {
            rval.addAll(l.getTimes());
        }
        return rval;
    }

    public List<L> getLayers() throws OgcException {
        return lcache.getLayers();
    }

    public List<OgcLayer> getLayersAsOgc(TimeFormat tformat, StyleLookup lookup)
            throws OgcException {
        return transform(getLayers(), tformat, lookup);
    }

    public List<OgcLayer> getLayersAsOgc(StyleLookup lookup)
            throws OgcException {
        return getLayersAsOgc(TimeFormat.LIST, lookup);
    }

    /**
     * @param layers
     * @return
     */
    protected List<OgcLayer> transform(List<L> layers, TimeFormat tformat,
            StyleLookup lookup) {
        List<OgcLayer> rval = new ArrayList<OgcLayer>(layers.size());
        for (L simple : layers) {
            rval.add(transform(simple, tformat, lookup));
        }
        return rval;
    }

    /**
     * Transform a simple layer as represented in the data storage area to an
     * OgcLayer that can be viewed through getCapabilities
     * <p>
     * Override this method to add additional Dimensions.
     * 
     * @param layer
     * @return
     */
    public OgcLayer transform(L layer, TimeFormat tformat, StyleLookup lookup) {
        OgcLayer rval = new OgcLayer();
        String name = layer.getName();
        rval.setName(key, name);
        rval.setTitle(name);
        setStylesForLayer(rval.getName(), layer, rval, lookup);
        OgcDimension timeDim = new OgcDimension("time", timeUnit,
                layer.getTimeEntries());
        String defaultTime = layer.getDefaultTimeEntry();
        timeDim.setDefaultVal(defaultTime);
        rval.addDimension(timeDim);
        for (OgcDimension dim : getDims(layer, layer.getDimensions())) {
            rval.addDimension(dim);
        }
        rval.setGeoBoundingBox(getGeoBoundingBox(layer));
        rval.setBoundingBox(getBoundingBoxes(layer));
        // rval.setCrs(Arrays.asList(layer.getTargetCrs()));
        return rval;
    }

    public static String getTimeRange(Date d) {
        Date start = DateUtils.truncate(d, Calendar.HOUR);
        Date end = DateUtils.addHours(start, 1);
        return format(start) + '/' + format(end);
    }

    protected void setStylesForLayer(String layerName, L layer, OgcLayer rval,
            StyleLookup lookup) {
        if (lookup != null) {
            String style = lookup.lookup(layer.getName());
            if (style != null) {
                OgcStyle ogcstyle = new OgcStyle(style);
                ogcstyle.setLegendUrl(createLegendUrl(layerName, style));
                ogcstyle.setDefault(true);
                rval.setStyles(Arrays.asList(ogcstyle));
            }
        }
    }

    public static String createLegendUrl(String layerName, String styleName) {
        String url = "&layer=" + layerName + "&style=" + styleName
                + "&format=image/png";
        return url.replaceAll(" ", "%20");
    }

    /**
     * @param layer
     * @return
     */
    protected OgcDimension[] getDims(L layer, Set<DIMENSION> dims) {
        if (dims == null) {
            return new OgcDimension[0];
        }
        List<OgcDimension> rval = new ArrayList<OgcDimension>(dims.size());
        for (DIMENSION dim : dims) {
            OgcDimension ogcdim = transform(layer, dim);
            if (ogcdim != null) {
                rval.add(ogcdim);
            }
        }
        return rval.toArray(new OgcDimension[rval.size()]);
    }

    /**
     * @param simpleDimension
     * @return
     */
    protected OgcDimension transform(L layer, DIMENSION dim) {
        if (dim == null || dim.getName() == null || dim.getValues() == null) {
            return null;
        }
        OgcDimension rval;
        List<String> values = new ArrayList<String>(dim.getValues());
        if (dim.getUnits() == null) {
            rval = new OgcDimension(dim.getName(), values);
        } else {
            rval = new OgcDimension(dim.getName(), dim.getUnits(), values);
        }
        rval.setDefaultVal(dim.getDefaultValue(layer));
        return rval;
    }

    protected OgcLayer transform(L layer, StyleLookup lookup) {
        return transform(layer, TimeFormat.LIST, lookup);
    }

    /**
     * @param layerName
     * @return null if layer name isn't found
     * @throws DataAccessLayerException
     */
    public Date getLatestTime(String layerName) throws OgcException {
        L simpleLayer = find(layerName);
        return getLatestTime(simpleLayer);
    }

    /**
     * @param layer
     * @return null if layer name isn't found
     */
    public static <L extends SimpleLayer<?>> Date getLatestTime(L layer) {
        if (layer == null) {
            return null;
        }
        Set<Date> times = layer.getTimes();
        TreeSet<Date> sorted = new TreeSet<Date>(times);
        return sorted.last();
    }

    /**
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * @return
     * @throws OgcException
     */
    public TreeSet<Date> getLatestTimes() throws OgcException {
        List<L> layers = getLayers();
        TreeSet<Date> rval = new TreeSet<Date>();
        for (L l : layers) {
            TreeSet<Date> times = new TreeSet<Date>(l.getTimes());
            rval.add(times.last());
        }
        return rval;
    }

    /**
     * @return the lcache
     */
    public ILayerCache<DIMENSION, L> getLcache() {
        return lcache;
    }

}
