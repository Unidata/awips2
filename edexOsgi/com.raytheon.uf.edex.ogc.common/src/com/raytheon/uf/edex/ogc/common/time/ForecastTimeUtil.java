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
package com.raytheon.uf.edex.ogc.common.time;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.DatatypeConverter;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;

/**
 * Utility methods and constants for forecast times. Used to find valid reftime,
 * forecast time and valid time combinations from OGC layer metadata.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ForecastTimeUtil {

	protected static final String refKey = "REFTIME";

	protected static final String fcstKey = "FORECAST_OFFSET";

	protected Pattern fcstPattern = Pattern
			.compile("^([0-9]+)([sSmMhHdD]?).*$");

	protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

	public static final byte NONE = 0x00;

	public static final byte VALID = 0x01;

	public static final byte FCST = 0x02;

	public static final byte FCST_VALID = 0x03;

	public static final byte REF = 0x04;

	public static final byte REF_VALID = 0x05;

	public static final byte REF_FCST = 0x06;

	public static final byte REF_FCST_VALID = 0x07;

	/**
	 * Gets a list of datatimes for specified layer. If dimensions has values
	 * for {@link ForecastTimeUtil#refKey} or {@link ForecastTimeUtil#fcstKey}
	 * then the list will be of size 1.
	 * 
	 * @param layer
	 * @param dimensions
	 * @return
	 * @throws WmsException
	 */
    public SortedSet<DataTime> getDataTimes(
            SimpleLayer<? extends SimpleDimension> layer,
            Map<String, String> dimensions) throws OgcException {
        return getDataTimes(layer, (String) null, dimensions);
    }

	/**
	 * Gets a list of datatimes for specified layer. If dimensions has values
	 * for {@link ForecastTimeUtil#refKey} or {@link ForecastTimeUtil#fcstKey}
	 * then the list will be of size 1. If time is null, a default time will be
	 * used.
	 * 
	 * @param layer
	 * @param time
	 * @param dimensions
	 * @return
	 * @throws WmsException
	 */
    public SortedSet<DataTime> getDataTimes(
            SimpleLayer<? extends SimpleDimension> layer, String time,
			Map<String, String> dimensions) throws OgcException {
		Calendar validTime = null;
		if (time != null) {
			validTime = getValidTime(layer, time);
		}
		return getDataTimes(layer, validTime, dimensions);
	}

    public SortedSet<DataTime> getDataTimes(
            SimpleLayer<? extends SimpleDimension> layer, Date time,
            Map<String, String> dimensions) throws OgcException {
        Calendar validTime = null;
        if (time != null) {
            validTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            validTime.setTime(time);
        }
        return getDataTimes(layer, validTime, dimensions);
    }

    protected SortedSet<DataTime> getDataTimes(
            SimpleLayer<? extends SimpleDimension> layer, Calendar time,
            Map<String, String> dimensions) throws OgcException {
        String refStr = dimensions.get(refKey);
        String fcstStr = dimensions.get(fcstKey);
		SortedSet<DataTime> rval = new TreeSet<DataTime>();
		byte state = getState(time, refStr, fcstStr);
		switch (state) {
		case NONE:
			time = getValidTime(layer, null);
		case VALID:
			rval = getCandidates(layer, time);
			break;
		case REF_FCST_VALID:
			rval.add(getDataTime(refStr, fcstStr, time));
			break;
		case FCST_VALID:
			rval.add(getDataTimeFcst(fcstStr, time));
			break;
		case REF_VALID:
			rval.add(getDataTimeRef(refStr, time));
			break;
		case REF:
			rval.add(getDataTimeRef(refStr, layer));
			break;
		case FCST:
			rval.add(getDataTimeFcst(fcstStr, layer));
			break;
		case REF_FCST:
			rval.add(getDataTime(refStr, fcstStr, null));
			break;
		}
		return rval;
	}

	/**
	 * @param fcstStr
	 * @param layer
	 * @return
	 * @throws OgcException
	 */
    protected DataTime getDataTimeFcst(String fcstStr,
            SimpleLayer<? extends SimpleDimension> layer)
			throws OgcException {
		int fcst = parseForcast(fcstStr);
        SimpleDimension refDim = layer.getDimension(refKey);
		Set<String> refTimes = refDim.getValues();
		SortedSet<Long> refEpochTimes = new TreeSet<Long>();
		for (String ref : refTimes) {
			Date d = parseTimeString(ref).getTime();
			refEpochTimes.add(d.getTime());
		}
		SortedSet<Date> validTimes = layer.getTimes();
		DataTime rval = null;
		for (Date valid : validTimes) {
			long ref = valid.getTime() - (fcst * 1000);
			Date refTime = new Date(ref);
			if (refEpochTimes.contains(refTime.getTime())) {
				rval = new DataTime(refTime, fcst);
				break;
			}
		}
		if (rval == null) {
			throw new OgcException(Code.InvalidDimensionValue,
					"No valid times match forecast offset: " + fcstStr);
		}
		return rval;
	}

	/**
	 * Extracts the specified dimension object from the layer object.
	 * 
	 * @param layer
	 * @param dimension
	 * @return
	 * @throws WmsException
	 */
    protected SimpleDimension getDimension(
            SimpleLayer<? extends SimpleDimension> layer, String dimension)
            throws OgcException {
        SimpleDimension dim;
		try {
            dim = layer.getDimension(dimension);
			if (dim == null) {
				// UNLIKELY: must have been the dimension
				log.error("layer: " + layer + " missing dimension: "
						+ dimension);
				throw new OgcException(Code.InternalServerError);
			}
			if (dim.getValues().isEmpty()) {
				log.error("layer " + layer + " missing " + dimension);
				throw new OgcException(Code.LayerNotDefined);
			}
			return dim;
		} catch (Exception e) {
			log.error("Problem getting dim: " + dimension + " for layer: "
					+ layer, e);
			throw new OgcException(Code.InternalServerError);
		}
	}

	/**
	 * Gets the refTime/forecast time pairs from the layer that could combine to
	 * the given valid time.
	 * 
	 * @param layer
	 * @param validTime
	 * @return
	 * @throws WmsException
	 */
    protected SortedSet<DataTime> getCandidates(
            SimpleLayer<? extends SimpleDimension> layer, Calendar validTime)
            throws OgcException {
		SimpleDimension refDim = getDimension(layer, refKey);
		SimpleDimension fcstDim = getDimension(layer, fcstKey);
		TreeSet<Double> fcsts = LayerTransformer.getDimValuesAsDouble(fcstDim);
		Set<String> refVals = refDim.getValues();
		ArrayList<Calendar> refs;
		try {
			refs = convert(refVals);
		} catch (Exception e) {
			log.error("invalid date in layer: " + layer, e);
			throw new OgcException(Code.InternalServerError);
		}
		SortedSet<DataTime> rval = new TreeSet<DataTime>();
		// try all combinations to find matches with valid time
		Calendar refPlusFcst = Calendar
				.getInstance(TimeZone.getTimeZone("GMT"));
		for (Double dbl : fcsts) {
			int fcst = (int) Math.floor(dbl);
			for (Calendar ref : refs) {
				long diff = ref.getTimeInMillis() + (fcst * 1000);
				refPlusFcst.setTimeInMillis(diff);
				if (equals(refPlusFcst, validTime)) {
					rval.add(new DataTime(ref, fcst));
				}
			}
		}
		return rval;
	}

	protected boolean equals(Calendar one, Calendar two) {
		return one.getTimeInMillis() == two.getTimeInMillis();
	}

	/**
	 * Parses time strings
	 * 
	 * @param times
	 * @return
	 * @throws WmsException
	 */
	protected ArrayList<Calendar> convert(Set<String> times)
			throws OgcException {
		ArrayList<Calendar> refs = new ArrayList<Calendar>(times.size());
		for (String val : times) {
			refs.add(parseTimeString(val));
		}
		return refs;
	}

	protected static Calendar parseTimeString(String time) throws OgcException {
		try {
			return DatatypeConverter.parseDateTime(time);
		} catch (Exception e) {
			throw new OgcException(Code.InvalidFormat, "Invalid Date Format");
		}
	}

	/**
	 * Creates datatime using valid time and refTime
	 * 
	 * @param refStr
	 * @param validTime
	 * @return
	 * @throws WmsException
	 */
	protected DataTime getDataTimeRef(String refStr, Calendar validTime)
			throws OgcException {
		Calendar ref = parseTimeStr(refStr, refKey);
		long diff = validTime.getTimeInMillis() - ref.getTimeInMillis();
		return new DataTime(ref, (int) (diff / 1000));
	}

	/**
	 * Creates datatime using only refTime
	 * 
	 * @param refStr
	 * @return valid datatime with lowest forecast offset
	 * @throws WmsException
	 */
    protected DataTime getDataTimeRef(String refStr,
            SimpleLayer<? extends SimpleDimension> layer) throws OgcException {
		Calendar ref = parseTimeStr(refStr, refKey);
        SimpleDimension fcstDim = layer.getDimension(fcstKey);
		TreeSet<Double> offsets = LayerTransformer.getDimValuesAsDouble(fcstDim);
		SortedSet<Date> validTimes = layer.getTimes();
		DataTime rval = null;
		for ( Double offset : offsets){
			int fcstTime = ( offset.intValue() * 1000);
			long refPlusOffset = ref.getTimeInMillis() + fcstTime ;
			Date d = new Date(refPlusOffset);
			if ( validTimes.contains(d)){
				rval = new DataTime(ref, offset.intValue());
				break;
			}
		}
		if (rval == null) {
			throw new OgcException(Code.InvalidDimensionValue,
					"No valid times match refTime: " + ref);
		}
		return rval;
	}

	/**
	 * Parses time string with error handling.
	 * 
	 * @param time
	 * @param name
	 *            used in error output
	 * @return
	 * @throws WmsException
	 */
	protected Calendar parseTimeStr(String time, String name)
			throws OgcException {
		Calendar rval;
		try {
			rval = parseTimeString(time);
		} catch (Exception e) {
			String msg = String.format("Invalid dimension %s: %s", name, time);
			throw new OgcException(Code.InvalidDimensionValue, msg);
		}
		return rval;
	}

	/**
	 * Creates datatime using forecast time and valid time
	 * 
	 * @param fcstStr
	 * @param validTime
	 * @return
	 * @throws WmsException
	 */
	protected DataTime getDataTimeFcst(String fcstStr, Calendar validTime)
			throws OgcException {
		int fcst = parseForcast(fcstStr);
		long refMillis = validTime.getTimeInMillis() - (fcst * 1000);
		Calendar refTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		refTime.setTimeInMillis(refMillis);
		return new DataTime(refTime, fcst);
	}

	/**
	 * Parses forecast string using limited ISO standard for time duration. This
	 * method handles integers with duration labels S|M|H|D (seconds, minutes,
	 * hours and days). It does not handle combined duration specifiers.
	 * 
	 * @param fcstStr
	 * @return
	 * @throws WmsException
	 */
	protected int parseForcast(String fcstStr) throws OgcException {
		// TODO handle all ISO standard durations
		Matcher m = fcstPattern.matcher(fcstStr);
		if (m.matches()) {
			int fcst = Integer.parseInt(m.group(1));
			String unit = m.group(2);
			if (unit != null && !unit.equalsIgnoreCase("s")) {
				if (unit.equalsIgnoreCase("m")) {
					fcst = fcst * 60;
				} else if (unit.equalsIgnoreCase("h")) {
					fcst = fcst * 60 * 60;
				} else if (unit.equalsIgnoreCase("d")) {
					fcst = fcst * 60 * 60 * 24;
				}
			}
			return fcst;
		} else {
			String msg = String.format("Invalid dimension %s: %s", fcstKey,
					fcstStr);
			throw new OgcException(Code.InvalidDimensionValue, msg);
		}
	}

	/**
	 * Construct datatime using refTime and forecast time.
	 * 
	 * @param refStr
	 * @param fcstStr
	 * @param validTime
	 *            used to confirm result
	 * @return
	 * @throws WmsException
	 */
	protected DataTime getDataTime(String refStr, String fcstStr,
			Calendar validTime) throws OgcException {
		Calendar ref = parseTimeStr(refStr, refKey);
		int fcst = parseForcast(fcstStr);
		if (validTime != null) {
			long diff = validTime.getTimeInMillis() - ref.getTimeInMillis();
			if (diff != (fcst * 1000)) {
				String msg = String.format("%s and time must differ by %s",
						refKey, fcstKey);
				throw new OgcException(Code.InvalidDimensionValue, msg);
			}
		}
		return new DataTime(ref, fcst);
	}

	/**
	 * If time is not null, parses time. Else, latest time is extracted from
	 * layer.
	 * 
	 * @param layer
	 * @param time
	 * @return
	 * @throws WmsException
	 */
    protected Calendar getValidTime(
            SimpleLayer<? extends SimpleDimension> layer, String time)
            throws OgcException {
		Calendar rval;
		if (time == null) {
			try {
				Date latestTime = LayerTransformer.getLatestTime(layer);
				if (latestTime == null) {
					throw new OgcException(Code.LayerNotDefined,
							"Unable to find layer: " + layer);
				}
				rval = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
				rval.setTime(latestTime);
			} catch (Exception e) {
				log.error("Problem getting layer", e);
				throw new OgcException(Code.InternalServerError);
			}
		} else {
			rval = parseTimeStr(time, "time");
		}
		return rval;
	}

	protected static byte getState(Calendar validTime, String refTime,
			String fcstOffset) {
		byte rval = 0;
		if (refTime != null) {
			rval |= 0x04;
		}
		if (fcstOffset != null) {
			rval |= 0x02;
		}
		if (validTime != null) {
			rval |= 0x01;
		}
		return rval;
	}
}
