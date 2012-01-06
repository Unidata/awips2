package gov.noaa.nws.ncep.edex.plugin.ncccfp.decoder;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.io.WKTReader;

import gov.noaa.nws.ncep.edex.plugin.ncccfp.decoder.NcccfpSeparator;
import gov.noaa.nws.ncep.common.dataplugin.ncccfp.NcccfpLocation;
import gov.noaa.nws.ncep.common.dataplugin.ncccfp.NcccfpRecord;

/**
 * 
 * NCCCFP Decoder
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/05/2009	155			F. J. Yen	From Raytheon's CCFP.  Fix AREA.  Store 1 instead of 14-15 decimal
 * 										places for lat/lon values and limit to 7 points in dataURI (length 255)
 * 										to conserve space and to fit into field.  Add number of points to dataURI.
 * 										Modify for LINE to be decoded.  Make decode synchronized.
 * 05/27/2010	155			F. J. Yen	Refactored for dataplugin for migration to to11dr11
 * 
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1
 */

public class NcccfpDecoder extends AbstractDecoder {
	private static String pluginName;

	private static final String PLUGIN_NAME = "ncccfp";

	/** Record used for returning values */
	NcccfpRecord record;

	/** Pattern object for regex search */
	Pattern pattern;

	/** Regex matcher */
	private Matcher matcher;

	/** Match the product returned from separator */
	private static final String PARSE_STRING = "[A-Z]{4}[0-9]{1,2} [A-Z]{4} [0-9]{6}(?: [A-Z]{3})?\n"
			+ "CFP[\\p{Alnum} ]{3}\n" // awips header
			+ "CCFP (\\d{4})(\\d{2})(\\d{2})_(\\d{2})\\d{2} (\\d{4})(\\d{2})(\\d{2})_(\\d{2})\\d{2}\n" // start/valid
			// times
			+ "(AREA|LINE).*\n" // SKIP AREA and LINE parsing
			+ "(CANADA ON|CANADA OFF)";

	/** Parse an AREA line */
	private static final String PARSE_AREA = "AREA (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (.*) (\\d+) (\\d+)";

	/** Parse a LINE line */
	private static final String PARSE_LINE = "LINE (\\d+) (.*)";

	/**
	 * Constructor
	 * 
	 * @throws DecoderException
	 */
	public NcccfpDecoder() throws DecoderException {
	}

    public NcccfpDecoder(String name) throws DecoderException {
    	pluginName = name;
    }

	public synchronized PluginDataObject[] decode(String msg)
			throws DecoderException, PluginException {
		record = new NcccfpRecord();
		record.setMessageData(msg);
		NcccfpLocation location = new NcccfpLocation();

		pattern = Pattern.compile(PARSE_STRING);
		matcher = pattern.matcher(msg);

		try {
			WKTReader wktReader = new WKTReader();
			if (matcher.find()) {
				Calendar start = TimeTools.getBaseCalendar(Integer
						.parseInt(matcher.group(1)), Integer.parseInt(matcher
						.group(2)), Integer.parseInt(matcher.group(3)));
				start.set(Calendar.HOUR_OF_DAY, Integer.parseInt(matcher
						.group(4)));
				record.setIssuetime(start);
				Calendar valid = TimeTools.getBaseCalendar(Integer
						.parseInt(matcher.group(5)), Integer.parseInt(matcher
						.group(6)), Integer.parseInt(matcher.group(7)));
				valid.set(Calendar.HOUR_OF_DAY, Integer.parseInt(matcher
						.group(8)));
				record.setValidtime(valid);
				TimeRange range = new TimeRange(record.getIssuetime(), record
						.getValidtime());
				record.setDataTime(new DataTime(record.getIssuetime(), range));
				record.setProducttype(matcher.group(9));
				if (matcher.group(10).equals("CANADA ON")) {
					record.setCanadaflag(Boolean.TRUE);
				} else {
					record.setCanadaflag(Boolean.FALSE);
				}
				record.setInsertTime(TimeTools.getSystemCalendar());
			}
			if (record.getProducttype().equals("AREA")) {
				pattern = Pattern.compile(PARSE_AREA);
				matcher = pattern.matcher(msg);
				if (matcher.find()) {
					record.setCoverage(Integer.parseInt(matcher.group(1)));
					record.setConf(Integer.parseInt(matcher.group(2)));
					record.setGrowth(Integer.parseInt(matcher.group(3)));
					record.setTops(Integer.parseInt(matcher.group(4)));
					record.setSpeed(Integer.parseInt(matcher.group(5)));
					record.setDirection(Integer.parseInt(matcher.group(6)));
					record.setNumPts(Integer.parseInt(matcher.group(7)));
					location
							.setBoxLat(Double.parseDouble(matcher.group(9)) * 0.1);
					location.setBoxLong(Double.parseDouble(matcher.group(10))
							* -0.1);
					String templatlonpairs = matcher.group(8);
					pattern = Pattern.compile("(\\d+) (\\d+)");
					matcher = pattern.matcher(templatlonpairs);
					StringBuffer wtk = new StringBuffer();
					wtk.append("POLYGON((");
					StringBuffer wtka = new StringBuffer();
					wtka.append("POLYGON((");
					int i7CntPts = 0;
					StringBuffer wtk7 = new StringBuffer();
					wtk7.append("POLYGONU((");

					if (matcher.find()) {
						wtk.append(Double.toString(Integer.parseInt(matcher
								.group(1)) * 0.1)
								+ " ");
						wtk.append(Double.toString(Integer.parseInt(matcher
								.group(2))
								* -0.1));
						/*
						 * Reduce from 14-15 decimal places to 1 for dataURI
						 */
						int len = matcher.group(1).length();
						String lat1deg = matcher.group(1).substring(0, len - 1);
						String lat1tenth = matcher.group(1).substring(len - 1);
						len = matcher.group(2).length();
						String lon1deg = matcher.group(2).substring(0, len - 1);
						if (!matcher.group(2).substring(0, 0).equals("-")) {
							lon1deg = "-" + lon1deg;
						}
						String lon1tenth = matcher.group(2).substring(len - 1);
						i7CntPts++;
						wtk7.append(lat1deg + "." + lat1tenth + " " + lon1deg
								+ "." + lon1tenth);
						wtka.append(lat1deg + "." + lat1tenth + " " + lon1deg
								+ "." + lon1tenth);
					}
					while (matcher.find()) {
						wtk.append(", "
								+ Double.toString(Integer.parseInt(matcher
										.group(1)) * 0.1) + " ");
						wtk.append(Double.toString(Integer.parseInt(matcher
								.group(2))
								* -0.1));
						/*
						 * Reduce from 14-15 decimal places to 1 for dataURI
						 */
						int len = matcher.group(1).length();
						String latdeg = matcher.group(1).substring(0, len - 1);
						String lat10 = matcher.group(1).substring(len - 1);
						len = matcher.group(2).length();
						String londeg = matcher.group(2).substring(0, len - 1);
						if (!matcher.group(2).substring(0, 0).equals("-")) {
							londeg = "-" + londeg;
						}
						String lon10 = matcher.group(2).substring(len - 1);
						if (i7CntPts < 7) {
							/*
							 * Truncate the number of coordinates to 7 for
							 * dataURI
							 */
							i7CntPts++;
							wtk7.append("," + latdeg + "." + lat10 + " "
									+ londeg + "." + lon10);
						}
						wtka.append("," + latdeg + "." + lat10 + " " + londeg
								+ "." + lon10);
					}
					wtk.append("))");
					wtk7.append("))");
					wtka.append("))");
					location.setGeometry((Polygon) wktReader.read(wtk
							.toString()));
					record.setLocation(location);
					/*
					 * Column LocationAll contains all the coordinates with 1
					 * decimal place. Column locationUri is locationAll
					 * truncated to 7 coordinates (or less).
					 */
					record.setLocationUri(wtk7.toString());
					location.setLocationAll(wtka.toString());
				}
			} else if (record.getProducttype().equals("LINE")) {
				pattern = Pattern.compile(PARSE_LINE);
				matcher = pattern.matcher(msg);
				if (matcher.find()) {
					record.setCoverage(null);
					record.setConf(null);
					record.setGrowth(null);
					record.setTops(null);
					record.setSpeed(null);
					record.setDirection(null);
					location.setBoxLat(0);
					location.setBoxLong(0);
					record.setNumPts(Integer.parseInt(matcher.group(1)));
					// String templatlonpairs = matcher.group(1);
					String templatlonpairs = matcher.group(2);
					pattern = Pattern.compile("(\\d+) (\\d+)");
					matcher = pattern.matcher(templatlonpairs);

					StringBuffer wtk = new StringBuffer();
					wtk.append("LINESTRING(");
					StringBuffer wtka = new StringBuffer();
					wtka.append("LINESTRING(");
					int i7CntPts = 0;
					StringBuffer wtk7 = new StringBuffer();
					wtk7.append("LINESTRINGU(");
					if (matcher.find()) {
						wtk.append(Double.toString(Integer.parseInt(matcher
								.group(1)) * 0.1)
								+ " ");
						wtk.append(Double.toString(Integer.parseInt(matcher
								.group(2))
								* -0.1));
						/*
						 * Reduce from 14-15 decimal places to 1 for dataURI
						 */
						int len = matcher.group(1).length();
						String lat1deg = matcher.group(1).substring(0, len - 1);
						String lat1tenth = matcher.group(1).substring(len - 1);
						len = matcher.group(2).length();
						String lon1deg = matcher.group(2).substring(0, len - 1);
						if (!matcher.group(2).substring(0, 0).equals("-")) {
							lon1deg = "-" + lon1deg;
						}
						String lon1tenth = matcher.group(2).substring(len - 1);
						i7CntPts++;
						wtk7.append(lat1deg + "." + lat1tenth + " " + lon1deg
								+ "." + lon1tenth);
						wtka.append(lat1deg + "." + lat1tenth + " " + lon1deg
								+ "." + lon1tenth);
					}
					while (matcher.find()) {
						wtk.append(", "
								+ Double.toString(Integer.parseInt(matcher
										.group(1)) * 0.1) + " ");
						wtk.append(Double.toString(Integer.parseInt(matcher
								.group(2))
								* -0.1));
						/*
						 * Reduce from 14-15 decimal places to 1 for dataURI
						 */
						int len = matcher.group(1).length();
						String latdeg = matcher.group(1).substring(0, len - 1);
						String lat10 = matcher.group(1).substring(len - 1);
						len = matcher.group(2).length();
						String londeg = matcher.group(2).substring(0, len - 1);
						if (!matcher.group(2).substring(0, 0).equals("-")) {
							londeg = "-" + londeg;
						}
						String lon10 = matcher.group(2).substring(len - 1);
						/*
						 * Truncate the number of coordinates to 7 for dataURI
						 */
						if (i7CntPts < 7) {
							i7CntPts++;
							wtk7.append("," + latdeg + "." + lat10 + " "
									+ londeg + "." + lon10);
						}
						wtka.append("," + latdeg + "." + lat10 + " " + londeg
								+ "." + lon10);
					}
					wtk.append(")");
					wtk7.append(")");
					wtka.append(")");
					location.setGeometry((LineString) wktReader.read(wtk
							.toString()));
					record.setLocation(location);
					/*
					 * Column locationAll contains all the coordinates with 1
					 * decimal place. Column locationUri is locationAll
					 * truncated to 7 coordinates (or less).
					 */
					record.setLocationUri(wtk7.toString());
					location.setLocationAll(wtka.toString());
				}
			}
		} catch (Exception e) {
			throw new DecoderException("Unable to decode NCCCFP", e);
		}
		if (record != null) {
			record.setPluginName(PLUGIN_NAME);
			record.constructDataURI();
			return new PluginDataObject[] { record };
		} else {
			return new PluginDataObject[0];
		}

	}
}
