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
package com.raytheon.uf.edex.plugin.loctables.util.store;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBWriter;
import com.vividsolutions.jts.io.WKTReader;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2010            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ObStationRow {

	// INSERT INTO common_obs_spatial (
	// gid, country, elevation, icao, the_geom, name, rbsnindicator, state,
	// upperairelevation, upperairgeom, wmoindex, wmoregion
	// ) VALUES (
	//
	public static final String LEAD_IN = "insert into common_obs_spatial (gid,catalogType,stationId,icao,wmoIndex,wmoRegion,country,state,pressureLevel,aerodromeFlag,rbsnIndicator,elevation,the_geom,upperAirElevation,upperairgeom,name) values (";

	public static final String CLOSE = ");";

	public static final String NULL = "NULL";

	private static WKTReader wktReader = new WKTReader();

	private static WKBWriter wkbWriter = new WKBWriter();

	private String gid;

	private String icao;

	private Integer wmoIndex;

	private String stationId;

	private Integer catalogType;

	private String rbsnIndicator;

	private String name;

	private String country;

	private String state;

	private Integer wmoRegion;

	// Column(length = 16)
	private String pressureLevel;

	// Column(length = 1)
	private String aerodromeFlag;

	// Surface observing location elevation
	private Integer elevation;

	// Surface observing location latitude/longitude
	private Point location;

	// Upperair observing location elevation
	private Integer upperAirElevation;

	// Upperair observing location latitude/longitude
	private Point upperAirGeometry;

	/**
     * 
     */
	public ObStationRow() {

	}

	/**
	 * 
	 * @param catType
	 */
	public ObStationRow(Integer catType) {
		catalogType = catType;
	}

	/**
	 * @return the icao
	 */
	public String getIcao() {
		return icao;
	}

	/**
	 * @param icao
	 *            the icao to set
	 */
	public void setIcao(String icao) {
		this.icao = icao;
	}

	/**
	 * @return the wmoIndex
	 */
	public Integer getWmoIndex() {
		return wmoIndex;
	}

	/**
	 * @param wmoIndex
	 *            the wmoIndex to set
	 */
	public void setWmoIndex(Integer wmoIndex) {
		this.wmoIndex = wmoIndex;
	}

	/**
	 * @return the stationId
	 */
	public String getStationId() {
		return stationId;
	}

	/**
	 * @param stationId
	 *            the stationId to set
	 */
	public void setStationId(String stationId) {
		this.stationId = stationId;
	}

	/**
	 * @return the catalogType
	 */
	public Integer getCatalogType() {
		return catalogType;
	}

	/**
	 * @param catalogType
	 *            the catalogType to set
	 */
	public void setCatalogType(Integer catalogType) {
		this.catalogType = catalogType;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		if (name != null) {
			StringBuilder sb = new StringBuilder(name);
			for (int i = 0; i < sb.length(); i++) {
				switch (sb.charAt(i)) {
				case '\'': {
					sb.setCharAt(i, ' ');
					break;
				}
				case '\\': {
					sb.setCharAt(i, '/');
					break;
				}
				case ';': {
					// This mod is required because the CoreDAO script
					// runner splits on semicolons which breaks otherwise
					// legal SQL.
					sb.setCharAt(i, ':');
					break;
				}
				}
			}
			name = sb.toString();
		}
		this.name = name;
	}

	/**
	 * @return the country
	 */
	public String getCountry() {
		return country;
	}

	/**
	 * @param country
	 *            the country to set
	 */
	public void setCountry(String country) {
		this.country = country;
	}

	/**
	 * @return the state
	 */
	public String getState() {
		return state;
	}

	/**
	 * @param state
	 *            the state to set
	 */
	public void setState(String state) {
		this.state = state;
	}

	/**
	 * @return the wmoRegion
	 */
	public Integer getWmoRegion() {
		return wmoRegion;
	}

	/**
	 * @param wmoRegion
	 *            the wmoRegion to set
	 */
	public void setWmoRegion(Integer wmoRegion) {
		this.wmoRegion = wmoRegion;
	}

	/**
	 * @return the elevation
	 */
	public Integer getElevation() {
		return elevation;
	}

	/**
	 * @param elevation
	 *            the elevation to set
	 */
	public void setElevation(Integer elevation) {
		this.elevation = elevation;
	}

	/**
	 * @return the upperAirElevation
	 */
	public Integer getUpperAirElevation() {
		return upperAirElevation;
	}

	/**
	 * @param upperAirElevation
	 *            the upperAirElevation to set
	 */
	public void setUpperAirElevation(Integer upperAirElevation) {
		this.upperAirElevation = upperAirElevation;
	}

	/**
	 * @return the rbsnIndicator
	 */
	public String getRbsnIndicator() {
		return rbsnIndicator;
	}

	/**
	 * @param rbsnIndicator
	 *            the rbsnIndicator to set
	 */
	public void setRbsnIndicator(String rbsnIndicator) {
		this.rbsnIndicator = rbsnIndicator;
	}

	/**
	 * @return the upperAirGeometry
	 */
	public Point getUpperAirGeometry() {
		return upperAirGeometry;
	}

	/**
	 * @param upperAirGeometry
	 *            the upperAirGeometry to set
	 */
	public void setUpperAirGeometry(Point upperAirGeometry) {
		this.upperAirGeometry = upperAirGeometry;
	}

	/**
	 * @return the location
	 */
	public Point getLocation() {
		return location;
	}

	/**
	 * @param location
	 *            the location to set
	 */
	public void setLocation(Point location) {
		this.location = location;
	}

	/**
	 * @return the gid
	 */
	public String getGid() {
		return ObStation.createGID(catalogType, stationId);
	}

	/**
	 * @return the pressureLevel
	 */
	public String getPressureLevel() {
		return pressureLevel;
	}

	/**
	 * @param pressureLevel
	 *            the pressureLevel to set
	 */
	public void setPressureLevel(String pressureLevel) {
		this.pressureLevel = pressureLevel;
	}

	/**
	 * @return the aerodromeFlag
	 */
	public String getAerodromeFlag() {
		return aerodromeFlag;
	}

	/**
	 * @param aerodromeFlag
	 *            the aerodromeFlag to set
	 */
	public void setAerodromeFlag(String aerodromeFlag) {
		this.aerodromeFlag = aerodromeFlag;
	}

	/**
	 * 
	 * @param value
	 * @param format
	 * @return
	 */
	private String fromInt(Integer value, String format) {
		return (value != null) ? String.format(format, value) : NULL;
	}

	/**
	 * 
	 * @param value
	 * @param format
	 * @return
	 */
	private String fromString(String value, String format) {

		StringBuilder builder = null;
		if (value != null) {
			builder = new StringBuilder("'");
			builder.append(String.format(format, value));
			builder.append("'");
		} else {
			builder = new StringBuilder(NULL);
		}
		return builder.toString();
	}

	/**
	 * 
	 * @param geo
	 * @return
	 */
	private String fromGeometry(Point geo) {
		String geometry = null;
		if (geo != null) {
			StringBuilder builder = new StringBuilder("'");
			builder.append(WKBWriter.bytesToHex(wkbWriter.write(geo)));
			builder.append("'");
			geometry = builder.toString();
		} else {
			geometry = NULL;
		}
		return geometry;
	}

	public static Point getPoint(double lat, double lon) {
		Point geometry = null;
		try {
			geometry = (Point) wktReader
					.read("POINT (" + lon + " " + lat + ")");
		} catch (ParseException pe) {
			pe.printStackTrace();
		}
		return geometry;
	}

	/**
	 * 
	 * @return
	 */
	public ObStation toObStation() {
		ObStation station = new ObStation();
		// gid
		station.setGid(getGid());
		// catalogType
		station.setCatalogType(getCatalogType());
		// stationId
		station.setStationId(getStationId());
		// icao
		station.setIcao(getIcao());
		// wmoIndex
		station.setWmoIndex(getWmoIndex());
		// wmoRegion
		station.setWmoRegion(getWmoRegion());
		// country
		station.setCountry(getCountry());
		// state
		station.setState(getState());
		// pressureLevel
		station.setPressureLevel(getPressureLevel());
		// aerodromeFlag
		station.setAerodromeFlag(getAerodromeFlag());
		// rbsnIndicator
		station.setRbsnIndicator(getRbsnIndicator());
		// elevation
		station.setElevation(getElevation());
		// the_geom
		station.setLocation(getLocation());
		// upperAirElevation
		station.setUpperAirElevation(getUpperAirElevation());
		// upperairgeom
		station.setUpperAirGeometry(getUpperAirGeometry());
		// name
		station.setName(getName());
		return station;
	}

	/**
	 * 
	 * @return
	 */
	public String toSQLInsertString() {

		StringBuilder builder = new StringBuilder(LEAD_IN);

		// gid,catalogType,stationId,icao,wmoIndex,wmoRegion,country,state,pressureLevel,aerodromeFlag,rbsnIndicator,elevation,the_geom,upperAirElevation,upperairgeom,name

		// gid
		builder.append(fromString(getGid(), "%s"));
		builder.append(",");
		// catalogtype
		builder.append(fromInt(getCatalogType(), "%d"));
		builder.append(",");
		// stationid
		builder.append(fromString(getStationId(), "%s"));
		builder.append(",");
		// icao
		builder.append(fromString(getIcao(), "%s"));
		builder.append(",");
		// wmoindex
		builder.append(fromInt(getWmoIndex(), "%d"));
		builder.append(",");
		// wmoregion
		builder.append(fromInt(getWmoRegion(), "%d"));
		builder.append(",");
		// country
		builder.append(fromString(getCountry(), "%s"));
		builder.append(",");
		// state
		builder.append(fromString(getState(), "%s"));
		builder.append(",");
		// pressureLevel
		builder.append(fromString(getPressureLevel(), "%s"));
		builder.append(",");
		// aerodromeFlag
		String s = getAerodromeFlag();
		builder.append(("A".equals(s)) ? "'A'" : "' '");
		builder.append(",");
		// rbsnindicator
		s = getAerodromeFlag();
		builder.append(("P".equals(s)) ? "'P'" : "' '");
		builder.append(",");

		// elevation
		builder.append(fromInt(getElevation(), "%d"));
		builder.append(",");
		// the_geom
		builder.append(fromGeometry(getLocation()));
		builder.append(",");
		// upperairelevation
		builder.append(fromInt(getUpperAirElevation(), "%d"));
		builder.append(",");

		// upperairgeom
		builder.append(fromGeometry(getUpperAirGeometry()));
		builder.append(",");
		// name
		builder.append(fromString(getName(), "%s"));

		builder.append(CLOSE);

		return builder.toString();
	}

	/**
	 * Determine if a given ObStation instance needs to be updated from a second
	 * ObStation instance.
	 * 
	 * @param a
	 *            Target ObStation instance to be updated.
	 * @param b
	 *            ObStation instance that may contain changes.
	 * @return Does the ObStation target instance need to be updated.
	 */
	public static boolean requiresUpdate(ObStation a, ObStation b) {
		boolean newStation = false;

		if (copyItem(a.getName(), b.getName())) {
			a.setName(b.getName());
			newStation = true;
		}

		if (copyItem(a.getCountry(), b.getCountry())) {
			a.setCountry(b.getCountry());
			newStation = true;
		}

		if (copyItem(a.getState(), b.getState())) {
			a.setState(b.getState());
			newStation = true;
		}

		if (copyItem(a.getWmoRegion(), b.getWmoRegion())) {
			a.setWmoRegion(b.getWmoRegion());
			newStation = true;
		}

		if (copyItem(a.getPressureLevel(), b.getPressureLevel())) {
			a.setPressureLevel(b.getPressureLevel());
			newStation = true;
		}

		if (copyItem(a.getAerodromeFlag(), b.getAerodromeFlag())) {
			a.setAerodromeFlag(b.getAerodromeFlag());
			newStation = true;
		}

		if (copyItem(a.getElevation(), b.getElevation())) {
			a.setElevation(b.getElevation());
			newStation = true;
		}

		if (copyItem(a.getUpperAirElevation(), b.getUpperAirElevation())) {
			a.setUpperAirElevation(b.getUpperAirElevation());
			newStation = true;
		}

		if (copyItem(a.getRbsnIndicator(), b.getRbsnIndicator())) {
			a.setRbsnIndicator(b.getRbsnIndicator());
			newStation = true;
		}

		if (copyItem(a.getUpperAirGeometry(), b.getUpperAirGeometry())) {
			a.setUpperAirGeometry(b.getUpperAirGeometry());
			newStation = true;
		}

		if (copyItem(a.getLocation(), b.getLocation())) {
			a.setLocation(b.getLocation());
			newStation = true;
		}

		return newStation;
	}

	private static boolean copyItem(Object a, Object b) {
		boolean copy = false;
		if ((a == null)) {
			copy = (b != null);
		} else {
			if (b != null) {
				if ((a instanceof Point) && (b instanceof Point)) {
					Point aa = (Point) a;
					Point bb = (Point) b;

					copy = (aa.getX() != bb.getX()) || (aa.getY() != bb.getY());
				} else {
					copy = (!a.equals(b));
				}
			}
		}
		return copy;
	}

	/**
	 * 
	 * @param args
	 */
	public static void main(String[] args) {

		ObStationRow row = new ObStationRow();
		row.setCatalogType(ObStation.CAT_TYPE_ACFT_PIREP);
		row.setStationId("KOMA");
		row.setIcao("KOMA");
		row.setElevation(390);
		row.setCountry("US");
		row.setState("NE");
		row.setName("Omaha, NE");
		row.setLocation(getPoint(45.2, -103.25));
		row.setWmoIndex(72553);

		row.setUpperAirElevation(391);
		row.setUpperAirGeometry(getPoint(45.5, -103.3));
		row.setRbsnIndicator("Y");
		row.setAerodromeFlag("A");

		System.out.println(row.toSQLInsertString());

	}

}
