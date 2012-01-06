//
// AddeImageURL.java
//

/*
This source file is part of the edu.wisc.ssec.mcidas package and is
Copyright (C) 1998 - 2009 by Tom Whittaker, Tommy Jasmin, Tom Rink,
Don Murray, James Kelly, Bill Hibbard, Dave Glowacki, Curtis Rueden
and others.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA
*/

package edu.wisc.ssec.mcidas.adde;


import java.util.Date;

import edu.wisc.ssec.mcidas.McIDASUtil;


/**
 * A class for holding the ADDE URL for an image directory or data request.
 *
 * <pre>
 * URLs must all have the following format:
 *
 *   adde://host/request?keyword_1=value_1&amp;keyword_2=value_2
 *
 * where request can be one of the following:
 *
 *   imagedata - request for data in AreaFile format (AGET)
 *   imagedirectory - request for image directory information (ADIR)
 *
 * There can be any valid combination of the following supported keywords:
 *
 *   group=&lt;groupname&gt;         ADDE group name
 *   descr=&lt;descriptor&gt;        ADDE descriptor name
 *   band=&lt;band&gt;               spectral band or channel number
 *   mag=&lt;lmag&gt; &lt;emag&gt;         image magnification, postitive for blowup,
 *                               negative for blowdown (default = 1, emag=lmag)
 *                               (imagedata only)
 *   latlon=&lt;lat&gt; &lt;lon&gt;        lat/lon point to center image on (imagedata only)
 *   linele=&lt;lin&gt; &lt;ele&gt; &lt;type&gt; line/element to center image on (imagedata only)
 *   place=&lt;placement&gt;         placement of lat/lon or linele points (center
 *                               or upperleft (def=center)) (imagedata only)
 *   pos=&lt;position&gt;            request an absolute or relative ADDE position
 *                               number.  May use &lt;start&gt; &lt;end&gt;  Default
 *                               for &lt;end&gt; is 0 if start&lt;0, or =start otherwise.
 *   size=&lt;lines&gt; &lt;elements&gt;   size of image to be returned (imagedata only)
 *   unit=&lt;unit&gt;               to specify calibration units other than the
 *                               default
 *   spac=&lt;bytes&gt;              number of bytes per data point, 1, 2, or 4
 *                               (imagedata only)
 *   doc=&lt;yes/no&gt;              specify yes to include line documentation
 *                               with image (def=no)
 *   nav=&lt;lalo&gt;                include the lat-lon navigation info and not the O&A.
 *   aux=&lt;yes/no&gt;              specify yes to include auxilliary information
 *                               with image
 *   time=&lt;time1&gt; &lt;time2&gt;      specify the time range of images to select
 *                               (def=latest image if pos not specified)
 *   day=&lt;day&gt;                 specify the day of the images to select
 *                               (def=latest image if pos not specified)
 *   cal=&lt;cal type&gt;            request a specific calibration on the image
 *                               (imagedata only)
 *   id=&lt;stn id&gt;               radar station id
 *   user=&lt;user_id&gt;            ADDE user identification
 *   proj=&lt;proj #&gt;             a valid ADDE project number
 *   trace=&lt;0/1&gt;               setting to 1 tells server to write debug
 *                               trace file
 *   version=                  ADDE version number, currently 1 except for
 *                             griddata requests
 *   debug=                    set to true to watch the printlns stream by
 *   compress=                 set to "gzip" if you want to use the GZIP
 *                             compression or "compress" if you want to use
 *                             transfers in Unix compress format (You need to
 *                             have the VisAD package if you want to support
 *                             this.)  default = none.
 *   port=                     Socket port to connect on.  Overridden by
 *                             a port specified in the host
 *                             (e.g., adde.ucar.edu:500)
 * </pre>
 *
 */
public class AddeImageURL extends AddeDatasetURL {

  /** Keyword for band */
  public static final String KEY_BAND = "BAND";

  /** Keyword for position */
  public static final String KEY_POS = "POS";

  /** Keyword for station id */
  public static final String KEY_ID = "ID";

  /** Keyword for lat/lon request */
  public static final String KEY_LATLON = "LATLON";

  /** Keyword for lin/ele request */
  public static final String KEY_LINEELE = "LINELE";

  /** Keyword for location */
  public static final String KEY_LOC = "LOC";

  /** Keyword for mag */
  public static final String KEY_MAG = "MAG";

  /** Keyword for number of items */
  public static final String KEY_NUM = "NUM";

  /** Keyword for place */
  public static final String KEY_PLACE = "PLACE";

  /** Keyword for size */
  public static final String KEY_SIZE = "SIZE";

  /** Keyword for spacing */
  public static final String KEY_SPAC = "SPAC";

  /** Keyword for calibration unit */
  public static final String KEY_UNIT = "UNIT";

  /** Keyword for navigation type */
  public static final String KEY_NAV = "NAV";

  /** Keyword for aux request */
  public static final String KEY_AUX = "AUX";

  /** Keyword for doc request */
  public static final String KEY_DOC = "DOC";

  /** Keyword for day request */
  public static final String KEY_DAY = "DAY";

  /** Keyword for time request */
  public static final String KEY_TIME = "TIME";

  /** number of lines */
  private int lines;

  /** number of elements */
  private int elements;

  /** element magnification */
  private int emag = 1;

  /** line magnification */
  private int lmag = 1;

  /** default placement value (CENTER, ULEFT) */
  private static String DEFAULT_PLACE_VALUE = "ULEFT";

  /** default key for location (LATLON, LINELE) */
  private static String DEFAULT_LOCATE_KEY = "LINELE";

  /** default value for location */
  private static String DEFAULT_LOCATE_VALUE = "0 0";

  /** default key for location (LATLON, LINELE) */
  private String locateKey = DEFAULT_LOCATE_KEY;

  /** placement value (CENTER, ULEFT) */
  private String placeValue = DEFAULT_PLACE_VALUE;

  /** value for location */
  private String locateValue = DEFAULT_LOCATE_VALUE;

  /** band */
  private String band = ALL;

  /** band */
  private String unit = DEFAULT_VALUE;

  /** nav type */
  private String navType = DEFAULT_VALUE;

  /** aux value */
  private String auxValue = YES;

  /** doc value */
  private String docValue = DEFAULT_VALUE;

  /** spacing value */
  private int spacing = -1;

  /** station id */
  private String locationId = null;

  /** dataset position */
  private int pos = 0;

  /** start time */
  private Date startDate = null;

  /** end time */
  private Date endDate = null;

  /** time coordinate */
  private String timeCoverage = "I";

  /** no arg constructor */
  public AddeImageURL() {}

  /**
   * Create an AddeImageURL.
   *
   * @param host host to send to
   * @param requestType   type of request (REQ_IMAGEDATA, REQ_IMAGEDIR)
   * @param group   ADDE group
   * @param descriptor   ADDE descriptor
   */
  public AddeImageURL(String host, String requestType, String group,
                      String descriptor) {
    this(host, requestType, group, descriptor, null);
  }

  /**
   * Create an ADDE Image URL from the given specs.
   *
   * @param host host to send to
   * @param requestType   type of request (REQ_IMAGEDATA, REQ_IMAGEDIR)
   * @param group   ADDE group (may be null)
   * @param descriptor   ADDE descriptor (may be null)
   * @param query   query string (key/value pairs)
   */
  public AddeImageURL(String host, String requestType, String group,
                      String descriptor, String query) {
    super(host, requestType, group, descriptor, query);
  }

  /**
   * Create an ADDE Image URL from the given spec
   * @param host host to send to
   * @param requestType   type of request (REQ_IMAGEDATA, REQ_IMAGEDIR)
   * @param group   ADDE group
   * @param descriptor ADDE descriptor
   * @param locateKey   locate key
   * @param locateValue  locate value
   * @param placeValue  place value
   * @param lines       number of lines
   * @param elements    number of elements
   * @param lmag        line magnification
   * @param emag        element magnification
   * @param band        band
   * @param unit        calibration unit
   * @param spacing     data size
   */
  public AddeImageURL(String host, String requestType, String group,
                      String descriptor, String locateKey,
                      String locateValue, String placeValue, int lines,
                      int elements, int lmag, int emag, String band,
                      String unit, int spacing) {
    super(host, requestType, group, descriptor);
    this.locateKey = locateKey;
    this.locateValue = locateValue;
    this.placeValue = placeValue;
    this.lines = lines;
    this.elements = elements;
    this.lmag = lmag;
    this.emag = emag;
    this.band = band;
    this.unit = unit;
    this.spacing = spacing;
  }

  /**
   * Get the PLACE value
   *
   * @return the PLACE value
   */
  public String getPlaceValue() {
    return placeValue;
  }

  /**
   * Get the locate key
   *
   * @return the locate key
   */
  public String getLocateKey() {
    return locateKey;
  }

  /**
   * Get the locate value
   *
   * @return the locate value
   */
  public String getLocateValue() {
    return locateValue;
  }

  /**
   * Get the number of lines
   *
   * @return the number of lines
   */
  public int getLines() {
    return lines;
  }

  /**
   * Get the number of elements
   *
   * @return the number of elements
   */
  public int getElements() {
    return elements;
  }

  /**
   * Get the element magnification
   *
   * @return the element magnification
   */
  public int getElementMag() {
    return emag;
  }

  /**
   * Get the line magnification
   *
   * @return the line magnification
   */
  public int getLineMag() {
    return lmag;
  }

  /**
   * Set the locate key
   *
   * @param value  the locate key
   */
  public void setLocateKey(String value) {
    locateKey = value;
  }

  /**
   * Set the locate value
   *
   * @param value the locate value
   */
  public void setLocateValue(String value) {
    locateValue = value;
  }

  /**
   * Set the place value
   *
   * @param value the place value
   */
  public void setPlaceValue(String value) {
    placeValue = value;
  }

  /**
   * Set the number of lines
   *
   * @param value  the number of lines
   */
  public void setLines(int value) {
    lines = value;
  }

  /**
   * Set the number of elements
   *
   * @param value the number of elements
   */
  public void setElements(int value) {
    elements = value;
  }

  /**
   * Set the element magnification
   *
   * @param value the element magnification
   */
  public void setElementMag(int value) {
    emag = value;
  }

  /**
   * Set the line magnification
   *
   * @param value the line magnification
   */
  public void setLineMag(int value) {
    lmag = value;
  }

  /**
   * Set the data size (SPAC)
   *
   * @param value the data size
   */
  public void setSpacing(int value) {
    spacing = value;
  }

  /**
   * Get the data size (SPAC)
   *
   * @return the data size
   */
  public int getSpacing() {
    return spacing;
  }

  /**
   * Set the band or band range
   *
   * @param value   the band range or ALL.  For  REQ_IMAGEDATA, must
   *                be a single band
   */
  public void setBand(String value) {
    band = value;
  }

  /**
   * Get the band or band range
   *
   * @return the band range or ALL.
   */
  public String getBand() {
    return band;
  }

  /**
   * Set the calibration unit
   *
   * @param value   the calibration unit
   */
  public void setUnit(String value) {
    unit = value;
  }

  /**
   * Get the calibration unit
   *
   * @return calibration unit
   */
  public String getUnit() {
    return unit;
  }

  /**
   * Set the navigation type
   *
   * @param value   the navigation type (X or LALO)
   */
  public void setNavType(String value) {
    navType = value;
  }

  /**
   * Get the navigation type
   *
   * @return navigation type (default or LALO)
   */
  public String getNavType() {
    return navType;
  }

  /**
   * Set the location ID for radar images
   *
   * @param value   the location ID
   */
  public void setId(String value) {
    locationId = value;
  }

  /**
   * Get the location ID for radar images
   *
   * @return location ID
   */
  public String getId() {
    return locationId;
  }

  /**
   * Set the dataset position
   *
   * @param value   the dataset position
   */
  public void setDatasetPosition(int value) {
    pos = value;
  }

  /**
   * Get the dataset position
   *
   * @return dataset position
   */
  public int getDatasetPosition() {
    return pos;
  }

  /**
   * Set the start date for the request
   *
   * @param value   the starting date for the request
   */
  public void setStartDate(Date value) {
    startDate = value;
  }

  /**
   * Get the start date for the request
   *
   * @return the start date for the request
   */
  public Date getStartDate() {
    return startDate;
  }

  /**
   * Set the end date for the request
   *
   * @param value   the ending date for the request
   */
  public void setEndDate(Date value) {
    endDate = value;
  }

  /**
   * Get the end date for the request
   *
   * @return the ending date for the request
   */
  public Date getEndDate() {
    return endDate;
  }


  /**
   * Set the time coverage
   *
   * @param value   the time coverage
   */
  public void setTimeCoverage(String value) {
    timeCoverage = value;
  }

  /**
   * Set the time coverage
   *
   * @return the time coverage
   */
  public String getTimeCoverage() {
    return timeCoverage;
  }

  /**
   * Set the AUX keyword value
   *
   * @param value   the AUX keyword value (YES, NO or DEFAULT_VALUE)
   */
  public void setAuxValue(String value) {
    auxValue = value;
  }

  /**
   * Get the AUX keyword value
   *
   * @return the AUX keyword value (YES, NO or DEFAULT_VALUE)
   */
  public String getAuxValue() {
    return auxValue;
  }

  /**
   * Set the DOC keyword value
   *
   * @param value   the DOC keyword value (YES, NO or DEFAULT_VALUE)
   */
  public void setDocValue(String value) {
    docValue = value;
  }

  /**
   * Get the DOC keyword value
   *
   * @return the DOC keyword value (YES, NO or DEFAULT_VALUE)
   */
  public String getDocValue() {
    return docValue;
  }


  /**
   * Create the ADDE URL
   * @return a Adde URL
   */
  protected String makeQuery() {
    StringBuffer buf = new StringBuffer(super.makeQuery());
    if (getRequestType().equals(REQ_IMAGEDATA)) {
      appendKeyValue(buf, KEY_BAND, band);
      appendKeyValue(buf, getLocateKey(), getLocateValue());
      appendKeyValue(buf, KEY_PLACE, getPlaceValue());
      appendKeyValue(buf, KEY_SIZE, getLines() + " " + getElements());
      appendKeyValue(buf, KEY_UNIT, getUnit());
      appendKeyValue(buf, KEY_MAG, getLineMag() + " " + getElementMag());
      appendKeyValue(buf, KEY_SPAC, ((getSpacing() == -1)
                                     ? DEFAULT_VALUE
                                     : "" + getSpacing()));
      appendKeyValue(buf, KEY_NAV, getNavType());
      appendKeyValue(buf, KEY_AUX, getAuxValue());
      appendKeyValue(buf, KEY_DOC, getDocValue());
    }
    else {
      appendKeyValue(buf, KEY_BAND, ALL);
    }
    // add in for the radar queries
    if (getId() != null) appendKeyValue(buf, KEY_ID, getId());
    appendDateOrPosString(buf);
    return buf.toString();
  }

  /**
   * Create a DAY/TIME or POS string
   * @param buf  buffer to append to
   */
  protected void appendDateOrPosString(StringBuffer buf) {
    if (getStartDate() == null && getEndDate() == null) {
      appendKeyValue(buf, KEY_POS, "" + getDatasetPosition());
    }
    else {
      int[] start = null;
      if (getStartDate() != null)
        start = McIDASUtil.mcSecsToDayTime(getStartDate().getTime() / 1000l);
      int[] end = null;
      if (getEndDate() != null)
        end = McIDASUtil.mcSecsToDayTime(getEndDate().getTime() / 1000l);
      StringBuffer day = new StringBuffer();
      StringBuffer time = new StringBuffer();
      if (start != null) {
        day.append("" + start[0]);
        time.append(McIDASUtil.mcHmsToStr(start[1]));
      }
      day.append(" ");
      time.append(" ");
      if (end != null) {
        if (getRequestType().equals(REQ_IMAGEDIR)) day.append("" + end[0]);
        time.append("" + McIDASUtil.mcHmsToStr(end[1]));
      }
      else {
        time.append(McIDASUtil.mcHmsToStr(start[1]));
      }
      time.append(" ");
      time.append(getTimeCoverage());
      appendKeyValue(buf, KEY_DAY, day.toString().trim());
      appendKeyValue(buf, KEY_TIME, time.toString().trim());
    }
  }
}

