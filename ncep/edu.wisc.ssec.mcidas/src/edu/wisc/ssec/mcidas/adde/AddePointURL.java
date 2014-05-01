//
// AddePointURL.java
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


import java.util.regex.*;


/**
* A class for holding the ADDE URL for an image directory or data request.
 * Decode the ADDE request for point data.
 *
 * If the request contains specific parameters (eg param=t),
 * then the class variable binaryData is set to this param string
 *
 * <pre>
 *   group=&lt;groupname&gt;         ADDE group name
 *   descr=&lt;descriptor&gt;        ADDE descriptor name
 *   pos=&lt;position&gt;            request an absolute or relative ADDE
 *                               position number
 *   select=&lt;select clause&gt;    to specify which data is required
 *   param=&lt;param list&gt;        what parameters to return
 *                             eg param=t[c]
 *                             note that the units [c] are ignored by server
 *                             it is the clients task to convert units
 *                             Note that if "param=" is used,
 *                             binaryData is set to the
 *                             (processed) parameter list
 *   max=&lt;max&gt;                 maximum number of obs to return
 *   trace=&lt;0/1&gt;               setting to 1 tells server to write debug
 *                               trace file (imagedata, imagedirectory)
 *   binaryData=&lt;param list&gt;   because an unlimited number of parameters may
 *                             be requested, these must be packaged up at the end
 *                             of the adde request, and this is known as the
 *                             "binary data" part of the request
 *
 * the following keywords are required:
 *
 *   group
 *
 * an example URL might look like:
 *   adde://rtds/point?group=neons&amp;descr=metar
 *
 * </pre>
 */
public class AddePointURL extends AddeDatasetURL {

  /** Keyword for SELECT */
  public static final String KEY_SELECT = "SELECT";

  /** Keyword for PARAM */
  public static final String KEY_PARAM = "PARAM";

  /** Keyword for MAX */
  public static final String KEY_MAX = "MAX";

  /** Keyword for MAX */
  public static final String KEY_NUM = "NUM";

  /** Keyword for POS */
  public static final String KEY_POS = "POS";

  /** the select clause */
  private String selectClause = "";

  /** the parameters */
  private String params = "";

  /** the max value */
  private int max = 1;

  /** the max value */
  private String pos = DEFAULT_VALUE;

  /** no arg constructor */
  public AddePointURL() {}

  /**
   * Create an AddePointURL.
   *
   * @param host host to send to
   * @param requestType   type of request (REQ_IMAGEDATA, REQ_IMAGEDIR)
   * @param group   ADDE group
   * @param descriptor   ADDE descriptor
   */
  public AddePointURL(String host, String requestType, String group,
                      String descriptor) {
    this(host, requestType, group, descriptor, null);
  }

  /**
   * Create an ADDE PointURL from the given specs.
   *
   * @param host host to send to
   * @param requestType   type of request (REQ_IMAGEDATA, REQ_IMAGEDIR)
   * @param group   ADDE group (may be null)
   * @param descriptor   ADDE descriptor (may be null)
   * @param query   query string (key/value pairs)
   */
  public AddePointURL(String host, String requestType, String group,
                      String descriptor, String query) {
    super(host, requestType, group, descriptor, query);
  }

  /**
   * Create an ADDE Point URL from the given spec
   * @param host host to send to
   * @param requestType   type of request (REQ_IMAGEDATA, REQ_IMAGEDIR)
   * @param group   ADDE group
   * @param descriptor ADDE descriptor
   * @param position   dataset position (number or ALL)
   * @param select     select clause
   * @param paramList  parameter list
   * @param maxNum     maximum number to return
   */
  public AddePointURL(String host, String requestType, String group,
                      String descriptor, String position, String select,
                      String paramList, int maxNum) {
    super(host, requestType, group, descriptor);
    this.pos = position;
    this.selectClause = selectClause;
    this.params = paramList;
    this.max = maxNum;
  }

  /**
   * Get the SELECT value
   *
   * @return the PLACE value
   */
  public String getSelectClause() {
    return selectClause;
  }

  /**
   * Set the SELECT clause
   *
   * @param value the SELECT clause
   */
  public void setSelectClause(String value) {
    selectClause = value;
  }

  /**
   * Get the PARAM value
   *
   * @return the PARAM value
   */
  public String getParams() {
    return params;
  }

  /**
   * Set the PARAM value
   *
   * @param value the PARAM clause
   */
  public void setParams(String value) {
    params = value;
  }

  /**
   * Get the MAX value
   *
   * @return the MAX value
   */
  public int getMaxNumber() {
    return max;
  }

  /**
   * Set the MAX value
   *
   * @param value the MAX clause
   */
  public void setMaxNumber(int value) {
    max = value;
  }

  /**
   * Get the POS value
   *
   * @return the POS value (number or ALL)
   */
  public String getPosition() {
    return pos;
  }

  /**
   * Set the POS value
   *
   * @param value the POS clause (number or ALL)
   */
  public void setPosition(String value) {
    pos = value;
  }

  /**
   * Create the ADDE URL
   * @return a Adde URL
   */
  protected String makeQuery() {
    StringBuffer buf = new StringBuffer(super.makeQuery());
    if (!getSelectClause().equals(""))
      appendKeyValue(buf, KEY_SELECT, getSelectClause());
    if (!getParams().equals("")) appendKeyValue(buf, KEY_PARAM, getParams());
    appendKeyValue(buf, KEY_MAX, String.valueOf(getMaxNumber()));
    appendKeyValue(buf, KEY_POS, getPosition());
    return buf.toString();
  }

  /**
   * Decode a URL and return an AddePointURL
   *
   * @param baseURL   url to decode
   * @return an AddePointURL object (or null)
   */
  public static AddePointURL decodeURL(String baseURL) {
    Pattern pattern = Pattern.compile("(.*)://(.*)/(.*)\\?(.*)");
    Matcher matcher = pattern.matcher(baseURL);
    boolean ok = matcher.find();
    if (!ok) {
      return null;
    }
    int numGroups = matcher.groupCount();
    if (numGroups >= 3) {
      String protocol = matcher.group(1);
      String requestType = matcher.group(3);
      if (!(protocol.equals("adde") &&
            requestType.toLowerCase().startsWith("point")))
        return null;
      AddePointURL apu = new AddePointURL();
      apu.setHost(matcher.group(2));
      apu.setRequestType(requestType);
      if (numGroups > 3) {
        String query = matcher.group(4);
        apu.parseQuery(query);
      }
      return apu;
    }
    return null;
  }

  /**
   * Parse the query string and set the values accordingly, subclasses
   * should extend to parse their particular keywords
   * @param query query string
   */
  protected void parseQuery(String query) {
    super.parseQuery(query);
    String test = getValue(query, KEY_SELECT);
    if (test != null) {
      setSelectClause(test);
    }
    test = getValue(query, KEY_PARAM);
    if (test != null) {
      setParams(test);
    }
    test = getValue(query, KEY_POS);
    if (test != null) {
      setPosition(test);
    }
    test = getValue(query, KEY_MAX);
    if (test != null) {
      setMaxNumber(Integer.parseInt(test));
    }
    // and just in case people are using NUM, convert to max else {
    test = getValue(query, KEY_NUM);
    if (test != null) {
      int num = max;
      if (test.equalsIgnoreCase(ALL)) {
        num = 99999;
      }
      else {
        try {
          num = Integer.parseInt(test);
        }
        catch (Exception e) {
        }
      }
      setMaxNumber(num);
    }
  }

  /**
   * Test the parsing
   *
   * @param args  url to parse
   */
  public static void main(String[] args) {
    if (args.length > 0) {
      AddePointURL url = AddePointURL.decodeURL(args[0]);
      System.out.println(url.getURLString());
    }
  }
}

