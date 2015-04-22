//
// AddeURL.java
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


/**
 * A class for holding information about an ADDE URL.
 *
 * <pre>
 *
 * URLs must all have the following format:
 *
 *   adde://host/request?keyword_1=value_1&amp;keyword_2=value_2
 *
 * where request can be one of the following:
 *
 *   datasetinfo - request for data set information (LWPR)
 *   griddirectory - request for grid directory information (GDIR)
 *   griddata - request for grid data (GGET)
 *   imagedata - request for data in AreaFile format (AGET)
 *   imagedirectory - request for image directory information (ADIR)
 *   pointdata - request for point data (MDKS)
 *   textdata - request to read a text file (TXTG)
 *   wxtext   - request to read a weather text file (WTXG)
 *   obtext   - request to read a observation text file (OBTG)
 *
 * There can be any valid combination of the following supported keywords:
 *
 * -------for any request
 *
 *   group=&lt;groupname&gt;         ADDE group name
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
 *
 * </pre>
 */
public class AddeURL implements Cloneable {

  /** The protocol */
  public static final String ADDE_PROTOCOL = "adde";

  /** AGET request type */
  public final static String REQ_AGET = "aget";

  /** ADIR request type */
  public final static String REQ_ADIR = "adir";

  /** LWPR request type */
  public final static String REQ_LWPR = "lwpr";

  /** GDIR request type */
  public final static String REQ_GDIR = "gdir";

  /** GGET request type */
  public final static String REQ_GGET = "gget";

  /** MDKS request type */
  public final static String REQ_MDKS = "mdks";

  /** TXTG request type */
  public final static String REQ_TXTG = "txtg";

  /** WTXG request type */
  public final static String REQ_WTXG = "wtxg";

  /** OBTG request type */
  public final static String REQ_OBTG = "obtg";

  /** Image data request type */
  public final static String REQ_IMAGEDATA = "imagedata";

  /** Image directory request type */
  public final static String REQ_IMAGEDIR = "imagedirectory";

  /** Data set info request type */
  public final static String REQ_DATASETINFO = "datasetinfo";

  /** Text request type */
  public final static String REQ_TEXT = "text";

  /** weather text request type */
  public final static String REQ_WXTEXT = "wxtext";

  /** obs text request type */
  public final static String REQ_OBTEXT = "obtext";

  /** Grid data request type */
  public final static String REQ_GRIDDATA = "griddata";

  /** Grid directory request type */
  public final static String REQ_GRIDDIR = "griddir";

  /** Point data request type */
  public final static String REQ_POINTDATA = "point";

  /** Default value for key/value pairs (X) */
  public final static String DEFAULT_VALUE = "X";

  /** Value for yes */
  public final static String YES = "YES";

  /** Value for no */
  public final static String NO = "NO";

  /** Value for ALL */
  public final static String ALL = "ALL";

  /** Keyword for trace */
  public static final String KEY_TRACE = "TRACE";

  /** Keyword for debug */
  public static final String KEY_DEBUG = "DEBUG";

  /** Keyword for port */
  public static final String KEY_PORT = "PORT";

  /** Keyword for project */
  public static final String KEY_PROJ = "PROJ";

  /** Keyword for user */
  public static final String KEY_USER = "USER";

  /** Keyword for compress */
  public static final String KEY_COMPRESS = "COMPRESS";

  /** Keyword for version */
  public static final String KEY_VERSION = "VERSION";

  /** Flag for "compress" compression. */
  public final static int COMPRESS = 503;

  /** Flag for "no compress" compression. */
  public final static int NO_COMPRESS = 500;

  /** Flag for GZip compression. */
  public final static int GZIP = 112;

  /** flag  for trace on */
  public static final int TRACE_ON = 0;

  /** flag  for trace off */
  public static final int TRACE_OFF = 1;

  /** the host */
  private String host = "localhost";

  /** the version */
  private String version = "" + AddeURLConnection.VERSION_1;

  /** the user id */
  private String user = AddeURLConnection.DEFAULT_USER;

  /** the project number */
  private int project = AddeURLConnection.DEFAULT_PROJ;

  /** the trace flag */
  private int trace = 0;

  /** the extra key/value pairs */
  private String extraKeys = null;

  /** the compression type */
  private int compress = -1; // let port determine compression by default

  /** the port */
  private int port = AddeURLConnection.DEFAULT_PORT;

  /** the request type */
  private String requestType = "";

  /** the debug type */
  private boolean debug = false;

  /**
   * Create an ADDE URL
   */
  public AddeURL() {}

  /**
   * Create an ADDE URL from the given spec
   * @param host host to send to
   * @param requestType   type of request (REQ_*)
   */
  public AddeURL(String host, String requestType) {
    this(host, requestType, null);
  }

  /**
   * Create an ADDE URL from the given spec
   * @param host host to send to
   * @param requestType   type of request (REQ_*)
   * @param extraKeys     extra key/value arguments
   */
  public AddeURL(String host, String requestType, String extraKeys) {
    this.host = host;
    this.requestType = requestType;
    this.extraKeys = extraKeys;
  }

  /**
   * Create the ADDE URL as a String
   *
   * @return an Adde URL
   */
  public String getURLString() {
    StringBuffer buf = new StringBuffer(ADDE_PROTOCOL);
    buf.append("://");
    buf.append(host);
    buf.append("/");
    buf.append(requestType);
    buf.append("?");
    buf.append(makeQuery());
    return buf.toString();
  }

  /**
   * Make the query portion of the URL (e.g., key1=value1&amp;key2=value2..)
   * Subclasses should override.
   *
   * @return the query portion of the URL
   */
  protected String makeQuery() {
    StringBuffer buf = new StringBuffer();
    appendKeyValue(buf, KEY_PORT, "" + getPort());
    appendKeyValue(buf, KEY_COMPRESS, getCompressionType());
    appendKeyValue(buf, KEY_USER, getUser());
    appendKeyValue(buf, KEY_PROJ, "" + getProject());
    appendKeyValue(buf, KEY_VERSION, getVersion());
    appendKeyValue(buf, KEY_DEBUG, Boolean.toString(getDebug()));
    appendKeyValue(buf, KEY_TRACE, "" + getTrace());
    if (getExtraKeys() != null) {
      if (!getExtraKeys().startsWith("&")) buf.append("&");
      buf.append(getExtraKeys());
    }
    return buf.toString();
  }

  /**
   * Parse the query string and set the values accordingly, subclasses
   * should extend to parse their particular keywords
   * @param query query string
   */
  protected void parseQuery(String query) {
    String test = getValue(query, KEY_PORT);
    if (test != null) {
      setPort(Integer.parseInt(test));
    }
    test = getValue(query, KEY_COMPRESS);
    if (test != null) {
      setCompressionFromString(test);
    }
    test = getValue(query, KEY_USER);
    if (test != null) {
      setUser(test);
    }
    test = getValue(query, KEY_PROJ);
    if (test != null) {
      setProject(Integer.parseInt(test));
    }
    test = getValue(query, KEY_VERSION);
    if (test != null) {
      setVersion(test);
    }
    test = getValue(query, KEY_DEBUG);
    if (test != null) {
      setDebug(test.equals("true"));
    }
    test = getValue(query, KEY_TRACE);
    if (test != null) {
      setTrace(Integer.parseInt(test));
    }
  }

  /**
   * Get the value for a particular key.
   * @param query  the query string
   * @param key    the key to search
   * @return the  value or null if it doesn't exist
   */
  public String getValue(String query, String key) {
    String retVal = null;
    int keyIndex = query.indexOf(key);
    if (keyIndex < 0) { // try lowercase version;
      keyIndex = query.indexOf(key.toLowerCase());
    }
    if (keyIndex >= 0) {
      int equalIndex = query.indexOf("=", keyIndex); 
      int ampIndex = query.indexOf("&", keyIndex);
      retVal = (ampIndex >= 0)
               ? query.substring(equalIndex+1, ampIndex) // to the amp
               : query.substring(equalIndex+1); // to the end
    }
    return retVal;
  }


  /**
   * A utility method to make a name=value part of the adde request string
   *
   * @param buf The buffer to append to
   * @param name The property name
   * @param value The value
   */
  protected void appendKeyValue(StringBuffer buf, String name, String value) {
    if ((buf.length() == 0) || (buf.charAt(buf.length() - 1) != '?')) {
      buf.append("&");
    }
    buf.append(name);
    buf.append("=");
    buf.append(value);
  }

  /**
   * Compare two AddeURLs
   *
   * @param o   Object in question
   *
   * @return true if they are the same object or if all
   */
  public boolean equals(Object o) {
    if (!(o.getClass().equals(this.getClass()))) {
      return false;
    }
    AddeURL that = (AddeURL)o;
    if (this == that) {
      return true;
    }
    return getURLString().equals(that.getURLString());
  }

  /**
   * Get the hashcode for this
   *
   * @return the hashcode
   */
  public int hashCode() {
    return getURLString().hashCode();
  }

  /**
   * Get the host for this ADDE URL
   * @return the host
   */
  public String getHost() {
    return host;
  }

  /**
   * Set the host for this ADDE URL
   * @param host the host
   */
  public void setHost(String host) {
    this.host = host;
  }

  /**
   * Get the request type for this ADDE URL
   * @return the host
   */
  public String getRequestType() {
    return requestType;
  }

  /**
   * Set the request type for this ADDE URL
   *
   * @param requestType the request Type
   */
  public void setRequestType(String requestType) {
    this.requestType = requestType;
  }

  /**
   * Get the extraKeys string for this ADDE URL
   * @return the extraKeys string
   */
  public String getExtraKeys() {
    return extraKeys;
  }

  /**
   * Set the extraKeys string for this ADDE URL
   * @param extraKeys the extraKeys
   */
  public void setExtraKeys(String extraKeys) {
    this.extraKeys = extraKeys;
  }

  /**
   * Get the project for this ADDE URL
   * @return the project
   */
  public String getUser() {
    return user;
  }

  /**
   * Set the user for this ADDE URL
   * @param user the user
   */
  public void setUser(String user) {
    this.user = user;
  }

  /**
   * Get the project for this ADDE URL
   * @return the project
   */
  public int getProject() {
    return project;
  }

  /**
   * Set the project for this ADDE URL
   * @param project the project
   */
  public void setProject(int project) {
    this.project = project;
  }

  /**
   * Get the version for this ADDE URL
   * @return the version
   */
  public String getVersion() {
    return version;
  }

  /**
   * Set the version this ADDE URL
   * @param version the version
   */
  public void setVersion(String version) {
    this.version = version;
  }

  /**
   * Get the debug value for this ADDE URL
   * @return the debug value (true or false)
   */
  public boolean getDebug() {
    return debug;
  }

  /**
   * Set the debug value this ADDE URL
   * @param debug the debug value (true or false);
   */
  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  /**
   * Get the port for this ADDE URL
   * @return the port
   */
  public int getPort() {
    return port;
  }

  /**
   * Set the port used for this ADDE URL
   * @param port the port
   */
  public void setPort(int port) {
    this.port = port;
  }

  /**
   * Get the compression type for this ADDE URL
   * @return the compression type
   */
  public int getCompression() {
    return compress;
  }

  /**
   * Set the compression type used for this ADDE URL
   * @param compress the compression type (GZIP, NO_COMPRESS, COMPRESS)
   */
  public void setCompression(int compress) {
    this.compress = compress;
  }

  /**
   * Get the trace value used for this ADDE URL
   * @return the trace value (TRACE_ON, TRACE_OFF)
   */
  public int getTrace() {
    return trace;
  }

  /**
   * Set the trace value used for this ADDE URL
   * @param trace   the trace value (TRACE_ON, TRACE_OFF)
   */
  public void setTrace(int trace) {
    this.trace = trace;
  }

  /**
   * Get the compression type from the port number if not specified.
   *
   * @return the compression type as a string
   */
  private String getCompressionType() {
    String testStr = null;
    int valueToCheck = (compress == -1)
                       ? port
                       : compress;
    switch (valueToCheck) {
      case NO_COMPRESS: // port == 500
      case 1: // port == 1
        testStr = "none";
        break;
      case COMPRESS: // port == 503
      case 2: // port == 2
        testStr = "compress";
        break;
      case GZIP: // port == 112
      case 3: // port == 3
      default:
        testStr = "gzip";
        break;
    }
    return testStr;
  }

  /**
   * Set the compression type from a string
   *
   * @param type  the string type
   */
  public void setCompressionFromString(String type) {
    if (type.equals("gzip") || type.equals("112")) {
      setCompression(112);
    }
    else if (type.equals("compress") || type.equals("503")) {
      setCompression(503);
    }
    else if (type.equals("none") || type.equals("500")) {
      setCompression(500);
    }
  }

  /**
   * Clones this instance.
   *
   * <p>This implementation never throws {@link CloneNotSupportException}.</p>
   *
   * @return                            A clone of this instance.
   * @throws CloneNotSupportedException if cloning isn't supported.
   */
  public Object clone() throws CloneNotSupportedException {

    Object clone = null;
    try {
      clone = super.clone();
    }
    catch (CloneNotSupportedException ex) {
      throw new Error("Assertion failure"); // can't happen
    }
    return clone;
  }

}
