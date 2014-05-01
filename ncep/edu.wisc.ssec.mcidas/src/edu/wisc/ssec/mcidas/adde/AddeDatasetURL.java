//
// AddeDatasetURL.java
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
 * A subclass of AddeURL to support queries on datasets.
 * <pre>
 *
 * URLs must all have the following format:
 *
 *   adde://host/request?keyword_1=value_1&amp;keyword_2=value_2
 *
 *   group=&lt;groupname&gt;         ADDE dataset group
 *   descr=&lt;descriptor&gt;        ADDE dataset descriptor
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
 */
public class AddeDatasetURL extends AddeURL {

  /** Keyword for dataset group */
  public static final String KEY_GROUP = "GROUP";

  /** Keyword for dataset descriptor */
  public static final String KEY_DESCRIPTOR = "DESCRIPTOR";

  /**
   * Dataset name
   */
  private String group = null;

  /**
   * Dataset name
   */
  private String descriptor = null;

  /**
   * Create an ADDE URL
   */
  public AddeDatasetURL() {}

  /**
   * Create an ADDE Dataset URL from the given spec
   * @param host host to send to
   * @param requestType   type of request (REQ_*)
   * @param group   ADDE group
   */
  public AddeDatasetURL(String host, String requestType, String group) {
    this(host, requestType, group, null);
  }

  /**
   * Create an ADDE Dataset URL from the given spec
   * @param host host to send to
   * @param requestType   type of request (REQ_*)
   * @param group   ADDE group
   * @param descriptor   ADDE descriptor (may be null)
   */
  public AddeDatasetURL(String host, String requestType, String group,
                        String descriptor) {
    this(host, requestType, group, descriptor, null);
  }

  /**
   * Create an ADDE URL from the given spec
   * @param host host to send to
   * @param requestType   type of request (REQ_*)
   * @param group   ADDE group
   * @param descriptor   ADDE descriptor (may be null)
   * @param extraKeys   extraKeys string (key/value pairs)
   */
  public AddeDatasetURL(String host, String requestType, String group,
                        String descriptor, String extraKeys) {
    super(host, requestType, extraKeys);
    this.group = group;
    this.descriptor = descriptor;
  }

  /**
   * Make the query portion of the URL (e.g., key1=value1&amp;key2=value2..)
   * Subclasses should override.
   *
   * @return the query portion of the URL
   */
  protected String makeQuery() {
    StringBuffer buf = new StringBuffer(super.makeQuery());
    if (getGroup() != null) appendKeyValue(buf, KEY_GROUP, getGroup());
    if (getDescriptor() != null)
      appendKeyValue(buf, KEY_DESCRIPTOR, getDescriptor());
    return buf.toString();
  }

  /**
   * Get the group for this ADDE URL
   * @return the group
   */
  public String getGroup() {
    return group;
  }

  /**
   * Set the group for this ADDE URL
   * @param group the group
   */
  public void setGroup(String group) {
    this.group = group;
  }

  /**
   * Get the dataset descriptor for this ADDE URL
   * @return the dataset descriptor
   */
  public String getDescriptor() {
    return descriptor;
  }

  /**
   * Set the dataset descriptor for this ADDE URL
   * @param desc the dataset descriptor
   */
  public void setDescriptor(String desc) {
    this.descriptor = desc;
  }

  /**
   * Parse the query string and set the values accordingly, subclasses
   * should extend to parse their particular keywords
   * @param query query string
   */
  protected void parseQuery(String query) {
    super.parseQuery(query);
    String test = getValue(query, KEY_GROUP);
    if (test != null) {
      setGroup(test);
    }
    // should be able to do this, but old URLS have desc
    //test = getValue(query, KEY_DESCRIPTOR);
    test = getValue(query, "DESC");
    if (test != null) {
      setDescriptor(test);
    }
  }

}

