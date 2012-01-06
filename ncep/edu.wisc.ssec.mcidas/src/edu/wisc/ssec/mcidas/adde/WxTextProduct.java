//
// WxTextProduct.java
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


import edu.wisc.ssec.mcidas.McIDASUtil;

import java.util.Date;


/**
 * Class to hold a weather text product returned from an ADDE server
 */
public class WxTextProduct {

  /** soure of the data */
  private String source = "";

  /** number of bytes */
  private int numBytes = 0;

  /** location in the file */
  private int location = 0;

  /** day of report */
  private int day = 0;

  /** time of report */
  private int time = 0;

  /** WMO id*/
  private String wmo = "";

  /** WMO station */
  private String wstn = "";

  /** AFOS product id */
  private String apro = "";

  /** AFOS station */
  private String astn = "";

  /** product text */
  private String text = "";

  /** date of bulleting */
  private Date date = new Date();

  /**
   * Create a new WxTextProduct from the header info.
   *
   * @param header   the ADDE header
   */
  public WxTextProduct(byte[] header) {
    int[] values = McIDASUtil.bytesToIntegerArray(header, 0, 13);
    source = McIDASUtil.intBitsToString(values[0]);
    numBytes = values[1];
    location = values[2];
    day = values[10];
    time = values[3];
    String wmoBase = McIDASUtil.intBitsToString(values[4]);
    int wmoNum = values[5];
    wmo = wmoBase + ((wmoNum < 10)
                       ? "0"
                       : "") + wmoNum;
    wstn = McIDASUtil.intBitsToString(values[6]);
    apro = McIDASUtil.intBitsToString(values[7]);
    astn = McIDASUtil.intBitsToString(values[8]);
    date = new Date(McIDASUtil.mcDayTimeToSecs(day, time) * 1000);
  }

  /**
   *  Adde text to the existing text
   *
   * @param newText 
   */
  public void addText(String newText) {
    text = text + newText;
  }

  /**
   * Set the text of the bulleting
   *
   * @param newText  the text
   */
  public void setText(String newText) {
    text = newText;
  }

  /**
   * Get the product text
   *
   * @return the product text
   */
  public String getText() {
    return text;
  }

  /**
   * Get the source of the product
   *
   * @return the source of the product
   */
  public String getSource() {
    return source;
  }

  /**
   * Get the day of the product
   *
   * @return the day of the product
   */
  public int getDay() {
    return day;
  }

  /**
   * Get the time of the product
   *
   * @return the time of the product
   */
  public int getTime() {
    return time;
  }

  /**
   * Get the wmo id of the product
   *
   * @return the wmo id of the product
   */
  public String getWmo() {
    return wmo;
  }

  /**
   * Get the wmo station
   *
   * @return the wmo station
   */
  public String getWstn() {
    return wstn;
  }

  /**
   * Get the AFOS product id
   *
   * @return the AFOS product id
   */
  public String getApro() {
    return apro;
  }

  /**
   * Get the AFOS station
   *
   * @return the AFOS station
   */
  public String getAstn() {
    return astn;
  }

  /**
   * Get the date of the product
   *
   * @return the date of the product
   */
  public Date getDate() {
    return date;
  }

  /**
   * Get the string representation of this product
   */
  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append("SOU    nb  location   day    time  WMO     WSTN APRO ASTN\n");
    buf.append("---  ----  -------- ------- ------ ------- ---- ---- ----\n"); 
    buf.append(source);
    buf.append(numBytes);
    buf.append(" ");
    buf.append(location);
    buf.append(" ");
    buf.append(day);
    buf.append(" ");
    buf.append(time);
    buf.append(" ");
    buf.append(wmo);
    buf.append("  ");
    buf.append(wstn);
    buf.append(" ");
    buf.append(apro);
    buf.append(" ");
    buf.append(astn);
    return buf.toString();
  }

}

