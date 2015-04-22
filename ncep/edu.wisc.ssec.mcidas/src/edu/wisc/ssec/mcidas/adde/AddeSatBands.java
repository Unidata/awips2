//
// AddeSatBands.java
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
import java.util.*;
import java.lang.*;
import java.io.*;

/** Helper class 
  * to interpret the band information 
  * from the ADDE SATBANDS file returned by servers
*/

public class AddeSatBands {
  String[] c;

  public AddeSatBands(String[] cards) {
    c = cards;
  }

  /** given a sensor and a source type, return a list of bands possible
  * @param sensor is the satellite sensor ID number
  * @param src is the sensor source code
  * 
  * @return String[] of channel words.  The array is 
  * 1-based to match the customary way of numbering channels (bands),
  * so element 0 is usually null
  */
  public String[] getBandDescr(int sensor, String src) {
    if (c == null) return null;
    int gotit = -1;
    Vector v = new Vector();
    for (int i=0; i<c.length; i++) {
      if ( ! c[i].startsWith("Sat ")) continue;
      StringTokenizer st = new StringTokenizer(c[i]," ");
      String temp = st.nextToken();  // throw away the key
      int m = st.countTokens();
      for (int k=0; k<m; k++) {
        int ss = Integer.parseInt(st.nextToken().trim());
        if (ss == sensor) {
          gotit = i;
          break;
        }
      }

      if (gotit != -1) break;
    }

    if (gotit == -1) return null;

    // now look for Source
    int gotSrc = -1;
    for (int i=gotit; i<c.length; i++) {
      if (c[i].startsWith("EndSat")) break;
      if ( ! c[i].startsWith("Cal ")) continue;
      String srcVal = c[i].substring(4).trim();
      if (srcVal.equals(src)) {
        gotSrc = i;
        break;
      }

    }

    if (gotSrc == -1) return null;

    gotSrc++;

    for (int i=gotSrc; i<c.length; i++) {
      if (c[i].startsWith("C") || c[i].startsWith("S") || c[i].startsWith("E")) break;
      if (c[i].startsWith("B") ) continue;
      String b = c[i].substring(0,2);
      int bi = Integer.parseInt(b.trim());
      String d = null;
      int ids = c[i].indexOf("DESC=");  // look for new file format

      if (ids > 0) {  // new format
        int idsb = c[i].indexOf("'",ids+5);
        if (idsb < 2) {  // no quoted field
          d = c[i].substring(ids+5);

        } else {
          int idse = c[i].indexOf("'", idsb+1);
          d = c[i].substring(idsb+1,idse);
        }
        
        
      } else {  // old format
        d = c[i].substring(3).trim();
      }

      if (bi >= v.size()) v.setSize(bi+1);
      v.setElementAt(d, bi);
    }

    int num = v.size();
    String[] s = new String[num];
    for (int i=0; i<num; i++) {
      s[i] = (String) v.elementAt(i);
    }

    return s;
  }

  public static void main(String[] a) {
    try {
      DataInputStream das = new DataInputStream(new FileInputStream("/src/edu/wisc/ssec/mcidas/adde/satband.txt"));
      //DataInputStream das = new DataInputStream(new FileInputStream("/src/edu/wisc/ssec/mcidas/adde/SATBAND-new.txt"));

      Vector v = new Vector();
      while(true) {
        String s = das.readLine();
        if (s == null) break;
        v.addElement(s);
      }

      das.close();
      int num = v.size();
      System.out.println("size of input file = "+num);

      String sat = "12";
      //String src = "MSAT";  // this is an error
      String src = "GMS";

      if (a != null && a.length > 1) {
        sat = a[0];
        src = a[1];
      }
      
      String[] sv = new String[num];
      for (int i=0; i<num; i++) { sv[i] = (String) v.elementAt(i); }
      AddeSatBands asb = new AddeSatBands(sv);
      String[] f = asb.getBandDescr(Integer.parseInt(sat), src);
      System.out.println("return from addesatbands");
      if (f == null) {
        System.out.println("####  No matches found...!");
      } else {

        int numb = f.length;
        System.out.println("length of return = "+numb);
        for (int i=0; i<numb; i++) {
          System.out.println("band = value -> "+i+" = "+f[i]);
        }
      }

    } catch (Exception e) {System.out.println(e);}

  }
}
