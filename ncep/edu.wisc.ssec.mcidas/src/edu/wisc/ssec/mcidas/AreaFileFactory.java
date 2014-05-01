//
// AreaFileFactory.java
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

package edu.wisc.ssec.mcidas;


import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import edu.wisc.ssec.mcidas.adde.AddeURLException;


/**
 * Utility class for creating <code>AreaFile</code> instances. This class
 * handles subsetting local files using urls whereas the <code>AreaFile</code>
 * constructors do not.
 *
 * <p>No instances of this class can be created.</p>
 * @version $Id: AreaFileFactory.java,v 1.8 2009-07-01 12:51:04 donm Exp $
 * @author Bruce Flynn, SSEC
 */
public final class AreaFileFactory {

  /** No-op constructor preventing instatiation of this class. */
  private AreaFileFactory() {}

  /**
   * String to calibration int.
   * @param cal calibration type string
   *
   * @return calibration type integer, Calibrator.CAL_NONE if unknown
   */
  public final static int calStrToInt(String cal) {
    int ret = Calibrator.CAL_NONE;
    String calType = cal.trim();
    if (calType.equalsIgnoreCase("temp")) ret = Calibrator.CAL_TEMP;
    else if (calType.equalsIgnoreCase("brit")) ret = Calibrator.CAL_BRIT;
    else if (calType.equalsIgnoreCase("rad")) ret = Calibrator.CAL_RAD;
    else if (calType.equalsIgnoreCase("raw")) ret = Calibrator.CAL_RAW;
    else if (calType.equalsIgnoreCase("refl")) ret = Calibrator.CAL_ALB;
    return ret;
  }

  /**
   * Calibration int to string.
   * @param cal calibration type
   *
   * @return calibration type string, raw if unknown
   */
  public final static String calIntToStr(int cal) {
    String ret = "raw";
    switch (cal) {
      case Calibrator.CAL_ALB:
        ret = "refl";
        break;
      case Calibrator.CAL_BRIT:
        ret = "brit";
        break;
      case Calibrator.CAL_RAD:
        ret = "rad";
        break;
      case Calibrator.CAL_RAW:
        ret = "raw";
        break;
      case Calibrator.CAL_TEMP:
        ret = "temp";
        break;
    }
    return ret;
  }

  /**
   * Construct a url query string for subsetting a local file.
   * @param sl starting line number.
   * @param nl number of lines.
   * @param lm line magnification.
   * @param se starting element.
   * @param ne number of elements.
   * @param em element magnification.
   * @param b band number.
   * @return &quot;?band=B&amp;linele=SL SE&amp;size=NL NE&amp;mag=LM EM&quot;
   */
  public final static String makeLocalQuery(int sl, int nl, int lm, int se,
                                            int ne, int em, int b) {
    return "?band=" + b + "&linele=" + sl + " " + se + "&size=" + nl + " " +
           ne + "&mag=" + lm + " " + em;
  }

  /**
   * Construct a url query string for subsetting a local file.
   * @param sl starting line number.
   * @param nl number of lines.
   * @param lm line magnification.
   * @param se starting element.
   * @param ne number of elements.
   * @param em element magnification.
   * @param b band number.
   * @param c calibration type
   * @return &quot;?band=B&amp;linele=SL SE&amp;size=NL NE&amp;mag=LM EM&quot&unit=C;
   */
  public final static String makeLocalQuery(int sl, int nl, int lm, int se,
                                            int ne, int em, int b, int c) {
    String ct = calIntToStr(c);
    return "?band=" + b + "&linele=" + sl + " " + se + "&size=" + nl + " " +
           ne + "&mag=" + lm + " " + em + "&unit=" + ct;
  }

  /**
   * Construct a url query string for subsetting a local file.
   * @param sl starting line number.
   * @param nl number of lines.
   * @param lm line magnification.
   * @param se starting element.
   * @param ne number of elements.
   * @param em element magnification.
   * @param b band number.
   * @param c calibration type as a string
   * @return &quot;?band=B&amp;linele=SL SE&amp;size=NL NE&amp;mag=LM EM&quot&unit=C;
   */
  public final static String makeLocalQuery(int sl, int nl, int lm, int se,
                                            int ne, int em, int b, String c) {
    return "?band=" + b + "&linele=" + sl + " " + se + "&size=" + nl + " " +
           ne + "&mag=" + lm + " " + em + "&unit=" + c;
  }


  /**
   * Construct a {@link java.net.URL} for subsetting a local file.
   * @param fpath canonical path to an area file.
   * @param sl starting line number.
   * @param nl number of lines.
   * @param lm line magnification.
   * @param se starting element.
   * @param ne number of elements.
   * @param em element magnification.
   * @param b band number.
   * @param u calibration unit
   * @return a string of the format <code>&quot;file://FPATH?band=B&amp;
   * linele=SL SE&amp;size=NL NE&amp;mag=LM EM&quot;&unit=U</code>
   *
   * @throws MalformedURLException bad URL specification
   */
  public final static URL makeLocalSubsetURL(String fpath, int sl, int nl,
                                             int lm, int se, int ne, int em,
                                             int b, String u)
          throws MalformedURLException {
    String surl = "file://" + fpath +
                  makeLocalQuery(sl, nl, lm, se, ne, em, b, u);
    URL url = null;
    try {
      url = new URL(surl);
    }
    catch (MalformedURLException e) {
      // this cannot occur because the protocol is hardcoded to a known one
    }
    return url;
  }

  /**
   * Create an initialized <code>AreaFile</code> instance. First, an attempt is
   * made to create a <code>URL</code> from <code>src</code>, if an error
   * occurrs, meaning <code>src</code> is not a valid url, <code>src</code> is
   * interpreted as a file path.
   * @param src A relative or canonical path to a local file as a string or a
   * string representation of a url appropriate for creating an
   * <code>AreaFile</code> instance. For more information on urls appropriate
   * for creating <code>AreaFile</code> instances see
   * {@link #getAreaFileInstance(URL)}.
   * <p>URLs containing encoded characters in their query strings will be decoded
   * before parsing.
   * </p>
   * @return an initialized, possibly subsetted, instance
   * @throws AreaFileException on any error constructing the instance.
   * @throws AddeURLException If the source is a URL and the query string is
   *  not formatted correctly.
   */
  public static final AreaFile getAreaFileInstance(final String src)
          throws AreaFileException, AddeURLException {

    // handle the special "pop up a gui" case for adde://image?
    if (src.startsWith("adde://") &&
        (src.endsWith("image?") || src.endsWith("imagedata?"))) {
      return new AreaFile(src);
    }

    // Try to create an instance as a URL first. If a valid URL instance cannot
    // be created lets try 'src' as a filename. 
    // NOTE: it's important to make sure the AreaFile and AddeURL exceptions 
    // are propagated from constructor or factory method.
    AreaFile af = null;
    URL url = null;
    try {
      url = new URL(src);
      af = getAreaFileInstance(url);
    }
    catch (MalformedURLException e) {
      af = new AreaFile(src);
    }

    return af;
  }

  /**
   * Create an initialized <code>AreaFile</code> instance.
   *
   * <p>A url appropriate for creating an instance will have a protocol of
   * either <code>adde</code> for remote ADDE data or <code>file</code> for
   * files on the local disk. Information on consructing ADDE urls can be found
   * in the {@link edu.wisc.ssec.mcidas.adde.AddeURLConnection} class.</p>
   *
   * <p>A local file url may either be a standard file url such as
   * file:///&lt;absolute file path&gt; or it may specify subsetting
   * information. If specifying subsetting information, the url can contain the
   * following parameters from the ADDE image data url specification with the
   * specified defaults:
   * <pre>
   * NAME      DEFAULT
   * linele  - 0 0 a  Must be used with size.
   *                  NOTE: only type 'a' is supported at this time
   * size    - 0 0    Must be used with linele.
   * mag     - 1 1    Only with linele and size, but not required.
   * band    - 1      Can be used separately, not required.
   * unit    - RAW    Calibration type
   * </pre>
   * A file url might look like:
   * <pre>
   * file://&lt;abs file path&gt;?linele=10 10&amp;band=3&amp;mag=-2 -4&amp;size=500 500&unit=BRIT
   * </pre>
   * </p>
   * <p>URLs containing encoded characters in their query strings will be decoded
   * before parsing.
   * </p>
   * @param url - the url as described above
   * @return an initialized, possibly subsetted, instance
   * @throws AreaFileException on any error constructing the instance.
   * @throws AddeURLException if the query string is not a valid ADDE query
   *  stirng.
   */
  public static final AreaFile getAreaFileInstance(final URL url)
          throws AddeURLException, AreaFileException {

    // it's a local file, investigate further
    if (url.getProtocol().equalsIgnoreCase("file")) {

      AreaFile af = null;
      if (url.getQuery() == null) {
        af = new AreaFile(url);
      }
      else {
        // open w/o query string
        af = new AreaFile(url.toString().split("\\?")[0]);
      }

      // is it a local url with subsetting
      String query = url.getQuery();
      if (query != null && query.contains("%")) {
        try {
          query = url.toURI().getQuery(); // URI queries are decoded
        }
        catch (URISyntaxException e) {
          throw new AddeURLException(e.getMessage());
        }
      }
      if (query != null) {
        final String whtspc = "(\\s)+";
        int startLine = 0;
        int numLines = af.getAreaDirectory().getLines();
        int lineMag = 1;
        int startElem = 0;
        int numEles = af.getAreaDirectory().getElements();
        int eleMag = 1;
        int band = -1;
        int calType = Calibrator.CAL_NONE;
        boolean linele = false;
        boolean size = false;
        boolean mag = false;
        int availableLines = numLines;
        int availableEles = numEles;

        // parse query string
        String[] props = query.split("&");
        for (int i = 0; i < props.length; i++) {
          String[] kv = props[i].split("=");

          if (kv[0].equalsIgnoreCase("mag")) {
            lineMag = Integer.parseInt(kv[1].split(whtspc)[0]);
            eleMag = Integer.parseInt(kv[1].split(whtspc)[1]);
            mag = true;
          }

          if (kv[0].equalsIgnoreCase("size")) {
            numLines = Integer.parseInt(kv[1].split(whtspc)[0]);
            numEles = Integer.parseInt(kv[1].split(whtspc)[1]);
            size = true;
          }

          if (kv[0].equalsIgnoreCase("band")) {
            int bandIdx = -1;
            int[] bands = af.getAreaDirectory().getBands();
            if (band == -1) {
              bandIdx = 0;
            }
            else {
              for (int j = 0; i < bands.length; j++) {
                if (bands[j] == band) {
                  bandIdx = j;
                }
              }
            }
            band = bands[bandIdx];
          }

          if (kv[0].equalsIgnoreCase("linele")) {
            String[] vals = kv[1].split(whtspc);
            startLine = Integer.parseInt(vals[0]);
            startElem = Integer.parseInt(vals[1]);
            if (vals.length >= 3 && !vals[2].equalsIgnoreCase("a")) {
              throw new AddeURLException("Image and earth types are not currenly supported");
            }
            availableLines -= startLine;
            availableEles -= startElem;
            linele = true;
          }

          if (kv[0].equalsIgnoreCase("unit")) {
            calType = calStrToInt(kv[1]);
            switch (calType) {
              case Calibrator.CAL_ALB:
              case Calibrator.CAL_BRIT:
              case Calibrator.CAL_RAD:
              case Calibrator.CAL_TEMP:
              case Calibrator.CAL_RAW:
              case Calibrator.CAL_NONE:
                break;
              default:
                throw new AreaFileException("Unsupported calibration type: " +
                                            kv[1]);
            }
          }

        }

        if (mag) {
          availableLines = availableLines / (Math.abs(lineMag) == 0
                                             ? 1
                                             : Math.abs(lineMag));
          availableEles = availableEles / (Math.abs(eleMag) == 0
                                           ? 1
                                           : Math.abs(eleMag));
          if (size) {
            numLines = Math.min(availableLines, numLines);
            numEles = Math.min(availableEles, numEles);
          }
          else {
            numLines = availableLines;
            numEles = availableEles;
          }

        }

        // recreate with new parameters
        af = new AreaFile(url.getPath(), startLine, numLines, lineMag,
                          startElem, numEles, eleMag, band);
        af.setCalType(calType);
      }
      return af;

    }
    else {
      return new AreaFile(url);
    }
  }

  /**
   * See {@link AreaFile#AreaFile(String, int, int, int, int, int, int, int)}
   *
   * @param fpath  the path to the file
   * @param startLine the starting image line
   * @param numLines the total number of lines to return
   * @param lineMag the line magnification. Valid values are &gt;= -1. -1, 0, and
   *                1 are all taken to be full line resolution, 2 is every
   *                other line, 3 every third, etc...
   * @param startElem the starting image element
   * @param numEles the total number of elements to return
   * @param eleMag the element magnification. Valid values are &gt;= -1. -1, 0, and 1
   *               are all taken to be full element resolution, 2 is every
   *               other element, 3 every third, etc...
   * @param band the 1-based band number for the subset, which must be present
   *             in the directory blocks band map or -1 for the first band
   *
   * @return
   *
   * @throws AreaFileException
   */
  public final static AreaFile getAreaFileInstance(String fpath,
          int startLine, int numLines, int lineMag, int startElem,
          int numEles, int eleMag, int band)
          throws AreaFileException {
    return new AreaFile(fpath, startLine, numLines, lineMag, startElem,
                        numEles, eleMag, band);
  }

  /**
   * Copy an area file from one place to another
   * @param source  source file or ADDE url
   * @param outputFile  name of the output file
   * @throws AreaFileException on any error constructing the instance.
   * @throws AddeURLException If the source is a URL and the query string is
   *  not formatted correctly.
   */
  public static void copyAreaFile(String source, String outputFile)
          throws AddeURLException, AreaFileException {
    copyAreaFile(source, outputFile, false);
  }

  /**
   * Copy an area file from one place to another
   * @param source  source file or ADDE url
   * @param outputFile  name of the output file
   * @param verbose  true to print out status messages
   * @throws AreaFileException on any error constructing the instance.
   * @throws AddeURLException If the source is a URL and the query string is
   *  not formatted correctly.
   */
  public static void copyAreaFile(String source, String outputFile,
                                  boolean verbose)
          throws AddeURLException, AreaFileException {
    AreaFile area = getAreaFileInstance(source);
    area.save(outputFile, verbose);
  }
}

