//
// GetAreaFile.java
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

import java.io.*;
import java.util.*;
import java.lang.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;
import edu.wisc.ssec.mcidas.*;
import edu.wisc.ssec.mcidas.adde.*;
import edu.wisc.ssec.mcidas.AreaFile;
import edu.wisc.ssec.mcidas.AreaFileException;

/** 
* Application to fetch an AREA file and write it as a local file.
*
* It is command-line driven, but can be forced into "gui mode" by
* either using the '-gui' option or specifying the URL 'adde://image?'
* as the source (although in the latter case, you still need to
* the desitnation filename on the command line; whereas with the -gui
* option, a FileChooser is presented to the user).
*
* @author Tom Whittaker, SSEC/CIMSS
*
*/
public class GetAreaFile implements ActionListener {
  final String[] paramNames = {"host", "group", "descr", "user", "proj", 
  "trace", "band","mag","linele","place","pos",
   "size","unit","spac","doc","latlon",
   "aux","time","day","cal","id" };

  String[] flags = {"h","g","d", "k","j","t",
   "b","m","l","n","p", 
   "s","u","z","o","r",
   "a","i","y","c","e"};

  String[] paramValues;
  String[] serverNames;
  String paramFile, outputFile;
  Properties pars;
  boolean verbose;
  boolean doGUI=false;
  GetAreaGUI gag = null;
  String gagRequest = null;

  /** AD_DATAOFFSET - byte offset to start of data block */
  public static final int AD_DATAOFFSET = 33;
  /** AD_NAVOFFSET - byte offset to start of navigation block */
  public static final int AD_NAVOFFSET  = 34;
  /** AD_AUXOFFSET - byte offset to start of auxilliary data section */
  public static final int AD_AUXOFFSET  = 59;
  /** AD_CALOFFSET - byte offset to start of calibration section */
  public static final int AD_CALOFFSET  = 62;
  /** AD_DATAWIDTH - number of bytes per data point */
  public static final int AD_DATAWIDTH  = 10;

  public static void main(String args[] ) {
    GetAreaFile gaf = new GetAreaFile(args);
  }

  public GetAreaFile(String args[]) {

    paramFile = "params.properties";
    verbose = false;

    // if no arguments, emit a "help" message
    if (args == null || args.length < 1) {
      System.out.println("Usage:  java edu.wisc.ssec.mcidas.GetAreaFile [options] output_file");
      System.out.println("   Command line [options] are:");
      for (int i=0; i<paramNames.length; i++) {
        System.out.println("    -"+flags[i]+" = "+paramNames[i]);
      }
      System.out.println("    -f = parameter save filename (def=params.properties)");
      System.out.println("    -v  (verbose text output)");
      System.out.println("    -gui = use GUI interface (no other options should be used with this)");
      System.out.println(" Note: for multi-argument options (like -s), you need to enclose the values in quotes. e.g., -s \"200 200\"");
      return;
    }

    paramValues = new String[paramNames.length];

    // first try to get all the command line parameters
    outputFile = doArguments(args);
    if (outputFile == null) {
      System.out.println("No output file specified...");
      return;
    }

    // now go for the properties file
    pars = fetchParams(paramFile);

    if (doGUI) {
      
     doGUI();
     // ug = new UseGUI(this, pars);
    } else {
      doRequest(pars);
    }


  }

  public void actionPerformed(ActionEvent e) {
    System.out.println("got action performed...");
    if (!e.getActionCommand().startsWith("adde://")) return;
    Object source = e.getSource();
    if (source.equals(gag)) {
      Properties p = new Properties();
      putProp(p,"host",gag.getServer());
      putProp(p,"group",gag.getGroup());
      putProp(p,"descr",gag.getDescr());
      putProp(p,"user",gag.getUserName());
      putProp(p,"proj",gag.getProjectNumber());
      putProp(p,"trace",null);
      putProp(p,"band",gag.getBand());
      putProp(p,"mag",gag.getMag());
      String ctype = gag.getCoordType();

      if (ctype.equals("E")) {
        putProp(p,"latlon",gag.getLocationString());
      } else if (ctype.equals("I")) {
        putProp(p,"linele",gag.getLocationString());
      } else if (ctype.equals("S")) {
        putProp(p,"id",gag.getLocationString());
      }

      putProp(p,"time",gag.getTime());
      putProp(p,"day",gag.getDay());

      putProp(p,"size",gag.getNumLinesEles());

      putProp(p,"unit",gag.getUnit());
      putProp(p,"doc",gag.getDoc());

      gagRequest = e.getActionCommand();
      JFileChooser getout = new JFileChooser();
      getout.setDialogTitle("Name of AREA file to write into");
      int status = getout.showSaveDialog(gag);
      if (status == JFileChooser.APPROVE_OPTION) {
        File fn = getout.getSelectedFile();
        putProp(p,"outfile",fn.toString()); // temporary...
        doRequest(p);
      } else {
        gag.status("File not saved!!!");
      }
    return;
    }

  }

  void putProp(Properties p, String name, String value) {
    if (value == null) return;
    if (value.trim().length() < 1) return;
    p.put(name,value);
    return;
  }

  
  private void doGUI() {
    String thost, tgroup, tdescr;
    String tpos, tday, ttime;
    String tlatlon, tlinele;
    String tsize, tmag, tspac;
    String tband, tunit;
    String taux, tcal, tid, tdoc;
    String ttrace, tuser, tproj;
    gag = new GetAreaGUI("Select File", false);
    gag.addActionListener(this);
    gag.show();
    tuser = pars.getProperty("user");
    if (tuser != null) gag.setUserName(tuser);
    tproj = pars.getProperty("proj");
    if (tproj != null) gag.setProjectNumber(tproj);

    tpos = pars.getProperty("pos");
    //if (tpos != null) gag.setDatasetPosition(tpos);

    tday = pars.getProperty("day");
    ttime = pars.getProperty("time");

    tlatlon = pars.getProperty("latlon");
    tlinele = pars.getProperty("linele");
    tid = pars.getProperty("id");

    if (tlatlon != null) {
      gag.setCoordType("E");
      gag.setLocationString(tlatlon);
        
    } else if (tlinele != null) {
      gag.setCoordType("I");
      gag.setLocationString(tlinele);

    } else if (tid != null) {
      gag.setCoordType("S");
      gag.setLocationString(tid);
    }

    tsize = pars.getProperty("size");
    if (tsize != null) {
      gag.setNumLinesEles(tsize);
    } else {
      gag.setNumLinesEles("480 640");
    }

    tmag = pars.getProperty("mag");
    if (tmag != null) gag.setMag(tmag);

    tband = pars.getProperty("band");
    if (tband != null) gag.setBand(tband);

    tspac = pars.getProperty("spac");
    //if (tspac != null) gag.setNumBytes(tspac);

    tunit = pars.getProperty("unit");
    if (tunit != null) gag.setUnit(tunit);

    tcal = pars.getProperty("cal");
    //if (tcal != null) gag.setCal(tcal);

    tdoc = pars.getProperty("doc");
    if (tdoc != null) gag.setDoc(tdoc);

    taux = pars.getProperty("aux");
    //if (taux != null) gag.setAux(taux);
  }

  public void doRequest(Properties params) {

    String request = makeADDEString(params);
    if (gagRequest != null) request = gagRequest;

    if (gag != null) outputFile = params.getProperty("outfile");

    System.out.println("Request sent: "+request);
    if (gag != null) gag.status("Request sent: "+request);

    AreaFile af;
    
    try {
      af = new AreaFile(request);
    } catch (AreaFileException e) {
      System.out.println("While getting AreaFile:"+e);
      String es = e.toString();
      String es2 = es;
      int ei = es.lastIndexOf("Exception:");
      if (ei > 0) es = es2.substring(ei+11);
      if (gag != null) gag.status("Error: "+es);
      return;
    }
    int[] dir;

    dir=af.getDir();
    if (dir == null) {
      System.out.println("No AREA file directory!");
      return;
    }
    if (verbose) {
      System.out.println("Length of directory = "+dir.length);

      for (int i=0; i<dir.length; i++) {
       System.out.println(" index "+i+" = "+dir[i]);
      }
    }

    int[] nav=null;
    nav=af.getNav();
    if (nav == null) {
      System.out.println("No navigation block!");
    } else {
      if (verbose) System.out.println("Length of nav block = "+nav.length);
    }

    int[] cal=null;
    cal=af.getCal();
    if (cal == null) {
      System.out.println("No calibration block!");
    } else {
      if (verbose) System.out.println("Length of cal block = "+cal.length);
    }

    int[] aux=null;
    aux=af.getAux();
    if (aux == null) {
      System.out.println("No aux block");
    } else {
      if (verbose) System.out.println("Length of aux block = "+aux.length);
    }

    int NL=dir[8];
    int NE=dir[9];

    if (verbose) System.out.println("Start reading data, num points="+(NL*NE));
    if (gag != null) gag.status("Start reading data, num points="+(NL*NE));


    int[][]data;

    try { data = af.getData(0,0,NL,NE); }
    catch (AreaFileException e) {System.out.println(e);return;}

    if (verbose) System.out.println("Finished reading data");
    if (gag != null) gag.status("Finished reading data");


    try {
      RandomAccessFile raf = new RandomAccessFile(outputFile,"rw");

    if (verbose) System.out.println("Dir to word 0");
      raf.seek(0);
      dir[0] = 0; // make sure this is zero!!
      for (int i=0; i<dir.length; i++) raf.writeInt(dir[i]);

    if (verbose) System.out.println("Nav to word "+dir[AD_NAVOFFSET]);
      if (nav != null && dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_NAVOFFSET]);
        for (int i=0; i<nav.length; i++) raf.writeInt(nav[i]);
      }

    if (verbose) System.out.println("Cal to word "+dir[AD_CALOFFSET]);
      if (cal != null && dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_CALOFFSET]);
        for (int i=0; i<cal.length; i++) raf.writeInt(cal[i]);
      }

    if (verbose) System.out.println("Aux to word "+dir[AD_AUXOFFSET]);
      if (aux != null && dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_AUXOFFSET]);
        for (int i=0; i<aux.length; i++) raf.writeInt(aux[i]);
      }

    if (verbose) System.out.println("Data to word "+dir[AD_DATAOFFSET]);
      if (dir[AD_NAVOFFSET] > 0) {
        raf.seek(dir[AD_DATAOFFSET]);
        for (int i=0; i<data.length; i++) {
          for (int j=0; j<data[i].length; j++) {
            if (dir[AD_DATAWIDTH] == 1) {
              raf.writeByte(data[i][j]);
            } else if (dir[AD_DATAWIDTH] == 2) {
              raf.writeShort(data[i][j]);
            } else if (dir[AD_DATAWIDTH] == 4) {
              raf.writeInt(data[i][j]);
            }
          }
        }
      }

      raf.close();
    } catch (Exception we) {System.out.println(we);}

    System.out.println( "Completed. Data saved to: "+outputFile+
                               "   Saving parameters to: "+paramFile);
    if (gag != null) gag.status("Completed. Data saved to: "+outputFile+
                               "   Saving parameters to: "+paramFile);

    writeParams(paramFile,params);
  }


  /** make the ADDE request string out of the various parameters
  * the host, group and descr are required.  Everything else is
  * optional.  version=1 is appended...
  */
  public String makeADDEString(Properties params) {
    String host = params.getProperty("host");
    if (host == null) return null;

    String group = params.getProperty("group");
    if (group == null) return null;

    String descr = params.getProperty("descr");
    if (descr == null) return null;

    StringBuffer sb = new StringBuffer("adde://"+host+"/image?");
    for (int i=0; i<paramNames.length; i++) {
      if (paramNames[i] != "host" && params.getProperty(paramNames[i]) != null) {
        sb.append(paramNames[i]+"="+params.getProperty(paramNames[i])+"&");
      }
    }
    sb.append("version=1");
    return sb.toString();
  }

  /** scans the input argument list and if it finds any legitimate
  * flags, it replaces the value of the parameter
  */
  String doArguments(String[] arg) {

    String outfile = arg[arg.length - 1];

    // if there is only one parameter, see if it's the '-gui' switch
    // instead of the 'outfile' name
    if (outfile.equalsIgnoreCase("-gui")) {
      doGUI = true;
      outfile = " ";
      return outfile;
    }

    for (int k=0; k<arg.length-1; k++) {
      String s = arg[k];
      if ((s.length()) > 1 && s.startsWith("-")) {
        if (s.equalsIgnoreCase("-gui")) {
          doGUI = true;
          continue;
        }

        String r = s.substring(1,2);
        if (r.equals("f")) {
          if (s.length() == 2) {
            paramFile = arg[++k];
          } else {
            paramFile = s.substring(2);
          }

        } else if (r.equals("v")) {
          verbose = true;

        } else {
          for (int i=0; i<paramNames.length; i++) {
            if (r.equals(flags[i])) {
              if (s.length() == 2) {
                paramValues[i] = arg[++k];
              } else {
                paramValues[i] = s.substring(2);
              }
            }
          }
        }
      } else {
        System.out.println("Problem with parameter: "+s);
        return null;
      }

    }
    return outfile;
  }


  /** fetch the parameters from the property file
  */
  Properties fetchParams(String filename) {
    
    Properties p = new Properties();
    try {

      InputStream is = new FileInputStream(filename);
      p.load(is);
      is.close();
    } catch (Exception e) {System.out.println(e);}

    for (int i=0; i<paramNames.length; i++) {
      if (paramValues[i] != null) p.put(paramNames[i], paramValues[i]);
    }
    //if (outputFile != null && outputFile.length>1) p.put("outfile",outputFile); 
    return p;

  }


  /** write the property file back out...
  */
  void writeParams(String filename, Properties p) {
    try {
      OutputStream os = new FileOutputStream(filename);
      p.save(os,"GetAreaFile properties");
      os.flush();
      os.close();
    } catch (Exception e) {System.out.println(e);}
  }

}


/* 
 *-g   group=<groupname>         ADDE group name
 *-k   user=<user_id>            ADDE user identification
 *-j   proj=<proj #>             a valid ADDE project number
 *-t   trace=<0/1>               setting to 1 tells server to write debug
 *                               trace file (imagedata, imagedirectory)
 *     version=1                 ADDE version number, currently 1
 *
 *-d   descr=<descriptor>        ADDE descriptor name
 *-b   band=<band>               spectral band or channel number
 *-m   mag=<lmag> <emag>         image magnification, postitive for blowup,
 *                               negative for blowdown (default = 1, emag=lmag)
 *                               (imagedata only)
 *-l   linele=<lin> <ele> <type> line/element to center image on 
 *-n   place=<placement>         placement of lat/lon or linele points (center
 *                               or upperleft (def=center)) 
 *-p   pos=<position>            request an absolute or relative ADDE position
 *                               number
 *-s   size=<lines> <elements>   size of image to be returned (imagedata only)
 *-u   unit=<unit>               to specify calibration units other than the
 *                               default
 *-z   spac=<bytes>              number of bytes per data point, 1, 2, or 4
 *                               (imagedata only)
 *-o   doc=<yes/no>              specify yes to include line documentation
 *                               with image (def=no)
 *-r   latlon=<lat> <lon>        lat/lon point to center image on 
 *-a   aux=<yes/no>              specify yes to include auxilliary information
 *                               with image for calibration purposes
 *-i   time=<time1> <time2>      specify the time range of images to select
 *                               (def=latest image if pos not specified)
 *-y   day=<day>                 specify the day of the images to select
 *                               (def=latest image if pos not specified)
 *-c   cal=<cal type>            request a specific calibration on the image
 *                               (imagedata only)
 *-i   id=<stn id>               radar station id
 *-h   host=                     ADDE server hostname or IP address
 *
*/

