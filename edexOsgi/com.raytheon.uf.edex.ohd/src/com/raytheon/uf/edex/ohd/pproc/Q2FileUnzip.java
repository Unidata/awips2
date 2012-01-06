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
package com.raytheon.uf.edex.ohd.pproc;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class Q2FileUnzip {

    String pathName = "";

    public Q2FileUnzip() {

    }

    public void unzip(String file) {
        String fname = file;

        File filename = null;
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        pathName = appsDefaults.getToken("mpe_qmosaic_dir");

        File pn = new File(pathName);
        if (pn.exists() == false) {
            pn.mkdir();
        }
        pn = null;
        FileInputStream fis;
        GZIPInputStream gis;
        BufferedOutputStream dest = null;
        int BUFFER = 8192;
        File tf = new File(fname);
        String tname = tf.getName();
        // rename the file from xmrg... to QMOSAIC
        tname = tname.replace("xmrg", "QMOSAIC");
        // remove the .gz from the output filename
        int in = tname.indexOf(".gz");
        if (in != -1) {
            tname = tname.substring(0, in);
        }
        // match date time in file name to change from MMddYYYYHHz to
        // YYYYMMddHHz standard format
        String q2pat = "(QMOSAIC)(\\d{2})(\\d{2})(\\d{4})(\\d{2}\\w)";
        Pattern q2p = Pattern.compile(q2pat);
        Matcher mt = q2p.matcher(tname);
        if (mt.matches() == true) {
            String foname = mt.group(1) + mt.group(4) + mt.group(2)
                    + mt.group(3) + mt.group(5);
            tname = foname;
            filename = new File(pathName + File.separatorChar + tname);
        }

        try {
            fis = new FileInputStream(fname);
            gis = new GZIPInputStream(new BufferedInputStream(fis));
            int count;
            byte data[] = new byte[BUFFER];
            // write the files to the disk
            FileOutputStream fos = new FileOutputStream(filename);
            dest = new BufferedOutputStream(fos, BUFFER);
            while ((count = gis.read(data, 0, BUFFER)) != -1) {
                dest.write(data, 0, count);
            }
            dest.flush();
            dest.close();
            gis.close();
            fis.close();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        String tname = "QMOSAIC1201200912z";
        String q2pat = "(QMOSAIC)(\\d{2})(\\d{2})(\\d{4})(\\d{2}\\w)";
        Pattern q2p = Pattern.compile(q2pat);
        Matcher mt = q2p.matcher(tname);
        System.out.println(mt.pattern());
        if (mt.matches() == true) {
            String fname = mt.group(1) + mt.group(4) + mt.group(2)
                    + mt.group(3) + mt.group(5);
            System.out.println("File name final: " + fname);
        }
    }
}
