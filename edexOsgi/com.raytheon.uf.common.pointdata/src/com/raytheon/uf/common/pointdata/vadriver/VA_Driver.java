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
package com.raytheon.uf.common.pointdata.vadriver;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Driver for using the VA_Advanced progressive disclosure to generate
 * localizaed spi files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 24, 2011           bfarmer     Initial creation
 * Dec 02, 2013  2537     bsteffen    Ensure streams are closed.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class VA_Driver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VA_Driver.class);

    Map<String, Integer> nameIndexMap = new HashMap<String, Integer>();

    int ns;

    private Integer MNS = 150000;

    Coordinate[] latLon = new Coordinate[MNS];

    int[] elevs = new int[MNS];

    int[] nums = new int[MNS];

    String[] nams = new String[MNS];

    String[] ids = new String[MNS];

    String[] namIdData;

    Integer[] goodness = new Integer[MNS];

    Double[] dist = new Double[MNS];

    int oorder;

    Map<String, Integer> namePositionMap = new HashMap<String, Integer>();

    int nn = 0;

    int max_name = 0;

    int stn_fmt = 0;

    int loc_fmt = 0;

    int pass = 0;

    int cities_file = 0;

    int verify = 0;

    int just_check = 0;

    int advanced = 0;

    int recomp = 0;

    float weight = 0;

    float maxDist = 3.4028235E38f;

    float minDist = 0;

    int uniqueFlag = 1;

    int filarg = 0;

    int use_aspect = 0;

    int dist_test = 0;

    int accum_good = 0;

    /*
     * defaults to 100 km
     */
    float dist_thresh = (float) (100. * 100. / (111. * 111.));

    public void vaStationsFile(File goodnessFile, File primary, File output) {
        String line;
        String[] splitLine;
        ns = 0;
        BufferedReader fis = null;
        BufferedReader pis = null;
        BufferedWriter fos = null;
        try {
            fis = new BufferedReader(new InputStreamReader(
                    new FileInputStream(goodnessFile)));
            for (line = fis.readLine(); line != null; line = fis.readLine()) {
                if (line.startsWith("#"))
                    continue;
                int comment = line.indexOf("//", 0);
                if (comment != -1) {
                    line = line.substring(0, comment);
                }
                splitLine = line.trim().split("\\s+");
                if (splitLine.length == 2) {
                    minDist = Float.valueOf(splitLine[0]);
                    maxDist = Float.valueOf(splitLine[1]);
                    latLon[ns].y = minDist; // Lat = Y
                    latLon[ns].x = maxDist; // Lon = X
                    continue;
                } else if (splitLine.length >= 6) {
                    nums[ns] = Integer.valueOf(splitLine[0]);
                    nams[ns] = splitLine[1];
                    latLon[ns] = new Coordinate(Float.valueOf(splitLine[3]),
                            Float.valueOf(splitLine[2]));
                    elevs[ns] = Integer.valueOf(splitLine[4]);
                    goodness[ns] = Float.valueOf(splitLine[5]).intValue();
                    if (splitLine.length >= 7) {
                        ids[ns] = splitLine[6];
                    } else {
                        ids[ns] = "";
                    }
                } else {
                    continue;
                }
                if ((latLon[ns].y == 0.0 && latLon[ns].x == 0.0)
                        || latLon[ns].y > 90.0 || latLon[ns].y < -90.0
                        || latLon[ns].x > 180.0 || latLon[ns].x < -180.0) {
                    continue;
                }
                if (splitLine.length <= 6 || ids[ns].equals("")) {
                    ids[ns] = nams[ns];
                }
                if (splitLine.length < 6) {
                    goodness[ns] = -1000 * (nums[ns] / 1000);
                } else {
                    dist[ns] = -1.0;// = goodness[ns].doubleValue();
                }
                nameIndexMap.put(ids[ns], ns);
                if (nams[ns].length() > max_name) {
                    max_name = nams[ns].length();
                }
                ++ns;
            }
            if (primary != null) {
                pis = new BufferedReader(new InputStreamReader(
                        new FileInputStream(primary)));
                int g = 0x7FFFFFFF;
                for (String pline = pis.readLine(); pline != null; pline = pis
                        .readLine()) {
                    pline = pline.trim();
                    Integer index = nameIndexMap.get(pline);
                    if (index != null) {
                        goodness[index] = g--;
                    }
                }
            }
            Coordinate[] latLonInput = new Coordinate[ns];
            Integer[] goodnessInput = new Integer[ns];
            Double[] distInput = new Double[ns];
            System.arraycopy(latLon, 0, latLonInput, 0, ns);
            System.arraycopy(goodness, 0, goodnessInput, 0, ns);
            System.arraycopy(dist, 0, distInput, 0, ns);
            VA_Advanced vaa = new VA_Advanced();
            vaa.setVaDistPass(true);
            vaa.setVaRecomp(false);
            vaa.setVaWeighting(weight);
            distInput = vaa
                    .getVaAdvanced(latLonInput, goodnessInput, distInput);
            // Write the output.
            fos = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(output)));
            String nameFormat = String.format("%s%ds", "%", max_name);
            for (int i = 0; i < ns; ++i) {
                fos.write(String.format("%5d ", nums[i]));
                fos.write(String.format(nameFormat, nams[i]));
                fos.write(String.format(" %8.4f %9.4f %5d %9.3f",
                        latLonInput[i].y, latLonInput[i].x, elevs[i],
                        distInput[i]));
                fos.newLine();
            }
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, "Could not read File ", e);

        } catch (IOException e) {
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException ioe) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "Error closing input file ["
                                    + goodnessFile.getName() + "]");
                }
            }
            if (pis != null) {
                try {
                    pis.close();
                } catch (IOException ioe) {
                    statusHandler.handle(
                            Priority.SIGNIFICANT,
                            "Error closing input file ["
                                    + primary.getName() + "]");
                }
            }
            if (fos != null) {
                try {
                    fos.close();
                } catch (IOException ioe) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "Error closing output file [" + output.getName()
                                    + "]");
                }
            }
        }

    }

    /**
     * @return the stn_fmt
     */
    public int getStn_fmt() {
        return stn_fmt;
    }

    /**
     * @param stn_fmt
     *            the stn_fmt to set
     */
    public void setStn_fmt(int stn_fmt) {
        this.stn_fmt = stn_fmt;
    }

    /**
     * @return the loc_fmt
     */
    public int getLoc_fmt() {
        return loc_fmt;
    }

    /**
     * @param loc_fmt
     *            the loc_fmt to set
     */
    public void setLoc_fmt(int loc_fmt) {
        this.loc_fmt = loc_fmt;
    }

    /**
     * @return the pass
     */
    public int getPass() {
        return pass;
    }

    /**
     * @param pass
     *            the pass to set
     */
    public void setPass(int pass) {
        this.pass = pass;
    }

    /**
     * @return the cities_file
     */
    public int getCities_file() {
        return cities_file;
    }

    /**
     * @param cities_file
     *            the cities_file to set
     */
    public void setCities_file(int cities_file) {
        this.cities_file = cities_file;
    }

    /**
     * @return the verify
     */
    public int getVerify() {
        return verify;
    }

    /**
     * @param verify
     *            the verify to set
     */
    public void setVerify(int verify) {
        this.verify = verify;
    }

    /**
     * @return the just_check
     */
    public int getJust_check() {
        return just_check;
    }

    /**
     * @param just_check
     *            the just_check to set
     */
    public void setJust_check(int just_check) {
        this.just_check = just_check;
    }

    /**
     * @return the advanced
     */
    public int getAdvanced() {
        return advanced;
    }

    /**
     * @param advanced
     *            the advanced to set
     */
    public void setAdvanced(int advanced) {
        this.advanced = advanced;
    }

    /**
     * @return the recomp
     */
    public int getRecomp() {
        return recomp;
    }

    /**
     * @param recomp
     *            the recomp to set
     */
    public void setRecomp(int recomp) {
        this.recomp = recomp;
    }

    /**
     * @return the weight
     */
    public float getWeight() {
        return weight;
    }

    /**
     * @param weight
     *            the weight to set
     */
    public void setWeight(float weight) {
        this.weight = weight;
    }

    /**
     * @return the uniqueFlag
     */
    public int getUniqueFlag() {
        return uniqueFlag;
    }

    /**
     * @param uniqueFlag
     *            the uniqueFlag to set
     */
    public void setUniqueFlag(int uniqueFlag) {
        this.uniqueFlag = uniqueFlag;
    }

    /**
     * @return the use_aspect
     */
    public int getUse_aspect() {
        return use_aspect;
    }

    /**
     * @param use_aspect
     *            the use_aspect to set
     */
    public void setUse_aspect(int use_aspect) {
        this.use_aspect = use_aspect;
    }

    /**
     * @return the dist_test
     */
    public int getDist_test() {
        return dist_test;
    }

    /**
     * @param dist_test
     *            the dist_test to set
     */
    public void setDist_test(int dist_test) {
        this.dist_test = dist_test;
    }

    /**
     * @return the accum_good
     */
    public int getAccum_good() {
        return accum_good;
    }

    /**
     * @param accum_good
     *            the accum_good to set
     */
    public void setAccum_good(int accum_good) {
        this.accum_good = accum_good;
    }

    /**
     * @return the dist_thresh
     */
    public float getDist_thresh() {
        return dist_thresh;
    }

    /**
     * @param dist_thresh
     *            the dist_thresh to set
     */
    public void setDist_thresh(float dist_thresh) {
        this.dist_thresh = dist_thresh;
    }

}
