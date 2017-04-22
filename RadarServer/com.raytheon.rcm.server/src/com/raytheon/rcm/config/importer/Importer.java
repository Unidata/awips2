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
package com.raytheon.rcm.config.importer;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collection;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.EndpointConfig;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.awips1.Awips1ConfigProvider;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil.Selector;
import com.raytheon.rcm.config.std.ConfigDoc;
import com.raytheon.rcm.config.std.StandardConfig;
import com.raytheon.rcm.config.std.StandardConfigProvider;
import com.raytheon.uf.common.util.RunProcess;

public class Importer {
    private static final String PROP_BASE = "com.raytheon.rcm";

    private static final String AWIPS1_PROP_BASE = "com.raytheon.rcm.awips1";

    String awips1Root;

    String rcmConfigDir;

    BufferedReader reader;

    private class Quit extends RuntimeException {

    }

    public static void main(String[] args) {
        System.exit((new Importer()).run(args));
    }

    public int run(String[] args) {
        reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            run2();
            return 0;
        } catch (Quit e) {
            System.out.println();
            return 1;
        }
    }

    private void run2() {
        System.out
                .println("NOTE: This utility should not be run while the AWIPS 2 RadarServer is running.");

        store(convert(gather()));
        maybeCopyRpsFles();
    }

    public Configuration gather() {
        awips1Root = prompt(
                "What is the root directory of the AWIPS 1 configuration?", "/");

        rcmConfigDir = prompt(
                "What is the configuration directory for the AWIPS 2 RadarServer?\n"
                        + "(Usually .../data/config in the RadarServer directory)\n",
                System.getProperty(PROP_BASE + ".configDir", ""));

        System.setProperty(AWIPS1_PROP_BASE + ".resourceRoot", awips1Root);

        String fxaLocalSite = System.getenv("FXA_LOCAL_SITE");
        if (fxaLocalSite == null) {
            File f = new File(awips1Root
                    + "/awips/fxa/data/localizationDataSets");
            File[] lcns = f.listFiles();
            if (lcns != null) {
                for (File lcn : lcns) {
                    if (lcn.isDirectory()
                            && (new File(lcn, "radarsInUse.txt")).isFile()) {
                        fxaLocalSite = lcn.getName();
                        break;
                    }
                }
            }
        }

        if (fxaLocalSite == null)
            fxaLocalSite = "";

        fxaLocalSite = prompt("What localization should be used?", fxaLocalSite);

        System.setProperty(AWIPS1_PROP_BASE + ".FXA_LOCAL_SITE", fxaLocalSite);

        Awips1ConfigProvider prov = new Awips1ConfigProvider();
        Configuration cfg = prov.getConfiguration();

        return cfg;
    }

    public ConfigDoc convert(Configuration source) {
        ConfigDoc doc = new ConfigDoc();
        // doc.collectionEnabled = source.isCollectionEnabled(); // Disable for
        // now
        doc.collectionEnabled = false;
        doc.decompressProducts = true; // Default for now...
        doc.pupID = source.getPupId();
        doc.regionCode = source.getRegionCode();
        doc.tdwrCollectionLimited = source.isTdwrCollectionLimited();
        doc.wmoSiteID = source.getWmoSiteID();
        Collection<String> radarIDs = source.getConfiguredRadarList();
        doc.radars = new RadarConfig[radarIDs.size()];
        int i = 0;
        for (String radarID : radarIDs)
            doc.radars[i++] = source.getConfigForRadar(radarID);

        /*
         * Create the endpoint configuration. The installer sets up some of its
         * fields, so we should use the existing configuration if possible.
         */
        EndpointConfig ec = null;
        try {
            StandardConfigProvider scp = new StandardConfigProvider();
            ec = ((StandardConfig) scp.getConfiguration()).getEndpointConfig();
        } catch (Exception e) {
            // nothing
        }
        // Create a fallback configuration if the if necessary.
        if (ec == null) {
            ec = new EndpointConfig();
            ec.setArchiveRoot("/data_store/radar");
            ec.setConnectionURL("amqp://guest:guest@/edex?brokerlist='tcp://edex-server:5672'");
        }
        doc.endpointConfig = ec;

        return doc;
    }

    public void store(ConfigDoc doc) {
        File dst = new File(rcmConfigDir + "/persist/config.xml");
        dst.getParentFile().mkdirs();
        Marshaller m = StandardConfigProvider.getMarshaller();
        try {
            m.marshal(doc, dst);
        } catch (JAXBException e) {
            System.err.println("Error saving configuration: " + e.getMessage());
        }
        System.out.println("New configuration written.");
    }

    public String prompt(String prompt, String defaultValue) {
        String result;
        // Console cns = System.console();
        // cns.format("%s [%s]\n", prompt, defaultValue);
        // String result = cns.readLine();
        System.out.format("%s [%s]\n", prompt, defaultValue);
        try {
            result = reader.readLine();
        } catch (IOException e) {
            throw new Quit();
        }

        System.out.println();
        if (result == null)
            throw new Quit();
        if (result.length() == 0)
            return defaultValue;
        else
            return result;
    }

    private void maybeCopyRpsFles() {
        File srcDir = new File(awips1Root + "/data/fxa/radar/lists");
        File dstDir = new File(rcmConfigDir, "drop-ins");
        try {
            String prompt = String.format(
                    "Copy local RPS files from\n  %s\nto\n  %s ?", srcDir,
                    dstDir);
            String v = prompt(prompt, "Y");
            if (v.equalsIgnoreCase("y") || Boolean.parseBoolean(v)) {
                // ok
            } else
                return;
        } catch (RuntimeException e) {
            return;
        }

        dstDir.mkdirs();

        int nFound = 0;

        File[] items = srcDir.listFiles();
        if (items != null) {
            for (File itm : items) {
                if (!itm.isFile())
                    continue;
                Selector sel = Awips1RpsListUtil.parseName(itm.getName());
                if (sel != null) {
                    ++nFound;
                    File dstFile = new File(dstDir, itm.getName());
                    String[] args = { "cp", "-p", itm.toString(),
                            dstFile.toString() };
                    try {
                        // DR#10955
                        RunProcess.getRunProcess().exec(args);
                    } catch (IOException e) {
                        System.err.format("Error: %s: %s\n", itm.getName(),
                                e.getMessage());
                        continue;
                    }
                    System.out.format("Copied %s\n", itm.getName());
                }
            }
        }

        if (nFound == 0)
            System.out.println("Did not find any RPS files");
    }

}
