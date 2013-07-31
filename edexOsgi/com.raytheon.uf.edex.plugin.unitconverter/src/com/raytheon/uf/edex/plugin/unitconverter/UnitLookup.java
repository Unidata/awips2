/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.unitconverter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

import javax.measure.unit.UnitFormat;

import ucar.units.NoSuchUnitException;
import ucar.units.PrefixDBException;
import ucar.units.SpecificationException;
import ucar.units.UnitDBException;
import ucar.units.UnitFormatManager;
import ucar.units.UnitParseException;
import ucar.units.UnitSystemException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2013            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */

public class UnitLookup {

    private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    protected static UnitLookup instance = null;

    protected Map<javax.measure.unit.Unit<?>, ucar.units.Unit> jsrToUcarMap = new HashMap<javax.measure.unit.Unit<?>, ucar.units.Unit>();

    protected Map<ucar.units.Unit, javax.measure.unit.Unit<?>> ucarToJsrMap = new HashMap<ucar.units.Unit, javax.measure.unit.Unit<?>>();

    protected UnitLookup() {
        init();
    }

    protected void init() {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext[] searchHierarchy = pm
                .getLocalSearchHierarchy(LocalizationType.EDEX_STATIC);

        LocalizationFile unitFile = null;

        for (LocalizationContext ctx : searchHierarchy) {
            LocalizationFile localizationFile = pm.getLocalizationFile(ctx,
                    "wxsrv" + IPathManager.SEPARATOR + "units"
                            + IPathManager.SEPARATOR + "unitMapping.tsv");

            if (localizationFile.exists() && unitFile == null) {
                unitFile = localizationFile;
            }
        }

        File file = unitFile.getFile();

        BufferedReader reader = null;

        try {
            if (file.exists() && file.canRead()) {
                reader = new BufferedReader(new FileReader(file));

                String line = reader.readLine();

                while (line != null) {
                    if (!line.startsWith("#") && !line.trim().isEmpty()) {
                        String[] unitStrings = line.split("\t");
                        String jsrUnitString = unitStrings[0];
                        String udunitString = unitStrings[1];

                        try {
                            if (jsrUnitString != null
                                    && !jsrUnitString.isEmpty()
                                    && udunitString != null
                                    && !udunitString.isEmpty()) {
                                javax.measure.unit.Unit<?> jsrUnit = (javax.measure.unit.Unit<?>) UnitFormat
                                        .getUCUMInstance().parseObject(
                                                jsrUnitString);
                                ucar.units.Unit udunit = UnitFormatManager
                                        .instance().parse(udunitString);

                                jsrToUcarMap.put(jsrUnit, udunit);
                                ucarToJsrMap.put(udunit, jsrUnit);
                            }
                        } catch (ParseException e) {
                            log.warn("Unable to parse javax unit string \""
                                    + jsrUnitString + "\"");
                        } catch (NoSuchUnitException e) {
                            log.warn("Unable to parse ucar unit string \""
                                    + udunitString + "\"");
                        } catch (UnitParseException e) {
                            log.warn("Unable to parse ucar unit string \""
                                    + udunitString + "\"");
                        } catch (SpecificationException e) {
                            log.warn("Unable to parse ucar unit string \""
                                    + udunitString + "\"");
                        } catch (UnitDBException e) {
                            log.warn("Unable to parse ucar unit string \""
                                    + udunitString + "\"");
                        } catch (PrefixDBException e) {
                            log.warn("Unable to parse ucar unit string \""
                                    + udunitString + "\"");
                        } catch (UnitSystemException e) {
                            log.warn("Unable to parse ucar unit string \""
                                    + udunitString + "\"");
                        }
                    }
                    line = reader.readLine();
                }
            } else {
                log.error("Unable to open unit mapping file "
                        + file.getAbsolutePath());
            }
        } catch (FileNotFoundException e1) {
            // file not found even after file.exists() check
            log.error(
                    "unit mapping file not found at " + file.getAbsolutePath(),
                    e1);
        } catch (IOException e1) {
            // failed to read file even after file.canRead() check
            log.error(
                    "Unable to read unit mapping file "
                            + file.getAbsolutePath(), e1);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                }
            }
        }
    }

    public static UnitLookup getInstance() {
        if (instance == null) {
            synchronized (UnitLookup.class) {
                if (instance == null) {
                    instance = new UnitLookup();
                }
            }
        }

        return instance;
    }

    /**
     * From a ucar unit return the jsr unit, or null if not found
     * 
     * @param ucarUnit
     * @return a jsr unit or null if not found
     */
    public javax.measure.unit.Unit<?> getJsrFromUcar(ucar.units.Unit ucarUnit) {
        return ucarToJsrMap.get(ucarUnit);
    }

    /**
     * From a jsr unit return the ucar unit or null if not found
     * 
     * @param jsrUnit
     * @return
     */
    public ucar.units.Unit getUcarFromJsr(javax.measure.unit.Unit<?> jsrUnit) {
        return jsrToUcarMap.get(jsrUnit);
    }

}
