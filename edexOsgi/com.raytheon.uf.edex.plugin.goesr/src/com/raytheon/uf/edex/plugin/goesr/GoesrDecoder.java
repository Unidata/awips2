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
package com.raytheon.uf.edex.plugin.goesr;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import javax.measure.unit.SI;
import javax.xml.bind.JAXB;

import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.edex.plugin.goesr.decoder.lookup.ProductDescription;
import com.raytheon.uf.edex.plugin.goesr.decoder.lookup.ProductDescriptions;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrDecoderException;
import com.raytheon.uf.edex.plugin.goesr.geospatial.GoesrProjectionFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.GoesrSatelliteHeight;

/**
 * This decoder attempts to open a potential GOES-R netCDF file, decode the data
 * contained in it, and make it available to be stored.
 * 
 * <pre>
 * The code implements the 
 * Ground Segment (GS) to Advanced Weather Interactive Processing System (AWIPS)
 * Interface Control Document (ICD)
 * DOCUMENT CONTROL NUMBER: 7034704 CDRL SE-08 REVISION B
 * Date 31 MAY 2012
 * *****
 * Some variances between the code and the revision are noted. These are
 * due to discrepancies between the ICD and the sample data. These have
 * been reported.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 31, 2012  796      jkorman     Initial creation
 * Jul  5, 2013  2123     mschenke    Changed to use in-memory netcdf object
 * Feb 13, 2015  4043     bsteffen    Include scene number in sector.
 * Apr 17, 2015  4336     bsteffen    Rewrite to be configurable for other attribute conventions.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class GoesrDecoder {

    private static final transient Logger logger = LoggerFactory
            .getLogger(GoesrDecoder.class);

    private GoesrProjectionFactory projectionFactory;

    private ProductDescriptions descriptions;

    public GoesrDecoder() {

    }

    /**
     * Decode raw bytes into Satellite Records
     * 
     * @param name
     *            he name of the dataset, typically the filename, may be null in
     *            which case a UUID is used.
     * @param goesrData
     *            bytes must be in netcdf format with no additional headers.
     * @return Satellite Records.
     */
    public SatelliteRecord[] decode(String name, byte[] goesrData) {
        if (projectionFactory == null) {
            logger.error("Cannot decode goesr data because no projection factory is available.");
            return new SatelliteRecord[0];
        } else if (descriptions == null) {
            logger.error("Cannot decode goesr data because no descriptions were loaded.");
        }

        if (name == null) {
            /*
             * Name is not used by anything in this plugin but is needed to
             * construct the NetcdfFile
             */
            name = UUID.randomUUID().toString();
        }

        NetcdfFile cdfFile = null;
        try {
            cdfFile = NetcdfFile.openInMemory(name, goesrData);
            List<SatelliteRecord> records = decodeFile(cdfFile);
            for (Iterator<SatelliteRecord> it = records.iterator(); it
                    .hasNext();) {
                SatelliteRecord record = it.next();
                if (record.getDataTime() == null) {
                    logger.error(
                            "Discarding Record due to missing DataTime: %",
                            record);
                    it.remove();
                } else if (record.getPhysicalElement() == null) {
                    logger.error(
                            "Discarding Record due to missing physical element: %",
                            record);
                    it.remove();
                } else if (record.getSectorID() == null) {
                    logger.error(
                            "Discarding Record due to missing sector ID: %",
                            record);
                    it.remove();
                }
                IDataRecord dataRecord = (IDataRecord) record.getMessageData();
                dataRecord.setGroup(record.getDataURI());
            }

            return records.toArray(new SatelliteRecord[0]);
        } catch (Throwable t) {
            logger.error("Uncaught error in decoder ", t);
        } finally {
            if (cdfFile != null) {
                try {
                    cdfFile.close();
                } catch (IOException e) {
                    logger.error("Unable to close goes-r netcdf file.", e);
                }
            }
        }
        return new SatelliteRecord[0];
    }

    /**
     * Decode the GOES-R data and return the data in a SatelliteRecord. If an
     * error occurs or the decoder is unable to decode the data, a null
     * reference may be returned.
     * 
     * @return The decoded GOES-R satellite data.
     */
    public List<SatelliteRecord> decodeFile(NetcdfFile cdfFile) {
        List<SatelliteRecord> records = new ArrayList<>(1);
        List<ProductDescription> descriptions = new ArrayList<>();
        for (ProductDescription description : this.descriptions
                .getDescriptions()) {
            try {
                if (description.match(cdfFile)) {
                    if (description.hasData()) {
                        records.addAll(description.getData(cdfFile,
                                projectionFactory));
                    } else {
                        descriptions.add(description);
                    }
                }
            } catch (GoesrDecoderException e) {
                logger.error("Could not create GOES-R data from description", e);
            }
        }
        if (records.isEmpty()) {
            logger.warn("No GOES-R records were found in {}",
                    cdfFile.getLocation());
        }
        for (SatelliteRecord record : records) {
            for (ProductDescription description : descriptions) {
                try {
                    description.describe(record, cdfFile);
                } catch (GoesrDecoderException e) {
                    logger.error("Could not create describe GOES-R data", e);
                }
            }
            if (record.getSatHeight() == null) {
                /*
                 * Some data formats, which are in a geostationary projection,
                 * do not define that satellite height, except in the projection
                 * metadata.
                 */
                CoordinateReferenceSystem crs = record.getCoverage().getCrs();
                double satHeight = GoesrSatelliteHeight.getOrbitalHeight(crs,
                        SI.KILOMETER);
                if (!Double.isNaN(satHeight)) {
                    record.setSatHeight((int) satHeight);
                }
            }
        }
        return records;
    }

    public void setProjectionFactory(GoesrProjectionFactory projectionFactory) {
        this.projectionFactory = projectionFactory;
    }

    /**
     * The {@link IPathManager} is used to look up description files.
     */
    public void setPathManager(IPathManager pathManager) {
        LocalizationFile[] files = pathManager.listStaticFiles(
                "satellite/goesr/descriptions/", new String[] { ".xml" }, true,
                true);
        ProductDescriptions descriptions = new ProductDescriptions();
        for (LocalizationFile file : files) {
            logger.info("Loading goesr data description from " + file.getName());
            try (InputStream inputStream = file.openInputStream()) {
                ProductDescriptions unmarshalled = JAXB.unmarshal(inputStream,
                        ProductDescriptions.class);
                descriptions.addDescriptions(unmarshalled);
            } catch (LocalizationException | IOException e) {
                logger.error(
                        "Unable to load product descriptions from "
                                + file.getName(), e);
            }
        }
        this.descriptions = descriptions;
    }

}
