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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.commons.collections.CollectionUtils;

import com.raytheon.uf.edex.plugin.mpe.biasmesgen.legacy.BiasmesgenLegacyFunctionsUtil;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.AdminDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarlocDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarrespDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RwbiasdynDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RwbiasstatDao;
import com.raytheon.uf.common.dataplugin.shef.tables.Admin;
import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;
import com.raytheon.uf.common.dataplugin.shef.tables.Radarresp;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasdyn;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Java implementation of biasmesgen_main. Biasmesgen will be triggered by the
 * mpe cron (for now) because it was originally executed by the overall
 * run_mpe_fieldgen script. Output produced by biasmesgen is at a minimum read
 * and used by: com.raytheon.edex.rpgenvdata.RPGEnvironmentalDataManager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2016 5576       bkowal      Initial creation
 * May 19, 2016 5576       bkowal      DAOs are now created on-demand
 * May 20, 2016 5576       bkowal      Implemented bias table output generation.
 * Jun 13, 2016 5576       bkowal      Implemented bias table write and debug output.
 * Jun 15, 2016 5576       bkowal      Bias table generation starts at the current hour.
 * Jun 16, 2016 5576       bkowal      Fixed julian time and memory span calculations.
 * Jul 12, 2016 4619       bkowal      Moved {@link AppsDefaultsConversionWrapper} to common.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class Biasmesgen {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    public void execute() {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        logger.info("Biasmesgen has started ...");
        try {
            BiasmesgenInputs inputs = gatherInputs();
            if (inputs.generationPossible()) {
                produceOutputs(inputs);
            }
        } catch (Exception e) {
            timer.stop();
            logger.error(
                    "Biasmesgen has failed to finish in "
                            + TimeUtil.prettyDuration(timer.getElapsedTime())
                            + ".", e);
        }
        timer.stop();
        logger.info("Biasmesgen has successfully finished in {}.",
                TimeUtil.prettyDuration(timer.getElapsedTime()));
    }

    private BiasmesgenInputs gatherInputs() throws Exception {
        final Admin admin = new AdminDao().getAdminRecord();
        final String siteId = admin.getHsa();
        BiasmesgenInputs inputs = new BiasmesgenInputs();
        logger.debug("AWIPS Site ID: {}", siteId);
        inputs.setSiteId(siteId);
        logger.debug("AWIPS Site ID Number: {}", admin.getHsaNum());
        inputs.setSiteIdNumber(admin.getHsaNum());

        /*
         * Retrieve the bias message output directory
         */
        final Path biasOutputRoot = AppsDefaultsConversionWrapper
                .getPathForToken(BiasmesgenConstants.AppsDefaults.BIAS_MSG_DIR);
        logger.debug("Bias Output Root Directory: {}",
                biasOutputRoot.toString());
        inputs.setBiasOutputRoot(biasOutputRoot);
        final String fxaLocalSite = AppsDefaults.getInstance().getToken(
                BiasmesgenConstants.AppsDefaults.FXA_LOCAL_SITE);
        if (fxaLocalSite == null) {
            throw new BiasmesgenFailedException(
                    "Failed to retrieve the FXA Local Site from "
                            + AppsDefaults.NAME + ". Please verify that the '"
                            + BiasmesgenConstants.AppsDefaults.FXA_LOCAL_SITE
                            + "' property has been defined.");
        }
        inputs.setFxaLocalSite(fxaLocalSite);
        logger.debug("FXA Local Site: {}", fxaLocalSite);

        Boolean sendLocalBias = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean(BiasmesgenConstants.AppsDefaults.SEND_LOCAL_BIAS_WHEN_RFC_BIAS_MISSING);
        if (sendLocalBias == null) {
            logger.debug("send_local_bias_when_rfc_bias_missing token is not\n"
                    + "defined. Will not send locally-generated mean field\n"
                    + "bias when the rfc mean field bias cannot be found.");
        } else if (Boolean.TRUE.equals(sendLocalBias)) {
            logger.debug("Will substitute local mean field bias when the rfc\n"
                    + "mean field bias cannot be found.");
        } else {
            logger.debug("Will not substitute the local mean field bias when\n"
                    + "the rfc mean field bias cannot be found.");
        }
        inputs.setSendLocalBias(sendLocalBias);

        /*
         * Retrieve the memory span values from the rwbiasstat table. Several
         * tables in the ihfs database could probably be combined because they
         * generally only contain one site-specific row. This would allow for a
         * core ihfs database and site-specific ihfs databases with a
         * significantly fewer number of tables.
         */
        Rwbiasstat rwbiasstat = new RwbiasstatDao().getForOffice(fxaLocalSite);
        if (rwbiasstat == null) {
            throw new BiasmesgenFailedException(
                    "Failed to retrieve the rwbiasstat memory span values for site: "
                            + fxaLocalSite
                            + ". Please verify the contents of your ihfs database.");
        }
        logger.info("Retrieved Rwbiasstat: {} for Site: {}",
                rwbiasstat.toString(), fxaLocalSite);
        inputs.setRwbiasstat(rwbiasstat);

        List<Radarresp> radarrespList = new RadarrespDao().getForSiteId(siteId);
        logger.debug("Retrieved {} Radarresp record(s) for Site: {}",
                radarrespList.size(), siteId);

        /*
         * Retrieve the Radar Number and the Office Id (Radar Bias Source) for
         * all Radarresp from the radarloc table.
         */
        final Map<String, Radarresp> radIdToRespMap = new HashMap<>(
                radarrespList.size(), 1.0f);
        Iterator<Radarresp> iterator = radarrespList.iterator();
        while (iterator.hasNext()) {
            Radarresp next = iterator.next();
            radIdToRespMap.put(next.getRadid(), next);
        }

        List<Radarloc> radarlocList = new RadarlocDao()
                .getNumAndOfficeForRadarIds(radIdToRespMap.keySet());
        if (radarlocList.isEmpty()) {
            radarrespList.clear();
            inputs.setRadarrespCollection(radarrespList);
        } else {
            Map<String, Radarloc> radIdLocMap = new HashMap<>(
                    radarlocList.size(), 1.0f);
            for (Radarloc radarloc : radarlocList) {
                radIdLocMap.put(radarloc.getRadid(), radarloc);
            }

            /*
             * Determine if a radar location could not be found for any of the
             * radars. TODO: updated in a newer version of Apache Commons
             * Collections, so Strings will actually be returned by the subtract
             * method.
             */
            final Collection<?> notFoundElements = CollectionUtils.subtract(
                    radIdToRespMap.keySet(), radIdLocMap.keySet());
            if (!notFoundElements.isEmpty()) {
                for (Object notFoundElement : notFoundElements) {
                    Radarresp radarresp = radIdToRespMap
                            .remove((String) notFoundElement);
                    if (radarresp != null) {
                        logger.info(
                                "Skipping Radar: {}. Unable to find location information.",
                                radarresp.toString());
                    }
                }
            }

            inputs.setRadarrespCollection(radIdToRespMap.values());
            inputs.setRadIdLocMap(radIdLocMap);
        }

        return inputs;
    }

    private void produceOutputs(final BiasmesgenInputs inputs) throws Exception {
        Calendar generationDateTime = TimeUtil.newGmtCalendar();

        /*
         * If multiple radars exist, the bias date/time will be required
         * multiple times; so, calculate it prior to the loop.
         */
        final short biasJulianDate = (short) BiasmesgenLegacyFunctionsUtil
                .calculateBiasJulianDate(generationDateTime);
        final int biasTime = BiasmesgenLegacyFunctionsUtil
                .biasSecondsAfterMidnight(generationDateTime);

        Calendar basedObservationDateTime = TimeUtil
                .newCalendar(generationDateTime);
        // zero minutes and seconds for the observation time.
        basedObservationDateTime = TimeUtil.minCalendarFields(
                basedObservationDateTime, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);

        RwbiasdynDao rwbiasdynDao = new RwbiasdynDao();
        for (Radarresp radarresp : inputs.getRadarrespCollection()) {
            /*
             * radarloc should never be null based on the retrieval verification
             * that has been implemented in gatherInputs().
             */
            Radarloc radarloc = inputs.getRadIdLocMap().get(
                    radarresp.getRadid());
            logger.info(
                    "Processing Radar: {} with Radar Number: {}, Office Id: {} ...",
                    radarresp.toString(), radarloc.getRadarNum(),
                    radarloc.getOfficeId());

            /*
             * Iterates over the number of hour(s) to subtract from the
             * observation date/time.
             */
            for (int hour = 0; hour < BiasmesgenConstants.BIAS_GENERATION_HOURS; hour++) {
                Calendar observationDateTime = TimeUtil
                        .newCalendar(basedObservationDateTime);
                /*
                 * adjust time to the past based on the number of data
                 * processing hours.
                 */
                observationDateTime.add(Calendar.HOUR, -hour);
                List<Rwbiasdyn> rwbiasdynList = retrieveRwbiasdynForRadarOfficeAndTime(
                        rwbiasdynDao, radarresp, radarloc.getOfficeId(),
                        observationDateTime);
                if (rwbiasdynList.isEmpty()) {
                    /*
                     * Determine if the local mean field bias should be used -
                     * dependent on whether or not the
                     * send_local_bias_when_rfc_bias_missing Apps_Defaults flag
                     * has been set to "true".
                     */
                    boolean shouldSubstitute = Boolean.TRUE.equals(inputs
                            .getSendLocalBias())
                            && inputs.getFxaLocalSite() != null
                            && !inputs.getFxaLocalSite().equals(
                                    radarloc.getOfficeId());
                    if (shouldSubstitute) {
                        logger.info(
                                "Invoking local mean field bias substitution for local site: {}.",
                                inputs.getFxaLocalSite());
                        rwbiasdynList = retrieveRwbiasdynForRadarOfficeAndTime(
                                rwbiasdynDao, radarresp,
                                inputs.getFxaLocalSite(), observationDateTime);
                    }

                    /*
                     * If no data has still been found at this point, the only
                     * option that is available is to continue.
                     */
                    if (rwbiasdynList.isEmpty()) {
                        continue;
                    }
                }

                /*
                 * Prepare the {@link BiasTableHeader}.
                 */
                BiasTableHeader header = new BiasTableHeader();
                header.setMessageDate(biasJulianDate);
                header.setMessageTime(biasTime);
                header.setSourceId(inputs.getSiteIdNumber());
                header.setDestinationId(radarloc.getRadarNum());

                /*
                 * Prepare the {@link BiasTableSubHeader}.
                 */
                BiasTableSubHeader subHeader = new BiasTableSubHeader();
                subHeader.setBiasSource(radarloc.getOfficeId());
                subHeader.setRadarId(radarloc.getRadid());

                /*
                 * Prepare the {@link BiasTableDate} for the observation
                 * date/time.
                 */
                BiasTableDate observationDate = new BiasTableDate(
                        observationDateTime);

                /*
                 * Prepare the {@link BiasTableDate} for the generation
                 * date/time.
                 */
                BiasTableDate generationDate = new BiasTableDate(
                        generationDateTime);

                /*
                 * Create a {@link BiasTableFile}.
                 */
                final BiasTableFile biasTableFile = new BiasTableFile();
                biasTableFile.setHeader(header);
                biasTableFile.setSubHeader(subHeader);
                biasTableFile.setObservationDate(observationDate);
                biasTableFile.setGenerationDate(generationDate);

                /*
                 * Initialize and populate {@link BiasTableRow}s for the data.
                 */
                for (Rwbiasdyn rwbiasdyn : rwbiasdynList) {
                    /*
                     * Note: this list should never be empty. The only way that
                     * this list would be empty would be if a Rwbiasstat was not
                     * found. However, this phase of processing would never be
                     * reached.
                     */
                    final List<Float> memorySpanList = inputs
                            .getMemorySpanList();

                    /*
                     * Verify that the memory span indicator is valid.
                     */
                    final short memspanInd = rwbiasdyn.getId().getMemspanInd();
                    // required range: 0 - 9
                    if (memspanInd < 0
                            || memspanInd >= BiasmesgenInputs.MEMORY_SPAN_COUNT) {
                        throw new BiasmesgenFailedException(
                                "An invalid memory span indicator value: "
                                        + memspanInd
                                        + " has been encountered in Rwbiasdyn record: "
                                        + rwbiasdyn.getId().toString() + ".");
                    }

                    final int memorySpan = BiasmesgenLegacyFunctionsUtil
                            .cnvrt1000(Math.log(memorySpanList.get(memspanInd)));
                    final int numPairs = BiasmesgenLegacyFunctionsUtil
                            .cnvrt1000(rwbiasdyn.getNumpairs());
                    final int sumGag = BiasmesgenLegacyFunctionsUtil
                            .cnvrt1000(rwbiasdyn.getSumgag());
                    final int sumRad = BiasmesgenLegacyFunctionsUtil
                            .cnvrt1000(rwbiasdyn.getSumrad());
                    final int bias = BiasmesgenLegacyFunctionsUtil
                            .cnvrt1000(rwbiasdyn.getBias());
                    biasTableFile.addRow(new BiasTableRow(memorySpan, numPairs,
                            sumGag, sumRad, bias));
                }

                final Path biasTablePath = determineBiasTableDestination(
                        inputs.getBiasOutputRoot(), generationDateTime, header,
                        subHeader);
                BiasTableWriteResult result = biasTableFile
                        .writeBiasTable(biasTablePath);
                logger.info("Wrote bias table file: {} with size: {}.",
                        result.getOutputPath(), result.getBytesWritten());
                /*
                 * Presently the file is written to a temporary location and
                 * then moved to a permanent location immediately afterwards.
                 * This is to ensure that the process that periodically checks
                 * for and reads Bias Table files does not attempt to read a
                 * file that is actively being written at the time. TODO: this
                 * logic can be removed when Camel is used to notify the
                 * receiving process that a Bias Table file has been written
                 * instead of the receiving process periodically checking for
                 * the existence of Bias Table files.
                 */
                try {
                    Files.move(result.getOutputPath(), biasTablePath,
                            StandardCopyOption.ATOMIC_MOVE);
                } catch (IOException e) {
                    throw new BiasmesgenFailedException(
                            "Failed to rename bias table file: "
                                    + result.getOutputPath().toString()
                                    + " to " + biasTablePath.toString() + ".",
                            e);
                }
                logger.info("Successfully renamed bias table file: {} -> {}.",
                        result.getOutputPath().toString(),
                        biasTablePath.toString());
                if (logger.isDebugEnabled()) {
                    logBiasTableDebug(biasTableFile, biasTablePath);
                }

                /*
                 * Only one file will actually be written for a particular
                 * radar. If a file was successfully written for the current
                 * hour, do not continue processing additional hours.
                 */
                break;
            }
        }
    }

    private List<Rwbiasdyn> retrieveRwbiasdynForRadarOfficeAndTime(
            final RwbiasdynDao rwbiasdynDao, final Radarresp radarresp,
            final String officeId, final Calendar observationDateTime) {
        logger.info(
                "Retreiving Rwbiasdyn record(s) for Radar: {}, Office: {}, Date/Time: {} ...",
                radarresp.toString(), officeId, observationDateTime.getTime()
                        .toString());
        List<Rwbiasdyn> rwbiasdynList = rwbiasdynDao.getForRadarAndObsTime(
                observationDateTime.getTime(), radarresp.getRadid(), officeId);
        logger.info(
                "Retrieved {} Rwbiasdyn record(s) for Radar: {}, Office: {}, Date/Time: {}.",
                rwbiasdynList.size(), radarresp.toString(), officeId,
                observationDateTime.getTime().toString());
        return rwbiasdynList;
    }

    /**
     * Determine the location that the bias table file should be written to
     * using the root bias path, the date and time that the file was generated
     * and other metadata defined in the bias header and bias sub-header.
     * 
     * @param biasOutputRoot
     *            the specified bias root {@link Path}
     * @param generationDateTime
     *            the date/time that the file was generated
     * @param header
     *            the specified {@link BiasTableHeader}
     * @param subHeader
     *            the specified {@link BiasTableSubHeader}
     * @return the full bias table file {@link Path}
     */
    private Path determineBiasTableDestination(Path biasOutputRoot,
            final Calendar generationDateTime, final BiasTableHeader header,
            final BiasTableSubHeader subHeader) {
        final String biasTableFileName = String.format(
                BiasmesgenConstants.BIAS_TABLE_FILE_NAME_FMT, header
                        .getDestinationId(), header.getSourceId(), header
                        .getMessageCode(), subHeader.getBlockId(),
                BiasmesgenConstants.biasTableSDF.format(generationDateTime
                        .getTime()));
        return biasOutputRoot.resolve(biasTableFileName);
    }

    private void logBiasTableDebug(final BiasTableFile biasTableFile,
            final Path biasTablePath) {
        logger.debug("BEGIN - Generated Bias Table File: {}",
                biasTablePath.toString());
        logger.debug("Bias Table Header = {}", biasTableFile.getHeader()
                .toString());
        logger.debug("Bias Table Sub Header = {}", biasTableFile.getSubHeader()
                .toString());
        logger.debug("Bias Table Observation Date = {}", biasTableFile
                .getObservationDate().toString());
        logger.debug("Bias Table Generation Date = {}", biasTableFile
                .getGenerationDate().toString());
        logger.debug("Bias Table Row Count = {}",
                biasTableFile.getNumberOfRows());
        if (CollectionUtils.isNotEmpty(biasTableFile.getBiasTable())) {
            int rowNumber = 0;
            for (BiasTableRow row : biasTableFile.getBiasTable()) {
                ++rowNumber;
                logger.debug("Bias Table Row: {} = {}", rowNumber,
                        row.toString());
            }
        }
        logger.debug("END - Generated Bias Table File: {}",
                biasTablePath.toString());
    }
}