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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

import java.nio.ByteBuffer;
import java.nio.file.Path;

import com.raytheon.uf.common.dataplugin.shef.tables.Dhradapt;
import com.raytheon.uf.common.dataplugin.shef.tables.DhradaptId;
import com.raytheon.uf.common.dataplugin.shef.tables.Dhrradar;
import com.raytheon.uf.common.dataplugin.shef.tables.DhrradarId;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.InvalidMpeRadarException;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarFile;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarInputException;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarSymbologyData;

/**
 * Representation of the DHR Radar file that provides a way to read DHR Radar
 * files. Write is not currently (June 2016) implemented because the files are
 * currently only read and converted to a different format before the
 * information is rewritten. Structure of the DHR Radar file is based on:
 * decode_dhr_dsp/TEXT/decodeDHR.c.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4625       bkowal      Initial creation
 * Nov 22, 2016 5588       nabowle     Refactor
 *
 * </pre>
 *
 * @author bkowal
 */

public class DHRRadarFile extends MpeRadarFile<DHRProductDescription> {

    public static final short DHR_PRODUCT_CODE = 32;

    public static final String DHR_PRODUCT_TYPE = "DHR";

    private Dhrradar radar;

    private Dhradapt adapt;

    private DHRProductDescription description;

    /**
     * @param radarFilePath
     * @param version
     * @throws MpeRadarInputException
     */
    public DHRRadarFile(Path radarFilePath, int version)
            throws MpeRadarInputException {
        super(radarFilePath, version, DHR_PRODUCT_TYPE, DHR_PRODUCT_CODE);
    }

    @Override
    protected void readDescription(ByteBuffer buf)
            throws InvalidMpeRadarException {
        setDescription(new DHRProductDescription(buf));
    }

    /**
     *
     */
    @Override
    protected void initialize() {
        DHRProductDescription desc = getDescription();
        MpeRadarSymbologyData symbolData = getSymbologyData();
        float[] params = symbolData.getAdapParams();

        DhrradarId radarId = new DhrradarId(getRadarId(),
                getDescription().getProductDateTime().getTime());
        radar = new Dhrradar();
        radar.setId(radarId);
        radar.setGridFilename(getGridFilename());
        radar.setIdentifier(radarId);

        radar.setDbzcnt((float) desc.getDataLevelCount());
        radar.setDbzinc(desc.getDataLevelInc());
        radar.setDbzmin(desc.getDbzMin());
        radar.setJDate(desc.getjDate());
        radar.setJTime(desc.getjTime());
        radar.setMeanFieldBias(desc.getMeanFieldBias());
        radar.setOpermode(desc.getOperationalMode());
        radar.setSampleSize(desc.getSampleSize());
        radar.setVolcovpat(desc.getVolumeCoveragePattern());

        DhradaptId adaptId = new DhradaptId(getRadarId(),
                desc.getProductDateTime().getTime());
        adapt = new Dhradapt();
        adapt.setId(adaptId);
        adapt.setBiasApplied(symbolData.getBiasApplied());

        int i = 0;
        adapt.setMinReflth(params[i++]);
        adapt.setMaxReflth(params[i++]);
        adapt.setRefTltest(params[i++]);
        adapt.setRngTltin(params[i++]);
        adapt.setRngTltout(params[i++]);
        adapt.setMaxBirng(params[i++]);
        adapt.setMinEchoar(params[i++]);
        adapt.setMinAwrefl(params[i++]);
        adapt.setMaxPctred(params[i++]);
        adapt.setMltZrcoef(params[i++]);
        adapt.setPwrZrcoef(params[i++]);
        adapt.setMinZrefl(params[i++]);
        adapt.setMaxZrefl(params[i++]);
        adapt.setMinBirng(params[i++]);

        if (getVersion() == MpeRadarFile.BUILD_VERSION_5) {
            adapt.setMaxStmspd(params[i++]);
            adapt.setMaxTimdif(params[i++]);
            adapt.setMinArtcon(params[i++]);
            adapt.setTimP1cont(params[i++]);
            adapt.setTimP2cont(params[i++]);
            adapt.setMaxEcarch(params[i++]);
        } else {
            /*
             * The following 6 parameters have been removed from ORPG build 8.
             * set them to default missing value.
             */
            adapt.setMaxStmspd(MpeRadarFile.DEFAULT_PARAM_VALUE);
            adapt.setMaxTimdif(MpeRadarFile.DEFAULT_PARAM_VALUE);
            adapt.setMinArtcon(MpeRadarFile.DEFAULT_PARAM_VALUE);
            adapt.setTimP1cont(MpeRadarFile.DEFAULT_PARAM_VALUE);
            adapt.setTimP2cont(MpeRadarFile.DEFAULT_PARAM_VALUE);
            adapt.setMaxEcarch(MpeRadarFile.DEFAULT_PARAM_VALUE);
        }

        adapt.setRngCutoff(params[i++]);
        adapt.setRngE1coef(params[i++]);
        adapt.setRngE2coef(params[i++]);
        adapt.setRngE3coef(params[i++]);
        adapt.setMinPrate(params[i++]);
        adapt.setMaxPrate(params[i++]);
        adapt.setTimRestrt(params[i++]);
        adapt.setMaxTimint(params[i++]);
        adapt.setMinTimprd(params[i++]);
        adapt.setThrHlyout(params[i++]);
        adapt.setEndTimgag(params[i++]);
        adapt.setMaxPrdval(params[i++]);
        adapt.setMaxHlyval(params[i++]);
        adapt.setTimBiest(params[i++]);
        adapt.setThrNosets(params[i++]);
        adapt.setResBias(params[i++]);
        adapt.setLongestLag(params[i++]);
    }

    /**
     * @return the radar
     */
    public Dhrradar getRadar() {
        return radar;
    }

    /**
     * @param radar
     *            the radar to set
     */
    public void setRadar(Dhrradar radar) {
        this.radar = radar;
    }

    /**
     * @return the adapt
     */
    public Dhradapt getAdapt() {
        return adapt;
    }

    /**
     * @param adapt
     *            the adapt to set
     */
    public void setAdapt(Dhradapt adapt) {
        this.adapt = adapt;
    }

    /**
     * @return the description
     */
    @Override
    public DHRProductDescription getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(DHRProductDescription description) {
        this.description = description;
    }

}
