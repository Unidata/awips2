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
package com.raytheon.uf.edex.plugin.mpe.gather.dsp;

import java.nio.ByteBuffer;
import java.nio.file.Path;

import com.raytheon.uf.common.dataplugin.shef.tables.Dspadapt;
import com.raytheon.uf.common.dataplugin.shef.tables.DspadaptId;
import com.raytheon.uf.common.dataplugin.shef.tables.Dspradar;
import com.raytheon.uf.common.dataplugin.shef.tables.DspradarId;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.InvalidMpeRadarException;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarFile;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarInputException;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarSymbologyData;

/**
 * Reads and represents a Digital Storm-Total Precipitation (DSP) Product.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 * Jul 24, 2018 5588       mapeters    Use MpeRadarFile.BUILD_VERSION_5 constant
 *
 * </pre>
 *
 * @author nabowle
 */

public class DSPRadarFile extends MpeRadarFile<DSPProductDescription> {

    public static final short DSP_PRODUCT_CODE = 138;

    public static final String DSP_PRODUCT_TYPE = "DSP";

    private Dspradar radar;

    private Dspadapt adapt;

    private DSPProductDescription description;

    /**
     * @param radarFilePath
     * @param version
     * @throws MpeRadarInputException
     */
    public DSPRadarFile(Path radarFilePath, int version)
            throws MpeRadarInputException {
        super(radarFilePath, version, DSP_PRODUCT_TYPE, DSP_PRODUCT_CODE);
    }

    @Override
    protected void readDescription(ByteBuffer buf)
            throws InvalidMpeRadarException {
        this.description = new DSPProductDescription(buf);
    }

    @Override
    protected void initialize() {
        DSPProductDescription desc = getDescription();
        MpeRadarSymbologyData symbolData = getSymbologyData();
        float[] params = symbolData.getAdapParams();

        DspradarId radarId = new DspradarId(getRadarId(),
                getDescription().getProductDateTime().getTime());
        radar = new Dspradar();
        radar.setId(radarId);
        radar.setGridFilename(getGridFilename());

        radar.setBeginTime(desc.getStartDateTime().getTime());
        radar.setEndTime(desc.getProductDateTime().getTime());
        radar.setJBegDate(desc.getJulianBeginDate());
        radar.setJBegTime(desc.getJulianBeginTime());
        radar.setJEndDate(desc.getjDate());
        radar.setJEndTime(desc.getjTime());
        radar.setMaxval(desc.getMaxPrecipValue());
        radar.setMeanFieldBias(desc.getMeanFieldBias());
        radar.setMinval((float) desc.getMinDataLevel());
        radar.setOpermode(desc.getOperationalMode());
        radar.setSampleSize(desc.getSampleSize());
        radar.setVolcovpat(desc.getVolumeCoveragePattern());

        DspadaptId adaptId = new DspadaptId(getRadarId(),
                desc.getProductDateTime().getTime());
        adapt = new Dspadapt();
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
    public Dspradar getRadar() {
        return radar;
    }

    /**
     * @param radar
     *            the radar to set
     */
    public void setRadar(Dspradar radar) {
        this.radar = radar;
    }

    /**
     * @return the adapt
     */
    public Dspadapt getAdapt() {
        return adapt;
    }

    /**
     * @param adapt
     *            the adapt to set
     */
    public void setAdapt(Dspadapt adapt) {
        this.adapt = adapt;
    }

    /**
     * @return the description
     */
    @Override
    public DSPProductDescription getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(DSPProductDescription description) {
        this.description = description;
    }

}
