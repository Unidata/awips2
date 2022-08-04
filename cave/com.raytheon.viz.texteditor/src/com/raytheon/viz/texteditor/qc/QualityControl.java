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
package com.raytheon.viz.texteditor.qc;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2011  10764      rferrel     Use QualityControlCfg.xml for
 *                                     configuable information.
 * Apr 29, 2013 3033       jsanchez    Updated method to retrieve files in localization.
 * Mar 23, 2013 4320       dgilling    Integrate WarningDecoderQCCheck.
 * Mar 10, 2016 5411       randerso    Added UnsubstitutedVariableCheck
 * Apr 10, 2016 6251       dgilling    Code cleanup and refactor.
 * May 18, 2016 6252       dgilling    Add support for additional checks.
 * </pre>
 *
 * @author rferrel
 * @version 1.0
 */
public class QualityControl {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QualityControl.class);

    private static final Map<String, IQCCheck> QC_CHECK_NAMES = new HashMap<String, IQCCheck>() {
        {
            put("wmo-header", new WmoHeaderCheck());
            put("unsubstituted-variable", new UnsubstitutedVariableCheck());
            put("mnd-header", new MndHeaderCheck());
            put("text-segment", new TextSegmentCheck());
            put("time-consistency", new TimeConsistentCheck());
            put("cta-marker", new CtaMarkerCheck());
            put("two-dollar", new TwoDollarCheck());
            put("warning-decoder", new WarningDecoderQCCheck());
        }
    };

    private static final QualityControl instance = new QualityControl();

    /** Maps warning PILs to appropriate warning text */
    private final Map<String, String> productTypeMap;

    private final Map<String, String> followupNNN;

    private final Map<String, String> nnnOfIdent;

    private final Collection<String> segmentedNNN;

    private final Collection<String> immediateCause;

    private final Map<String, List<String>> bulletTypeMap;

    private final Map<String, String> countyTypes;

    private final Map<String, List<IQCCheck>> productQcChecks;

    public static QualityControl getInstance() {
        return instance;
    }

    private QualityControl() {
        Optional<QualityControlCfg> optConfig = loadQualityControlCfg();
        if (optConfig.isPresent()) {
            QualityControlCfg qcConfig = optConfig.get();
            this.immediateCause = Arrays.asList(qcConfig.getImmediateCause());
            this.followupNNN = qcConfig.getFollowupNNN();
            this.nnnOfIdent = qcConfig.getNnnOfIdent();
            this.productTypeMap = qcConfig.getProductTypeMap();
            this.segmentedNNN = qcConfig.getSegmentedNNN();
            this.bulletTypeMap = qcConfig.getBulletTypeMap();
        } else {
            this.immediateCause = Collections.emptyList();
            this.followupNNN = Collections.emptyMap();
            this.nnnOfIdent = Collections.emptyMap();
            this.productTypeMap = Collections.emptyMap();
            this.segmentedNNN = Collections.emptyList();
            this.bulletTypeMap = Collections.emptyMap();
        }

        this.countyTypes = loadCountyTypes();

        this.productQcChecks = loadProductChecks();
    }

    private Map<String, String> loadCountyTypes() {
        try {
            String file = WarnFileUtil
                    .convertFileContentsToString("countyTypes.txt", null, null);
            Map<String, String> countyTypes = new HashMap<>();
            for (String line : file.split("\n")) {
                String[] parts = line.split("\\\\");
                String key = parts[0].trim();
                String value = parts.length < 2 || parts[1] == null ? ""
                        : parts[1];
                countyTypes.put(key, value);
            }
            return countyTypes;
        } catch (IOException e) {
            statusHandler.error("Could not read countyTypes.txt", e);
        }

        return Collections.emptyMap();
    }

    private Optional<QualityControlCfg> loadQualityControlCfg() {
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile file = pm
                .getStaticLocalizationFile(LocalizationUtil.join("textws",
                        "gui", "QualityControlCfg.xml"));

        if (file != null) {
            try (InputStream inStream = file.openInputStream()) {
                QualityControlCfg qcCfg = JAXB.unmarshal(inStream,
                        QualityControlCfg.class);
                return Optional.of(qcCfg);
            } catch (LocalizationException | IOException e) {
                statusHandler.error(
                        "Unable to read/load QualityControlCfg.xml for QC check.",
                        e);
            }
        } else {
            statusHandler.error(
                    "Unable to locate QualityControlCfg.xml for QC check.");
        }

        return Optional.empty();
    }

    private Map<String, List<IQCCheck>> loadProductChecks() {
        IPathManager pm = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> fileByLevels = pm
                .getTieredLocalizationFile(
                        LocalizationType.CAVE_STATIC,
                        LocalizationUtil.join("textws", "gui",
                                "QCProductChecks.properties"));

        if (!fileByLevels.isEmpty()) {
            Properties combinedProps = new Properties();

            for (LocalizationLevel level : pm.getAvailableLevels()) {
                ILocalizationFile file = fileByLevels.getOrDefault(level, null);
                if ((file != null) && (file.exists())) {
                    try (InputStream inStream = file.openInputStream()) {
                        combinedProps.load(inStream);
                    } catch (LocalizationException | IOException e) {
                        statusHandler.error(
                                "Unable to read/load [" + file
                                        + "] for QC check.",
                                e);
                    }
                }
            }

            Map<String, List<IQCCheck>> checksMap = new HashMap<>();
            for (Entry<Object, Object> entry : combinedProps.entrySet()) {
                String pil = entry.getKey().toString();
                String value = entry.getValue().toString();

                List<String> checkNames = Arrays.stream(value.split(","))
                        .map(String::trim).filter(name -> !name.isEmpty())
                        .collect(Collectors.toList());

                List<IQCCheck> checks = new ArrayList<>(checkNames.size());
                for (String checkName : checkNames) {
                    IQCCheck check = QC_CHECK_NAMES.getOrDefault(checkName,
                            null);
                    if (check != null) {
                        checks.add(check);
                    } else {
                        statusHandler.warn("Invalid check [" + checkName
                                + "] specified for PIL [" + pil + "].");
                    }
                }

                checksMap.put(pil, checks);
            }

            return checksMap;
        } else {
            statusHandler.error(
                    "Unable to locate QCProductChecks.properties for QC check.");
        }

        return Collections.emptyMap();
    }

    /**
     *
     * @param header
     * @param body
     * @param nnn
     * @return true, if it passes the QC check
     */
    public QcCheckResponse checkWarningInfo(String header, String body,
            String nnn) {
        Collection<IQCCheck> checks = productQcChecks.getOrDefault(nnn,
                productQcChecks.get("default"));
        if (CollectionUtil.isNullOrEmpty(checks)) {
            return QcCheckResponse.DEFAULT_RESPONSE;
        }

        for (IQCCheck check : checks) {
            String errorMsg = check.runQC(header, body, nnn);
            if (!errorMsg.isEmpty()) {
                errorMsg += "\nPlease fix this problem so complete\n check of text can be performed.\n";
                return new QcCheckResponse(false, errorMsg);
            }
        }

        return QcCheckResponse.DEFAULT_RESPONSE;
    }

    public Optional<String> getProductWarningType(String nnn) {
        return (productTypeMap.containsKey(nnn))
                ? Optional.of(productTypeMap.get(nnn))
                : Optional.empty();
    }

    public boolean match(String nnn, String phensig) {
        String mappedNnn = (segmentedNNN.contains(nnn))
                ? followupNNN.get(phensig) : nnnOfIdent.get(phensig);
        return ((mappedNnn != null) && (mappedNnn.equals(nnn)));
    }

    public Collection<String> getImmediateCauses() {
        return Collections.unmodifiableCollection(immediateCause);
    }

    public Map<String, String> getCountyTypeMap() {
        return Collections.unmodifiableMap(countyTypes);
    }

    public Collection<String> getSegmentedNNN() {
        return Collections.unmodifiableCollection(segmentedNNN);
    }

    public Map<String, List<String>> getBulletTypeMap() {
        return Collections.unmodifiableMap(bulletTypeMap);
    }
}
