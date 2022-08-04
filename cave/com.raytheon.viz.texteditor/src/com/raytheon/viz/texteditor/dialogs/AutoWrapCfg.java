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
package com.raytheon.viz.texteditor.dialogs;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Auto Wrap Configuration
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 09, 2011           rferrel   Initial creation
 * May 01, 2019  7831     randerso  Added support for nonWrapPils. Significant
 *                                  refactor to move configuration loading and
 *                                  validation out of TextEditorDialog and make
 *                                  AutoWrapCfg.xml an incremental override.
 *
 * </pre>
 *
 * @author rferrel
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class AutoWrapCfg {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AutoWrapCfg.class);

    /**
     * Auto wrap start range column..
     */
    private static final int DEFAULT_START = 69;

    /**
     * Auto wrap end range column.
     */
    private static final int DEFAULT_END = 80;

    @XmlElement(name = "RangeStart")
    private Integer rangeStart;

    @XmlElement(name = "RangeEnd")
    private Integer rangeEnd;

    @XmlElement(name = "WrapButtonCfg")
    private List<WrapButtonCfg> buttons;

    @XmlElement(name = "NonWrapPil")
    private Set<String> nonWrapPils = new HashSet<>();

    @XmlElement(name = "WrapPil")
    private Set<String> wrapPils = new HashSet<>();

    /**
     * Get the auto wrap menu's configuration and perform sanity checks.
     *
     * @return autoWrapCfg
     */
    public static AutoWrapCfg loadAutoWrapCfg() {
        AutoWrapCfg autoWrapCfg = new AutoWrapCfg();
        autoWrapCfg.setRangeStart(DEFAULT_START);
        autoWrapCfg.setRangeEnd(DEFAULT_END);

        IPathManager pm = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> configMap = pm
                .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                        "textws/gui/AutoWrapCfg.xml");

        for (LocalizationLevel level : configMap.keySet().stream().sorted()
                .collect(Collectors.toList())) {
            ILocalizationFile lf = configMap.get(level);
            try (InputStream in = lf.openInputStream()) {
                AutoWrapCfg cfg = JAXB.unmarshal(in, AutoWrapCfg.class);

                autoWrapCfg.merge(cfg);
            } catch (LocalizationException | IOException e) {
                statusHandler.handle(Priority.ERROR, "Error reading " + lf, e);
            }
        }

        // Perform Sanity Checks on configuration.
        StringBuilder message = new StringBuilder();
        if (autoWrapCfg.getRangeStart() <= 0) {
            message.append("Bad value for RangeStart (")
                    .append(autoWrapCfg.getRangeStart())
                    .append(") using default: ").append(DEFAULT_START)
                    .append("\n");
            autoWrapCfg.setRangeStart(DEFAULT_START);
        }

        if (autoWrapCfg.getRangeEnd() <= 0) {
            message.append("Bad value for RangeEnd (")
                    .append(autoWrapCfg.getRangeEnd())
                    .append(") using default: ").append(DEFAULT_END)
                    .append("\n");
            autoWrapCfg.setRangeEnd(DEFAULT_END);
        }

        if (autoWrapCfg.getRangeEnd() < autoWrapCfg.getRangeStart()) {
            message.append("RangeEnd (").append(autoWrapCfg.getRangeEnd())
                    .append(") less then RangeStart (")
                    .append(autoWrapCfg.getRangeStart())
                    .append(") swapping values\n");
            int tmp = autoWrapCfg.getRangeEnd();
            autoWrapCfg.setRangeEnd(autoWrapCfg.getRangeStart());
            autoWrapCfg.setRangeStart(tmp);
        }

        int rangeStart = autoWrapCfg.rangeStart;
        int rangeEnd = autoWrapCfg.rangeEnd;

        // Check buttonCfg values.
        Map<Integer, WrapButtonCfg> wrapCols = new HashMap<>();
        Iterator<WrapButtonCfg> iter = autoWrapCfg.getButtons().iterator();
        while (iter.hasNext()) {
            WrapButtonCfg buttonCfg = iter.next();

            // check for wrapCol < rangeStart
            if (buttonCfg.getWrapCol() < rangeStart) {
                message.append("Item \"").append(buttonCfg.getLabelName())
                        .append("\" WrapCol (").append(buttonCfg.getWrapCol())
                        .append(") < RangeStart, ");

                if (wrapCols.containsKey(rangeStart)) {
                    message.append("discarding.\n");
                    iter.remove();
                    continue;
                } else {
                    message.append("changing to ").append(rangeStart)
                            .append("\n");
                    buttonCfg.setWrapCol(rangeStart);
                }
            }

            // check for wrapCol > rangeEnd
            if (buttonCfg.getWrapCol() > rangeEnd) {
                message.append("Item \"").append(buttonCfg.getLabelName())
                        .append("\" WrapCol (").append(buttonCfg.getWrapCol())
                        .append(") > RangeEnd, ");

                if (wrapCols.containsKey(rangeEnd)) {
                    message.append("discarding.\n");
                    iter.remove();
                    continue;
                } else {
                    message.append("changing to ").append(rangeEnd)
                            .append("\n");
                    buttonCfg.setWrapCol(rangeEnd);
                }
            }

            WrapButtonCfg duplicate = wrapCols.get(buttonCfg.getWrapCol());
            if (duplicate == null) {
                wrapCols.put(buttonCfg.getWrapCol(), buttonCfg);
            } else {
                message.append("Item \"").append(buttonCfg.getLabelName())
                        .append(" is a duplicate of ")
                        .append(duplicate.getLabelName())
                        .append(", discarding\n");
                iter.remove();
                continue;
            }
        }

        Collections.sort(autoWrapCfg.getButtons(),
                Comparator.comparingInt(WrapButtonCfg::getWrapCol));

        int selectionCnt = 0;
        String selectionLabel = null;
        for (WrapButtonCfg buttonCfg : autoWrapCfg.getButtons()) {
            if (buttonCfg.isSelected()) {
                ++selectionCnt;
                if (selectionCnt == 1) {
                    selectionLabel = buttonCfg.getLabelName();
                } else {
                    buttonCfg.setSelected(false);
                }
            }
        }

        if (selectionCnt == 0 && !autoWrapCfg.getButtons().isEmpty()) {
            WrapButtonCfg buttonCfg = autoWrapCfg.getButtons().get(0);
            message.append("No button selected. Selecting top item \"")
                    .append(buttonCfg.getLabelName()).append("\"\n");
            buttonCfg.setSelected(true);
        } else if (selectionCnt > 1) {
            message.append(selectionCnt)
                    .append(" items selected; will select item \"")
                    .append(selectionLabel).append("\"\n");
        }

        if (message.length() > 0) {
            message.insert(0,
                    "Errors in AutoWrapCfg.xml. Expand to see details\n");
            IUFStatusHandler statusHandler = UFStatus
                    .getHandler(TextEditorDialog.class);
            statusHandler.error(message.toString());
        }
        return autoWrapCfg;
    }

    /**
     * Private constructor, use {@link #loadAutoWrapCfg()}
     */
    private AutoWrapCfg() {
    }

    /**
     * @return the rangeStart
     */
    public int getRangeStart() {
        return rangeStart;
    }

    /**
     * @param rangeStart
     *            the rangeStart to set
     */
    public void setRangeStart(int rangeStart) {
        this.rangeStart = rangeStart;
    }

    /**
     * @return the rangeEnd
     */
    public int getRangeEnd() {
        return rangeEnd;
    }

    /**
     * @param rangeEnd
     *            the rangeEnd to set
     */
    public void setRangeEnd(int rangeEnd) {
        this.rangeEnd = rangeEnd;
    }

    /**
     * @return the buttons
     */
    public List<WrapButtonCfg> getButtons() {
        return buttons;
    }

    /**
     * @param buttons
     *            the buttons to set
     */
    public void setButtons(List<WrapButtonCfg> buttons) {
        this.buttons = buttons;
    }

    /**
     * @return the nonWrapPils
     */
    public Set<String> getNonWrapPils() {
        return nonWrapPils;
    }

    /**
     * @param nonWrapPils
     *            the nonWrapPils to set
     */
    public void setNonWrapPils(Set<String> nonWrapPils) {
        this.nonWrapPils = nonWrapPils;
    }

    private void merge(AutoWrapCfg cfg) {
        if (cfg.rangeStart != null) {
            rangeStart = cfg.rangeStart;
        }

        if (cfg.rangeEnd != null) {
            rangeEnd = cfg.rangeEnd;
        }

        if (cfg.buttons != null) {
            buttons = cfg.buttons;
        }

        nonWrapPils.addAll(cfg.nonWrapPils);
        nonWrapPils.removeAll(cfg.wrapPils);
    }
}
