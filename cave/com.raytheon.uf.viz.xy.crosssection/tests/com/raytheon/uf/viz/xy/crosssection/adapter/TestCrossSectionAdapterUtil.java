/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy.crosssection.adapter;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.RETURNS_SMART_NULLS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Unit tests for {@link CrossSectionAdapterUtil}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestCrossSectionAdapterUtil {

    private final PersistablePluginDataObject ppdo = new PersistablePluginDataObject() {

        private static final long serialVersionUID = 1L;

        @Override
        public String getPluginName() {
            return "grid";
        }
    };

    private IConfigurationElement configWithoutConstraints = mock(
            IConfigurationElement.class, RETURNS_SMART_NULLS);

    private IConfigurationElement configWithConstraints;

    private IConfigurationElement ppdoConfig;

    @BeforeEach
    public void setupBeforeEach() throws Exception {
        ppdoConfig = mock(IConfigurationElement.class, RETURNS_SMART_NULLS);
        when(ppdoConfig.getAttribute("class")).thenReturn(
                "com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject");

        configWithConstraints = mock(IConfigurationElement.class);
        when(configWithConstraints.getChildren("constraint"))
                .thenReturn(new IConfigurationElement[] {
                        mock(IConfigurationElement.class) });
    }

    @Test
    public void testSelectConfig1() {
        // Empty list -> null
        IConfigurationElement cfg = CrossSectionAdapterUtil
                .selectConfig(List.of());

        assertNull(cfg);
    }

    @Test
    public void testSelectConfig2() {
        // Single config -> that config returned
        IConfigurationElement cfgOutput = CrossSectionAdapterUtil
                .selectConfig(List.of(configWithoutConstraints));

        assertSame(configWithoutConstraints, cfgOutput);
    }

    @Test
    public void testSelectConfig3() {
        // Multiple configs without constraints -> null returned
        IConfigurationElement cfgOutput = CrossSectionAdapterUtil.selectConfig(
                List.of(configWithoutConstraints, configWithoutConstraints));

        assertNull(cfgOutput);
    }

    @Test
    public void testSelectConfig4() {
        // Multiple configs with constraints -> null returned
        IConfigurationElement cfgOutput = CrossSectionAdapterUtil.selectConfig(
                List.of(configWithConstraints, configWithConstraints));

        assertNull(cfgOutput);
    }

    @Test
    public void testSelectConfig5() {
        /*
         * Config with constraints and config without -> return one with
         * constraints
         */
        IConfigurationElement cfgOutput = CrossSectionAdapterUtil.selectConfig(
                List.of(configWithConstraints, configWithoutConstraints));

        assertSame(configWithConstraints, cfgOutput);
    }

    @Test
    public void testSelectConfig6() {
        /*
         * Config without constraints and config with (reverse order from above
         * test) -> return one with constraints
         */
        IConfigurationElement cfgOutput = CrossSectionAdapterUtil.selectConfig(
                List.of(configWithoutConstraints, configWithConstraints));

        assertSame(configWithConstraints, cfgOutput);
    }

    @Test
    public void testAdapterConfigMatchesPdo1() throws VizException {
        // Class matches and no constraints -> match
        Map<String, Object> uriFields = Map.of("info.datasetId", "radar-koax");

        boolean matches = CrossSectionAdapterUtil
                .adapterConfigMatchesPdo(ppdoConfig, ppdo, uriFields);

        assertTrue(matches);
    }

    @Test
    public void testAdapterConfigMatchesPdo2() throws VizException {
        // Class matches and constraint matches -> match
        IConfigurationElement constraintElem = buildConstraintElem(
                "info.datasetId", "radar-koax");
        when(ppdoConfig.getChildren("constraint"))
                .thenReturn(new IConfigurationElement[] { constraintElem });
        Map<String, Object> uriFields = Map.of("info.datasetId", "radar-koax");

        boolean matches = CrossSectionAdapterUtil
                .adapterConfigMatchesPdo(ppdoConfig, ppdo, uriFields);

        assertTrue(matches);
    }

    @Test
    public void testAdapterConfigMatchesPdo3() throws VizException {
        // Class matches and regex constraint matches -> match
        IConfigurationElement constraintElem = buildConstraintElem(
                "info.datasetId", "radar-.*");
        when(ppdoConfig.getChildren("constraint"))
                .thenReturn(new IConfigurationElement[] { constraintElem });
        Map<String, Object> uriFields = Map.of("info.datasetId", "radar-koax");

        boolean matches = CrossSectionAdapterUtil
                .adapterConfigMatchesPdo(ppdoConfig, ppdo, uriFields);

        assertTrue(matches);
    }

    @Test
    public void testAdapterConfigMatchesPdo4() throws VizException {
        // Class matches and regex constraint does not match -> no match
        IConfigurationElement constraintElem = buildConstraintElem(
                "info.datasetId", "radar-.*");
        when(ppdoConfig.getChildren("constraint"))
                .thenReturn(new IConfigurationElement[] { constraintElem });

        Map<String, Object> uriFields = Map.of("info.datasetId", "HRRR");

        boolean matches = CrossSectionAdapterUtil
                .adapterConfigMatchesPdo(ppdoConfig, ppdo, uriFields);

        assertFalse(matches);
    }

    protected IConfigurationElement buildConstraintElem(String key,
            String value) {
        IConfigurationElement cfg = mock(IConfigurationElement.class);
        when(cfg.getAttribute("key")).thenReturn(key);
        when(cfg.getAttribute("value")).thenReturn(value);
        return cfg;
    }
}
