/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2011            ekladstrup     Initial creation
 *
 */
package com.raytheon.uf.edex.ogc.common.colormap;

import java.io.InputStream;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.ogc.common.spatial.AltUtil;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalEnabled;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalSpatialFactory;

/**
 * TODO Add Description
 * 
 * @author ekladstrup
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class StyleRuleLibrary {

	@XmlElements({ @XmlElement(name = "styleRule", type = StyleRule.class) })
	private List<StyleRule> rules;

	private Map<String, List<StyleRule>> _ruleMap;

	public StyleRuleLibrary() {

	}

	public static StyleRuleLibrary load(InputStream inputStream)
			throws JAXBException {
		JAXBContext context = JAXBContext.newInstance(StyleRuleLibrary.class);
		Unmarshaller unmarshaller = context.createUnmarshaller();
		Object lib = unmarshaller.unmarshal(inputStream);
		if (lib instanceof StyleRuleLibrary) {
			return (StyleRuleLibrary) lib;
		} else {
			return null;
		}

	}

	public void setRules(List<StyleRule> rules) {
		this.rules = rules;
	}

	public List<StyleRule> getRules() {
		return rules;
	}

	protected Map<String, List<StyleRule>> getRuleMap() {
		if (_ruleMap == null && rules != null) {
			_ruleMap = new HashMap<String, List<StyleRule>>();
			for (StyleRule rule : rules) {
				String cmap = rule.getColorMapName();
				List<StyleRule> list = _ruleMap.get(cmap);
				if (list == null) {
					list = new ArrayList<StyleRule>();
					_ruleMap.put(cmap, list);
				}
				list.add(rule);
			}
		}
		return _ruleMap;
	}

	    /**
     * @param rule
     * @param record
     * @return false if there was a range and it was out of bounds, true
     *         otherwise
     * @throws ParseException
     */
    protected boolean initRule(StyleRule rule, PluginDataObject record)
            throws ParseException {
		LevelRange range = rule.getLevelRange();
		if (range != null) {
			// only accept if range is ok
            return initRange(rule, record);
		}
		// if there is no range, we accept the rule
		return true;
	}

	    /**
     * @param rule
     * @param record
     * @return true if range is in bounds
     * @throws ParseException
     */
    @SuppressWarnings("unchecked")
    protected boolean initRange(StyleRule rule, PluginDataObject record)
            throws ParseException {
		LevelRange range = rule.getLevelRange();
        VerticalEnabled<PluginDataObject> enabled;
        if (record instanceof VerticalEnabled<?>) {
            enabled = (VerticalEnabled<PluginDataObject>) record;
        } else if ((enabled = (VerticalEnabled<PluginDataObject>) VerticalSpatialFactory
                .getEnabled(record.getClass())) != null) {
        } else {
            return false;
        }
        VerticalCoordinate vert = enabled.getVerticalCoordinate(record);

        if (vert != null) {
            Unit<?> styleLevelUnit = Unit.valueOf(range.getUnit());
            double levelValue = AltUtil.convert(styleLevelUnit,
                    Reference.UNKNOWN, vert).getValue();
            if (levelValue >= range.getLower()
                    && levelValue <= range.getUpper()) {
                setupLevelRange(rule, levelValue);
				// range is in bounds
				return true;
			}
		}
		// range is out of bounds
		return false;
	}

    public StyleRule getMatchForLayer(PluginDataObject record)
			throws ParseException {
        if (record == null) {
			return null;
		}
        String datauri = record.getDataURI();
		for (StyleRule rule : rules) {
			String pattern = rule.getLayerRegex();
            if (datauri.matches(pattern)) {
				// do not return rule if there is an invalid range
                if (initRule(rule, record)) {
					return rule;
				}
			}
		}
		return null;
	}

    public StyleRule getMatchForLayer(String layerName) throws ParseException {
        if (layerName == null) {
            return null;
        }
        for (StyleRule rule : rules) {
            String pattern = rule.getLayerRegex();
            if (layerName.matches(pattern)) {
                return rule;
            }
        }
        return null;
    }

    public StyleRule getLayerStyleWithNewCmap(PluginDataObject record,
            String newColormap) throws ParseException {
        StyleRule rule = getMatchForLayer(record);
        if (rule != null && newColormap != null) {
            rule.setColorMapName(newColormap);
            return rule;
        }
        return null;
    }

	private void setupLevelRange(StyleRule rule, Double lvlVal) {
		MapRange range = rule.getMapRange();
		LevelRange lRange = rule.getLevelRange();
		if (range != null && lRange != null) {
			if (range.getLowerMaximum() != null
					&& range.getLowerMinimum() != null) {
				// calculate upper and lower
				double maxRange = Math.abs(range.getUpperMaximum()
						- range.getLowerMaximum());
				double minRange = Math.abs(range.getUpperMinimum()
						- range.getLowerMinimum());
				double levelRange = Math.abs(lRange.getUpper()
						- lRange.getLower());

				double lvlRatioOffset = (lvlVal - lRange.getLower())
						/ levelRange;

				Double newMax = (maxRange * lvlRatioOffset)
						+ range.getLowerMaximum();
				Double newMin = (minRange * lvlRatioOffset)
						+ range.getLowerMinimum();

				range.setUpperMaximum(newMax.floatValue());
				range.setUpperMinimum(newMin.floatValue());
			}
		}
	}
}
