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
package com.raytheon.uf.common.dataplugin.gfe.weather;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 15, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WeatherKeyTest {
    private static WxDefinition wxDefinition;

    public static void main(String[] args) {
        // make a Weather Definition
        List<WeatherCoverage> covs = new ArrayList<WeatherCoverage>();
        covs.add(new WeatherCoverage("<NoCov>", "nocov"));
        covs.add(new WeatherCoverage("Iso", "Isolated"));
        covs.add(new WeatherCoverage("WSct", "Widely Scattered"));
        covs.add(new WeatherCoverage("Sct", "Scattered"));
        covs.add(new WeatherCoverage("Chc", "Chance"));

        List<WeatherIntensity> intens = new ArrayList<WeatherIntensity>();
        intens.add(new WeatherIntensity("<NoInten>", "No Intensity"));
        intens.add(new WeatherIntensity("--", "Very Light"));
        intens.add(new WeatherIntensity("-", "Light"));
        intens.add(new WeatherIntensity("+", "Heavy"));

        List<WeatherAttribute> attrs = new ArrayList<WeatherAttribute>();
        attrs.add(new WeatherAttribute("InMount", "In the Mountains"));
        attrs.add(new WeatherAttribute("NearFoot", "Near the Foothills"));
        attrs.add(new WeatherAttribute("HvyDP", "Heavy Downpours"));
        attrs.add(new WeatherAttribute("FL", "Frequent Lightning"));

        WeatherType wt0 = new WeatherType("<NoWx>", "NoWeather",
                Arrays.asList(new WeatherCoverage("<NoCov>", "none")),
                Arrays.asList(new WeatherIntensity("<NoInten>", "nointen")),
                new ArrayList<WeatherAttribute>());
        WeatherType wt5 = new WeatherType("F", "Fog", covs, intens, attrs);
        WeatherType wt1 = new WeatherType("SW", "Snow Showers", covs, intens,
                attrs);
        WeatherType wt2 = new WeatherType("L", "Drizzle", covs, intens, attrs);
        WeatherType wt3 = new WeatherType("R", "Rain", covs, intens, attrs);
        WeatherType wt4 = new WeatherType("T", "Thunder", covs, intens, attrs);

        List<WeatherType> wts = new ArrayList<WeatherType>();
        wts.add(wt0);
        wts.add(wt1);
        wts.add(wt2);
        wts.add(wt3);
        wts.add(wt4);
        wts.add(wt5);

        List<WeatherVisibility> wvs = new ArrayList<WeatherVisibility>();
        wvs.add(new WeatherVisibility("<NoVis>"));
        wvs.add(new WeatherVisibility("0SM"));
        wvs.add(new WeatherVisibility("1SM"));
        wvs.add(new WeatherVisibility("2SM"));
        wvs.add(new WeatherVisibility("2 1/2SM"));
        wvs.add(new WeatherVisibility("3SM"));
        wvs.add(new WeatherVisibility("6SM"));

        wxDefinition = new WxDefinition(wts, wvs);

        String siteId = "XXX";
        WeatherSubKey.setWxDefinition(siteId, wxDefinition);

        WeatherKey nullKey = new WeatherKey(siteId,
                "<NoCov>:<NoWx>:<NoInten>:<NoVis>:");
        System.out.println("NULLKEY: " + nullKey);

        // Test #1
        // Get the available types
        System.out.println("Available types: "
                + WeatherSubKey.availableWxTypes(siteId));
        System.out.println();

        // Test #2
        // For a weather type Rain, get the available coverages, intensities,
        // visibilities, and attributes
        String wxType = "R";
        System.out.println("Available coverages for Rain: "
                + WeatherSubKey.availableCoverages(siteId, wxType));
        System.out.println("Available intensities for Rain: "
                + WeatherSubKey.availableIntensities(siteId, wxType));
        System.out.println("Available visibilities for Rain: "
                + WeatherSubKey.availableVisibilities(siteId));
        System.out.println("Available attributes for Rain: "
                + WeatherSubKey.availableAttributes(siteId, wxType));
        System.out.println();

        // Test #3
        // make a WeatherSubKey using the constructor using strings for the
        // coverage, intensity, type, visibility, and attributes. Test for
        // validity.
        String coverage = "Iso";
        String type = "R";
        String intensity = "--";
        String visibility = "2 1/2SM";
        List<String> attributes = new ArrayList<String>();
        attributes.add("InMount");

        WeatherSubKey wsk = new WeatherSubKey(siteId, coverage, type,
                intensity, visibility, attributes);
        System.out.println(" wsk as int " + wsk.subKeyAsInt());
        if (wsk.isValid()) {
            System.out.println("Test #3 WeatherSubKey: " + wsk.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #3 WeatherSubKey: " + wsk.getType()
                    + " is NOT valid.");
        }
        System.out.println();

        // Test #4
        // make a WeatherSubKey using the constructor using strings for the
        // coverage, intensity, type, visibility, and attributes.
        // Make coverage invalid. Test for validity.
        String coverageBad = "Isolatedd";

        wsk = new WeatherSubKey(siteId, coverageBad, type, intensity,
                visibility, attributes);
        System.out.println("Test #4: SubKey as Int " + wsk.subKeyAsInt());
        if (wsk.isValid()) {
            System.out.println("Test #4 WeatherSubKey: " + wsk.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #4 WeatherSubKey: " + wsk.getType()
                    + " is NOT valid.");
        }
        System.out.println();

        // Test #5
        // make a WeatherSubKey using the constructor using strings for the
        // coverage, intensity, type, visibility, and attributes. Make intensity
        // invalid. Test for validity.
        String intensityBad = "VVery Light";

        wsk = new WeatherSubKey(siteId, coverage, type, intensityBad,
                visibility, attributes);
        if (wsk.isValid()) {
            System.out.println("Test #5 WeatherSubKey: " + wsk.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #5 WeatherSubKey: " + wsk.getType()
                    + " is NOT valid.");
        }
        System.out.println();

        // Test #6
        // make a WeatherSubKey using the constructor using strings for the
        // coverage, intensity, type, visibility, and attributes. Make
        // visibility
        // invalid. Test for validity.
        String visibilityBad = "100SM";

        wsk = new WeatherSubKey(siteId, coverage, type, intensity,
                visibilityBad, attributes);
        if (wsk.isValid()) {
            System.out.println("Test #6 WeatherSubKey: " + wsk.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #6 WeatherSubKey: " + wsk.getType()
                    + " is NOT valid.");
        }
        System.out.println();

        // Test #7
        // make a WeatherSubKey using the constructor using strings for the
        // coverage, intensity, type, visibility, and attributes. Make attribute
        // invalid. Test for validity.
        String attributeBad = "In the house";

        wsk = new WeatherSubKey(siteId, coverage, type, intensity, visibility,
                Arrays.asList(attributeBad));
        if (wsk.isValid()) {
            System.out.println("Test #7 WeatherSubKey: " + wsk.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #7 WeatherSubKey: " + wsk.getType()
                    + " is NOT valid.");
        }
        System.out.println();

        // Test #8
        // make a WeatherSubKey using the constructor using strings for the
        // coverage, intensity, type, visibility, and attributes. Make type
        // invalid. Test for validity.
        String typeBad = "BigTimeRain";

        wsk = new WeatherSubKey(siteId, coverage, typeBad, intensity,
                visibility, attributes);
        if (wsk.isValid()) {
            System.out.println("Test #8 WeatherSubKey: " + wsk.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #8 WeatherSubKey: " + wsk.getType()
                    + " is NOT valid.");
        }
        System.out.println();

        // Test #9
        // make two WeatherSubKeys that should not combine

        // Rain
        String coverage1 = "Iso";
        String type1 = "R";
        String intensity1 = "--";
        String visibility1 = "2 1/2SM";
        List<String> attributes1 = Arrays.asList("InMount");

        WeatherSubKey wsk1 = new WeatherSubKey(siteId, coverage1, type1,
                intensity1, visibility1, attributes1);
        if (wsk1.isValid()) {
            System.out.println("Test #9 WeatherSubKey: " + wsk1.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #9 WeatherSubKey: " + wsk1.getType()
                    + " is NOT valid.");
        }

        // Snow showers
        String coverage2 = "Iso";
        String type2 = "SW";
        String intensity2 = "--";
        String visibility2 = "2 1/2SM";
        List<String> attributes2 = Arrays.asList("InMount");

        WeatherSubKey wsk2 = new WeatherSubKey(siteId, coverage2, type2,
                intensity2, visibility2, attributes2);
        if (wsk2.isValid()) {
            System.out.println("Test #9 WeatherSubKey: " + wsk2.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #9 WeatherSubKey: " + wsk2.getType()
                    + " is NOT valid.");
        }
        System.out.println(" wsk2 as int " + wsk2.subKeyAsInt());

        List<WeatherSubKey> wsks = new ArrayList<WeatherSubKey>();
        wsks.add(wsk1);
        wsks.add(wsk2);

        WeatherKey wk = new WeatherKey(siteId, wsks);
        System.out
                .println("WeatherKey with Rain and Snow Showers: (different types)");
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }
        if (wk.getSubKeys().size() == 2) {
            System.out
                    .println("Test #9 successful - Rain and Snow Showers did not combine.");
        } else {
            System.out
                    .println("Test #9 NOT successful - Rain and Snow Showers should not "
                            + "combine.");
        }
        System.out.println();

        // Test #10
        // make another Rain WeatherSubKey and try to combine with Rain from
        // Test #9.

        // Rain
        String coverage3 = "Iso";
        String type3 = "R";
        String intensity3 = "--";
        String visibility3 = "2 1/2SM";
        List<String> attributes3 = new ArrayList<String>();
        attributes3.add("InMount");

        WeatherSubKey wsk3 = new WeatherSubKey(siteId, coverage3, type3,
                intensity3, visibility3, attributes3);
        if (wsk3.isValid()) {
            System.out.println("Test #10 WeatherSubKey: " + wsk3.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #10 WeatherSubKey: " + wsk3.getType()
                    + " is NOT valid.");
        }

        wsks.clear();
        wsks.add(wsk1);
        wsks.add(wsk3);

        wk = new WeatherKey(siteId, wsks);
        System.out.println("WeatherKey with Rain and Rain: (all same)");
        System.out.println("WeatherSubKey: " + wsk1);
        System.out.println("WeatherSubKey: " + wsk3);
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.getSubKeys().size() == 1) {
            System.out
                    .println("Test #10 successful - Rain and Rain did combine.");
        } else {
            System.out
                    .println("Test #10 NOT successful - Rain and Rain should combine.");
        }
        System.out.println();

        // Test #11
        // Same as test #10, but coverage is different for the two SubKeys.
        coverage3 = "WSct";

        wsk3 = new WeatherSubKey(siteId, coverage3, type3, intensity3,
                visibility3, attributes3);

        if (wsk3.isValid()) {
            System.out.println("Test #11 WeatherSubKey: " + wsk3.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #11 WeatherSubKey: " + wsk3.getType()
                    + " is NOT valid.");
        }

        wsks.clear();
        wsks.add(wsk1);
        wsks.add(wsk3);

        wk = new WeatherKey(siteId, wsks);
        System.out
                .println("WeatherKey with Rain and Rain (different coverage):");
        System.out.println("WeatherSubKey: " + wsk1);
        System.out.println("WeatherSubKey: " + wsk3);
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.getSubKeys().size() == 1) {
            System.out
                    .println("Test #11 successful - Rain and Rain did combine.");
        } else {
            System.out
                    .println("Test #11 NOT successful - Rain and Rain should combine.");
        }
        System.out.println();

        // Test #12
        // make another Rain WeatherSubKey with different visibility

        // Rain
        String coverage4 = "Iso";
        String type4 = "R";
        String intensity4 = "--";
        String visibility4 = "2SM";
        List<String> attributes4 = new ArrayList<String>();
        attributes4.add("InMount");

        WeatherSubKey wsk4 = new WeatherSubKey(siteId, coverage4, type4,
                intensity4, visibility4, attributes4);
        if (wsk4.isValid()) {
            System.out.println("Test #12 WeatherSubKey: " + wsk4.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #12 WeatherSubKey: " + wsk4.getType()
                    + " is NOT valid.");
        }

        wsks.clear();
        wsks.add(wsk1);
        wsks.add(wsk4);

        wk = new WeatherKey(siteId, wsks);
        System.out
                .println("WeatherKey with Rain and Rain: (different visibility)");
        System.out.println("WeatherSubKey: " + wsk1);
        System.out.println("WeatherSubKey: " + wsk4);
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.getSubKeys().size() == 1) {
            System.out
                    .println("Test #12 successful - Rain and Rain did combine.");
        } else {
            System.out
                    .println("Test #12 NOT successful - Rain and Rain should combine.");
        }
        System.out.println();

        // Test #13
        // make another Rain WeatherSubKey with different intensity

        // Rain
        String coverage5 = "Iso";
        String type5 = "R";
        String intensity5 = "-";
        String visibility5 = "2 1/2SM";
        List<String> attributes5 = new ArrayList<String>();
        attributes5.add("InMount");

        WeatherSubKey wsk5 = new WeatherSubKey(siteId, coverage5, type5,
                intensity5, visibility5, attributes5);
        if (wsk5.isValid()) {
            System.out.println("Test #13 WeatherSubKey: " + wsk5.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #13 WeatherSubKey: " + wsk5.getType()
                    + " is NOT valid.");
        }

        wsks.clear();
        wsks.add(wsk1);
        wsks.add(wsk5);
        wsks.add(wsk5);

        wk = new WeatherKey(siteId, wsks);
        System.out
                .println("WeatherKey with Rain and Rain: (different intensity)");
        System.out.println("WeatherSubKey: " + wsk1);
        System.out.println("WeatherSubKey: " + wsk5);
        System.out.println("WeatherSubKey: " + wsk5);
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.getSubKeys().size() == 1) {
            System.out
                    .println("Test #13 successful - Rain and Rain did combine.");
        } else {
            System.out
                    .println("Test #13 NOT successful - Rain and Rain should combine.");
        }
        System.out.println();

        // Test #14
        // make another Rain WeatherSubKey with different attributes

        // Rain
        String coverage6 = "Iso";
        String type6 = "R";
        String intensity6 = "--";
        String visibility6 = "2 1/2SM";
        List<String> attributes6 = new ArrayList<String>();
        attributes6.add("NearFoot");

        WeatherSubKey wsk6 = new WeatherSubKey(siteId, coverage6, type6,
                intensity6, visibility6, attributes6);
        if (wsk6.isValid()) {
            System.out.println("Test #14 WeatherSubKey: " + wsk6.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #14 WeatherSubKey: " + wsk6.getType()
                    + " is NOT valid.");
        }

        wsks.clear();
        wsks.add(wsk1);
        wsks.add(wsk6);

        wk = new WeatherKey(siteId, wsks);
        System.out
                .println("WeatherKey with Rain and Rain: (different attributes)");
        System.out.println("WeatherSubKey: " + wsk1);
        System.out.println("WeatherSubKey: " + wsk6);
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.getSubKeys().size() == 1) {
            System.out
                    .println("Test #14 successful - Rain and Rain did combine.");
        } else {
            System.out
                    .println("Test #14 NOT successful - Rain and Rain should combine.");
        }
        System.out.println();

        // Test #15
        // make two Rain WeatherSubKey with different intensity and visibility

        // Rain
        String coverage7 = "Iso";
        String type7 = "R";
        String intensity7 = "-";
        String visibility7 = "2SM";
        List<String> attributes7 = new ArrayList<String>();
        attributes7.add("NearFoot");

        WeatherSubKey wsk7 = new WeatherSubKey(siteId, coverage7, type7,
                intensity7, visibility7, attributes7);
        if (wsk7.isValid()) {
            System.out.println("Test #15 WeatherSubKey: " + wsk7.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #15 WeatherSubKey: " + wsk7.getType()
                    + " is NOT valid.");
        }

        String coverage8 = "Iso";
        String type8 = "R";
        String intensity8 = "+";
        String visibility8 = "1SM";
        List<String> attributes8 = new ArrayList<String>();
        attributes8.add("NearFoot");

        WeatherSubKey wsk8 = new WeatherSubKey(siteId, coverage8, type8,
                intensity8, visibility8, attributes8);
        if (wsk8.isValid()) {
            System.out.println("Test #15 WeatherSubKey: " + wsk8.getType()
                    + " is valid.");
        } else {
            System.out.println("Test #15 WeatherSubKey: " + wsk8.getType()
                    + " is NOT valid.");
        }

        wsks.clear();
        wsks.add(wsk7);
        wsks.add(wsk8);

        wk = new WeatherKey(siteId, wsks);
        System.out
                .println("WeatherKey with Rain and Rain: (different intensity and "
                        + "visibility)");
        System.out.println("WeatherSubKey: " + wsk7);
        System.out.println("WeatherSubKey: " + wsk8);
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.getSubKeys().size() == 1) {
            System.out
                    .println("Test #15 successful - Rain and Rain did combine.");
        } else {
            System.out
                    .println("Test #15 NOT successful - Rain and Rain should combine.");
        }
        System.out.println();

        // Test #16
        // make a WeatherKey from a coded long unsigned int

        int code = 137445377;
        wk = new WeatherKey(siteId, code);
        WeatherKey wkCompare = new WeatherKey(siteId, Arrays.asList(wsk1));
        System.out.println("Test #16: WeatherKey (code 137445377)");
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.equals(wkCompare)) {
            System.out.println("Test #16 successful.");
        } else {
            System.out.println("Test #16 NOT successful.");
        }
        System.out.println();

        // Test #17
        // make another WeatherKey from a coded long unsigned int

        code = 135348225;
        wk = new WeatherKey(siteId, code);
        wkCompare = new WeatherKey(siteId, Arrays.asList(wsk2));
        System.out.println("Test #17: WeatherKey (code 135348225)");
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }

        if (wk.equals(wkCompare)) {
            System.out.println("Test #17 successful.");
        } else {
            System.out.println("Test #17 NOT successful.");
        }
        System.out.println();

        // Test #18
        // make another WeatherKey from a bad coded long unsigned int
        code = 999999;
        wk = new WeatherKey(siteId, code);

        System.out.println("Test #18: WeatherKey (code 999999)");
        for (int i = 0; i < wk.getSubKeys().size(); i++) {
            System.out.println("wk [" + i + "] " + wk.getSubKeys().get(i));
        }
        System.out.println();

        // Test #19
        // make a WeatherSubKey from a single string
        String str = "Iso:R:--:2 1/2SM:InMount";
        WeatherSubKey wsk19 = new WeatherSubKey(siteId, str);
        System.out.println("Test #19: WeatherSubKey from string");
        System.out.println("string: " + str + " " + wsk19);
        System.out.println();

        // Test #20
        // make a WeatherSubKey from a single (bad) string
        str = "Iso:Y:--:2 1/2SM:InMount";
        WeatherSubKey wsk20 = new WeatherSubKey(siteId, str);
        System.out.println("Test #20: WeatherSubKey from badstring");
        System.out.println("string: " + str + " " + wsk20);
        System.out.println();

        // Test #21
        // make a WeatherKey
        System.out.println("Test #21: getCompositeTypes()");
        str = "Sct:R:-:3SM:HvyDP";
        WeatherSubKey wsk21a = new WeatherSubKey(siteId, str);

        str = "Sct:SW:-:1SM:<NoAttr>";
        WeatherSubKey wsk21b = new WeatherSubKey(siteId, str);

        str = "Sct:T:<NoInten>:6SM:FL";
        WeatherSubKey wsk21c = new WeatherSubKey(siteId, str);

        str = "WSct:F:<NoInten>:2SM:<NoAttr>";
        WeatherSubKey wsk21d = new WeatherSubKey(siteId, str);

        wsks.clear();
        wsks.add(wsk21a);
        wsks.add(wsk21b);
        wsks.add(wsk21c);
        wsks.add(wsk21d);

        wk = new WeatherKey(siteId, wsks);

        List<WxComposite> wxComps;

        wxComps = wk.getCompositeTypes();
        System.out.println("Test #21: Composite Types");

        wk.printOn(System.out);
        System.out.println();
        System.out.println("composite types: " + wxComps);

        System.out.println();

        // Test #22
        // make a WeatherKey
        System.out
                .println("Test #22: WeatherKey constructed with one good and one bad key");
        str = "Sct:R:-:3SM:HvyDP^WSct:F:<NoInten>:22SM:<NoAttr>";
        System.out.println("input string " + str);

        wk = new WeatherKey(siteId, str);
        wk.printOn(System.out);
        System.out.println();
        System.out.println("pretty string " + wk.toPrettyString());
        System.out.println();

        // Test #23
        // make a WeatherKey
        System.out
                .println("Test #23: WeatherKey constructed with Chc:R:-:<NoVis>:");
        str = "Chc:R:-:<NoVis>:";

        System.out.println("input string " + str);

        wk = new WeatherKey(siteId, str);
        wk.printOn(System.out);
        System.out.println();
        System.out.println("pretty string " + wk.toPrettyString());
        System.out.println();

        // Test #24
        // make a WeatherKey
        System.out
                .println("Test #24: WeatherKey constructed with Chc:R:-:<NoVis>:<NoAttr>");
        str = "Chc:R:-:<NoVis>:<NoAttr>";

        System.out.println("input string " + str);

        wk = new WeatherKey(siteId, str);
        wk.printOn(System.out);
        System.out.println();
        System.out.println("pretty string " + wk.toPrettyString());
        System.out.println();

        // TEST #24
        // working with description strings
        System.out.println("Description String Test");
        String des = "* [SW,T] - * FL";
        List<WeatherSubKey> descKeys = WeatherKey.descriptionSubKeys(siteId,
                des);

        // TEST #25
        // working with long seq of's
        System.out.println("Long List");
        String key = "Chc:R:-:<NoVis>:<NoAttr>";
        List<WeatherSubKey> subkeys = new ArrayList<WeatherSubKey>();
        for (int i = 0; i < 17; i++) {
            subkeys.add(new WeatherSubKey(siteId, key));
        }
        WeatherKey wk1 = new WeatherKey(siteId, subkeys);
        System.out.println(wk1);

        return;
    }
}
