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
package com.raytheon.uf.common.dataplugin.gfe.discrete;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

//import org.apache.log4j.PropertyConfigurator;

public class DiscreteKeyTest {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteKeyTest.class);

    static DiscreteDefinition _dxDefinition = new DiscreteDefinition();

    public static void main(String[] args) {
        String siteId = "XXX";

        // make a DiscreteKey
        List<DiscreteKeyDef> dkd = new ArrayList<DiscreteKeyDef>();
        dkd.add(new DiscreteKeyDef("<NoData>", "No Data"));
        dkd.add(new DiscreteKeyDef("A", "Alpha"));
        dkd.add(new DiscreteKeyDef("B", "Beta"));
        dkd.add(new DiscreteKeyDef("C", "Charlie"));
        dkd.add(new DiscreteKeyDef("D", "Delta"));
        _dxDefinition.addDefinition("test_SFC", false, 0, dkd);
        _dxDefinition.addDefinition("testoverlap_SFC", true, 0, dkd);

        System.out.println("Discrete Definition tests");

        // test out discrete definition
        List<String> available = _dxDefinition.availableDefs();
        System.out.println("available defs: " + available);
        for (int i = 0; i < available.size(); i++) {
            System.out.println("parmNameAndLevel: " + available.get(i));
            System.out.println("overlaps: "
                    + _dxDefinition.overlaps(available.get(i)));
            List<DiscreteKeyDef> keys = _dxDefinition.keys(available.get(i));
            System.out.println("keys: " + keys);
            for (int j = 0; j < keys.size(); j++) {
                int keyIndex = _dxDefinition.keyIndex(available.get(i), keys
                        .get(j).getSymbol());
                System.out.println("index: " + keyIndex);
                String sym = _dxDefinition
                        .keySymbol(available.get(i), keyIndex);
                String des = _dxDefinition.keyDesc(available.get(i), sym);
                System.out.println("sym: " + sym + "   des: " + des);
            }

        }
        System.out.println("PRINTON: " + _dxDefinition);

        DiscreteKey.setDiscreteDefinition(siteId, _dxDefinition);

        if (!DiscreteKey.discreteDefinition(siteId).equals(_dxDefinition)) {
            System.out.println("ERROR: definitions vary");
        } else {
            System.out.println("Equality test: OK: definitions the same");
        }

        ParmID pid = new ParmID("test_SFC:BOU_GRID__Fcst_00000000_0000");
        ParmID pidOut = new ParmID(
                "testoverlap_SFC:BOU_GRID__Fcst_00000000_0000");

        System.out.println();
        System.out.println("Constructor tests");

        DiscreteKey k = new DiscreteKey();
        System.out.println("Default: " + k);
        DiscreteKey k1 = new DiscreteKey(siteId, "B", pid);
        System.out.println("single, non-overlap:(B) " + k1);
        DiscreteKey k2 = new DiscreteKey(siteId, "B^A", pid);
        System.out.println("dual, no overlap:(B^A) " + k2);
        DiscreteKey k3 = new DiscreteKey(siteId, "B^A", pidOut);
        System.out.println("dual, overlap:(B^A) " + k3);
        List<String> subkeys = new ArrayList<String>();
        subkeys.add("C");
        subkeys.add("A");
        subkeys.add("B");
        DiscreteKey k4 = new DiscreteKey(siteId, subkeys, pid);
        System.out.println("multiple non-overlap:(C^A^B) " + k4);
        DiscreteKey k5 = new DiscreteKey(siteId, subkeys, pidOut);
        System.out.println("multiple overlap:(C^A^B) " + k5);
        DiscreteKey k6 = new DiscreteKey(siteId, "A^A^A^A^A", pid);
        System.out.println("multiple non-overlap:(A^A^A^A^A) " + k6);
        DiscreteKey k7 = new DiscreteKey(siteId, "A^A^A^A^A", pidOut);
        System.out.println("multiple overlap:(A^A^A^A^A) " + k7);

        System.out.println();
        System.out.println("Comparison operator tests");
        System.out.println("Values should all be true");

        DiscreteKey key1, key2;
        key1 = new DiscreteKey(siteId, "A^B", pidOut);
        key2 = new DiscreteKey(siteId, "B^A", pidOut);
        System.out.println(key1 + " == " + key2 + ": " + (key1.equals(key2)));

        key1 = new DiscreteKey(siteId, "A^C", pidOut);
        key2 = new DiscreteKey(siteId, "B^A", pidOut);
        System.out.println(key1 + " != " + key2 + ": " + (!key1.equals(key2)));

        key1 = new DiscreteKey(siteId, "B^C", pidOut);
        key2 = new DiscreteKey(siteId, "B^A", pidOut);
        System.out.println(key1 + " <= " + key2 + ": "
                + (key1.compareTo(key2) <= 0));

        key1 = new DiscreteKey(siteId, "B^C", pidOut);
        key2 = new DiscreteKey(siteId, "C", pidOut);
        System.out.println(key1 + " >= " + key2 + ": "
                + (key1.compareTo(key2) >= 0));

        key1 = new DiscreteKey(siteId, "B", pidOut);
        key2 = new DiscreteKey(siteId, "A", pidOut);
        System.out.println(key1 + " < " + key2 + ": "
                + (key1.compareTo(key2) < 0));

        key1 = new DiscreteKey(siteId, "A", pidOut);
        key2 = new DiscreteKey(siteId, "C", pidOut);
        System.out.println(key1 + " > " + key2 + ": "
                + (key1.compareTo(key2) > 0));

        System.out.println();

        DiscreteKey k8 = DiscreteKey.combine(k3, k5);
        System.out.println("k8=k3+k5 " + k8 + ' ' + k3 + ' ' + k5);
        try {
            DiscreteKey k9 = DiscreteKey.combine(k1, k5);
            System.out.println("k9=k1+k5 " + k9 + ' ' + k1 + ' ' + k5);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        k3.addAll(k5);
        System.out.println("k3+=k5 " + k3 + ' ' + k5);

        try {
            k1.addAll(k5);
            System.out.println("k1+=k5 " + k1 + ' ' + k5);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        System.out.println("key as string: " + k5.toString() + ' ' + k5);
        System.out.println("k5valid: " + k5.isValid());
        System.out.println("k3valid: " + k3.isValid());
        System.out.println("k8valid: " + k8.isValid());

        System.out.println("descriptionSubKeys(A,C): "
                + k5.descriptionSubKeys("[A,C]"));
        ;
        System.out.println("descriptionSubKeys(*): "
                + k5.descriptionSubKeys("*"));

    }
}
